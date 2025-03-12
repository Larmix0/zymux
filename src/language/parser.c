#include "dynamic_array.h"
#include "parser.h"
#include "report_error.h"

#if DEBUG_AST
    #include "debug_ast.h"
#endif

#define PEEK(parser) (*(parser)->current)
#define PEEK_PREVIOUS(parser) (*((parser)->current - 1))
#define CHECK(parser, expected) (PEEK(parser).type == (expected))

#define ADVANCE_PEEK(parser) (*(parser)->current++)
#define ADVANCE(parser) ((parser)->current++)
#define RETREAT(parser) ((parser)->current--)

#define MATCH(parser, expected) (CHECK(parser, expected) ? (ADVANCE(parser), true) : false)
#define CONSUME(parser, expected, ...) \
    (CHECK(parser, expected) \
        ? ADVANCE_PEEK(parser) \
        : (parser_error_missing(parser, &PEEK_PREVIOUS(parser), true, __VA_ARGS__), PEEK(parser)))
            
#define IS_EOF(parser) (CHECK(parser, TOKEN_EOF))

#define NULL_NODE(parser) (new_keyword_node(parser->program, create_token("null", TOKEN_NULL_KW)))

static Node *assignment(Parser *parser);
static Node *expression(Parser *parser);
static Node *declaration(Parser *parser);

/** Returns an initialized parser. */
Parser create_parser(ZmxProgram *program, TokenArray tokens) {
    Parser parser = {
        .isPanicking = false, .ast = CREATE_DA(), .program = program,
        .tokens = tokens, .current = tokens.data, .syncSpot = NULL
    };
    return parser;
}

/** Frees all the memory the passed parser owns. */
void free_parser(Parser *parser) {
    FREE_DA(&parser->ast);
    free_all_nodes(parser->program);
}

/** Reports a parsing error on a specific, erroneous token, then starts panicking. */
static void parser_error_at(
    Parser *parser, Token *erroredToken, const bool shouldPanic, const char *format, ...
) {
    if (parser->isPanicking) {
        return;
    }

    if (shouldPanic) {
        parser->isPanicking = true;
        parser->syncSpot = erroredToken;
    }
    va_list args;
    va_start(args, format);
    zmx_user_error(parser->program, erroredToken->pos, "Syntax error", format, &args);
    va_end(args);
}

/**
 * Reports a missing token parsing error on the token before the missing one, then starts panicking.
 * 
 * This is reserved for parsing errors that occur due to the parser expecting a token,
 * but a different one being there instead. Like CONSUME() failing.
 * 
 * It errors the missing character by showing
 * the character after the token located before the missing one (which is what beforeMissing is).
 */
static void parser_error_missing(
    Parser *parser, Token *beforeMissing, const bool shouldPanic, const char *format, ...
) {
    if (parser->isPanicking) {
        return;
    }

    if (shouldPanic) {
        parser->isPanicking = true;
        parser->syncSpot = beforeMissing + 1;
    }
    // Errors out the character after the token located before the missing one.
    const SourcePosition errorPos = create_src_pos(
        beforeMissing->pos.line, beforeMissing->pos.column + beforeMissing->pos.length, 1
    );
    va_list args;
    va_start(args, format);
    zmx_user_error(parser->program, errorPos, "Syntax error", format, &args);
    va_end(args);
}

/**
 * Parse out a full string.
 * 
 * This is done by alternating between parsing a string literal and an interpolated expression
 * until the STRING_END token is seen.
 * 
 * We start with a gauranteed string literal from the lexer. Even if the first thing on the string
 * is interpolation, we at least have an empty string before it.
 * We try to optimize that token out if there's interpolation
 * (although we don't if it's just an empty string, as that's valid).
 */
static Node *parse_string(Parser *parser) {
    NodeArray exprs = CREATE_DA();
    bool nextIsString = true;
    while (!MATCH(parser, TOKEN_STRING_END)) {
        ASSERT(!IS_EOF(parser), "No string end token to parse for string.");
        if (nextIsString) {
            const Token literal = CONSUME(parser, TOKEN_STRING_LIT, "Expected a string literal.");
            bool emptyString = literal.stringVal.length > 0;
            bool onlyString = CHECK(parser, TOKEN_STRING_END) && exprs.length == 0;
            if (emptyString || onlyString) {
                APPEND_DA(&exprs, new_literal_node(parser->program, literal));
            }
        } else {
            APPEND_DA(&exprs, expression(parser));
        }
        nextIsString = !nextIsString;
        MATCH(parser, TOKEN_INTERPOLATE);
    }
    return new_string_node(parser->program, exprs);
}

/** 
 * Parses and returns a primary.
 * 
 * The highest priority precedence. Includes basic things like integer literals and identifiers.
 */
static Node *primary(Parser *parser) {
    switch (ADVANCE_PEEK(parser).type) {
    case TOKEN_NULL_KW:
    case TOKEN_TRUE_KW:
    case TOKEN_FALSE_KW:
    case TOKEN_INT_KW:
    case TOKEN_FLOAT_KW:
    case TOKEN_BOOL_KW:
    case TOKEN_STRING_KW:
        return new_keyword_node(parser->program, PEEK_PREVIOUS(parser));
    case TOKEN_INT_LIT:
    case TOKEN_FLOAT_LIT:
        return new_literal_node(parser->program, PEEK_PREVIOUS(parser));
    case TOKEN_STRING_LIT:
        RETREAT(parser); // Leave parsing the whole string to the string parser.
        return parse_string(parser);
    case TOKEN_IDENTIFIER:
        return new_get_var_node(parser->program, PEEK_PREVIOUS(parser));
    case TOKEN_LPAR: {
        Node *parenthesized = expression(parser);
        CONSUME(parser, TOKEN_RPAR, "Expected ')' after parenthesized expression.");
        return new_parentheses_node(parser->program, parenthesized);
    }
    default:
        parser_error_at(parser, &PEEK_PREVIOUS(parser), true, "Invalid expression.");
        return new_error_node(parser->program);
    }
}

/** A list expression, which holds multiple values/expressions. */
static Node *list(Parser *parser) {
    if (!CHECK(parser, TOKEN_LSQUARE)) {
        return primary(parser);
    }
    NodeArray items = CREATE_DA();
    Token *leftBracket = &ADVANCE_PEEK(parser);
    if (!CHECK(parser, TOKEN_RSQUARE) && !CHECK(parser, TOKEN_EOF)) {
        APPEND_DA(&items, expression(parser));
    }
    while (!MATCH(parser, TOKEN_RSQUARE) && !parser->isPanicking) {
        if (IS_EOF(parser)) {
            parser_error_at(parser, leftBracket, true, "List was never closed.");
            return new_error_node(parser->program);
        }
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or ']' after list element.");
        APPEND_DA(&items, expression(parser));
    }
    return new_list_node(parser->program, items, leftBracket->pos);
}

/** Parses one map entry into the key and value arrays. */
static void map_entry(Parser *parser, NodeArray *keys, NodeArray *values) {
    APPEND_DA(keys, expression(parser));
    CONSUME(parser, TOKEN_COLON, "Expected ':' after map key.");
    APPEND_DA(values, expression(parser));
}

static Node *map(Parser *parser) {
    if (!CHECK(parser, TOKEN_LCURLY)) {
        return list(parser);
    }
    NodeArray keys = CREATE_DA();
    NodeArray values = CREATE_DA();
    Token *leftCurly = &ADVANCE_PEEK(parser);
    if (!CHECK(parser, TOKEN_RCURLY) && !CHECK(parser, TOKEN_EOF)) {
        map_entry(parser, &keys, &values);
        
    }
    while (!MATCH(parser, TOKEN_RCURLY) && !parser->isPanicking) {
        if (IS_EOF(parser)) {
            parser_error_at(parser, leftCurly, true, "Map was never closed.");
            return new_error_node(parser->program);
        }
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or '}' after map entry.");
        map_entry(parser, &keys, &values);
    }
    return new_map_node(parser->program, keys, values, leftCurly->pos);
}

/** Returns a parsed argument for a call. TODO: expand later with keyword arguments. */
static Node *one_arg(Parser *parser) {
    return expression(parser);
}

/** Returns an array of parsed argument expressions that should end in a closing parenthesis. */
static NodeArray parse_args(Parser *parser) {
    NodeArray args = CREATE_DA();
    if (!MATCH(parser, TOKEN_RPAR)) {
        APPEND_DA(&args, one_arg(parser));
        while (!MATCH(parser, TOKEN_RPAR) && !IS_EOF(parser) && !parser->isPanicking) {
            CONSUME(parser, TOKEN_COMMA, "Expected ',' or ')' after argument.");
            APPEND_DA(&args, one_arg(parser));
        }
    }
    return args;
}

static Node *parse_subscr(Parser *parser, Node *expr) {
    Node *subscript = expression(parser);
    CONSUME(parser, TOKEN_RSQUARE, "Expected ']' after subscript.");
    if (MATCH(parser, TOKEN_EQ)) {
        Node *value = expression(parser);
        return new_assign_subscr_node(parser->program, expr, subscript, value);
    }
    return new_get_subscr_node(parser->program, expr, subscript);
}

/** 
 * Handles parsing calls, which are an expression followed by a pair of parentheses.
 * 
 * Calls can be recursive, like calling a function which is returned by a called function
 * Example: someFunc()();
 * TODO: allow keyword arguments and optional arguments.
 */
static Node *call(Parser *parser) {
    Node *expr = map(parser);
    while (true) {
        if (MATCH(parser, TOKEN_LPAR)) {
            NodeArray args = parse_args(parser);
            expr = new_call_node(parser->program, expr, args);
        } else if (MATCH(parser, TOKEN_DOT)) {
            const Token property = CONSUME(
                parser, TOKEN_IDENTIFIER, "Expected a field or a method name after '.'."
            );
            expr = new_get_property_node(parser->program, property, expr);
        } else if (MATCH(parser, TOKEN_LSQUARE)) {
            expr = parse_subscr(parser, expr);
        } else {
            break;
        }
    }
    return expr;
}

/** 
 * A unary is just a primary with a some operation to the left of it.
 * We allow unary to be recursive to make something like (---10) possible.
 */
static Node *unary(Parser *parser) {
    if (CHECK(parser, TOKEN_MINUS) || CHECK(parser, TOKEN_BANG) || CHECK(parser, TOKEN_TILDE)) {
        const Token operation = ADVANCE_PEEK(parser);
        return new_unary_node(parser->program, operation, unary(parser));
    }
    return call(parser);    
}

/** Binary operations relating to data types, like "is" checks and "as" conversions. */
static Node *binary_data_type(Parser *parser) {
    Node *expr = unary(parser);
    if (CHECK(parser, TOKEN_IS_KW) || CHECK(parser, TOKEN_AS_KW)) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *dataType = unary(parser);
        if (
            dataType->type != AST_KEYWORD ||
            (AS_PTR(KeywordNode, dataType)->keyword != TOKEN_INT_KW
            && AS_PTR(KeywordNode, dataType)->keyword != TOKEN_FLOAT_KW
            && AS_PTR(KeywordNode, dataType)->keyword != TOKEN_BOOL_KW
            && AS_PTR(KeywordNode, dataType)->keyword != TOKEN_STRING_KW)
        ) {
            parser_error_at(
                parser, &PEEK_PREVIOUS(parser), false, "Expected data type after '%s'.",
                operation.type == TOKEN_IS_KW ? "is" : "as"
            );
            return new_error_node(parser->program);
        }
        expr = new_binary_node(parser->program, expr, operation, dataType);
    }
    return expr;
}

/** Handles the precedence of exponents, which is tighter than terms and factors. */
static Node *exponent(Parser *parser) {
    Node *expr = binary_data_type(parser);
    while (CHECK(parser, TOKEN_EXPO)) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = binary_data_type(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** A factor in Zymux is multiplication, division, or modulo. */
static Node *factor(Parser *parser) {
    Node *expr = exponent(parser);
    while (
        CHECK(parser, TOKEN_STAR) || CHECK(parser, TOKEN_SLASH) || CHECK(parser, TOKEN_PERCENT)
    ) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = exponent(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** A term encapsulates the + and - in precedence. */
static Node *term(Parser *parser) {
    Node *expr = factor(parser);
    while (CHECK(parser, TOKEN_PLUS) || CHECK(parser, TOKEN_MINUS)) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = factor(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** 
 * Includes all binary bitwise operators in one precedence.
 * 
 * so all binary bitwise operationss execute left to right. This does not include the tilde (~),
 * as it's considered a unary operator instead.
 */
static Node *binary_bitwise(Parser *parser) {
    Node *expr = term(parser);
    while (
        CHECK(parser, TOKEN_LSHIFT) || CHECK(parser, TOKEN_RSHIFT) || CHECK(parser, TOKEN_TILDE)
        || CHECK(parser, TOKEN_BAR) || CHECK(parser, TOKEN_AMPER) || CHECK(parser, TOKEN_CARET)
    ) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = term(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);   
    }
    return expr;
}

/** Comparisons handle any comparison operations other than equal and unequal. */
static Node *comparison(Parser *parser) {
    Node *expr = binary_bitwise(parser);
    while (
        CHECK(parser, TOKEN_GREATER) || CHECK(parser, TOKEN_GREATER_EQ)
        || CHECK(parser, TOKEN_LESS) || CHECK(parser, TOKEN_LESS_EQ)
    ) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = binary_bitwise(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** Precedence of checking equality or inequality. */
static Node *equality(Parser *parser) {
    Node *expr = comparison(parser);
    while (CHECK(parser, TOKEN_EQ_EQ) || CHECK(parser, TOKEN_BANG_EQ)) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = comparison(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** 
 * Return a potentially extended version of pos using the passed extension position if appropriate.
 * Otherwise (like them not being on the same line), it will just return back pos as is. 
 */
static SourcePosition extended_position(SourcePosition pos, const Token extension) {
    if (pos.line == extension.pos.line) {
        pos.length = extension.pos.length + extension.pos.column - pos.column;
    }
    return pos;
}

/** 
 * A range with a starting number, ending number, and an optional step number.
 * Adding a second double dot and a step number is optional and set to 1 by default.
 * 
 * The steps must be positive, and the language figures out whether
 * to subtract or add the step number based off whether the starting or ending number is bigger.
 */
static Node *range(Parser *parser) {
    SourcePosition rangePos = PEEK(parser).pos;
    Node *expr = equality(parser);
    if (MATCH(parser, TOKEN_DOT_DOT)) {
        Node *end = equality(parser);
        rangePos = extended_position(rangePos, PEEK_PREVIOUS(parser));

        Node *step;
        if (MATCH(parser, TOKEN_DOT_DOT)) {
            step = equality(parser);
            rangePos = extended_position(rangePos, PEEK_PREVIOUS(parser));
        } else {
            step = new_literal_node(parser->program, create_int_token("1", 10)); // Default 1 step.            
        }
        expr = new_range_node(parser->program, expr, end, step, rangePos);
    }
    return expr;
}

/** Handles binary logical operators (like "||" and "&&"). */
static Node *binary_logical(Parser *parser) {
    Node *expr = range(parser);
    if (CHECK(parser, TOKEN_BAR_BAR) || CHECK(parser, TOKEN_AMPER_AMPER)) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = binary_logical(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** 
 * Handles assignments with an operation augment.
 * 
 * Simply desugars the augmented assignment to a normal assignment with an extra
 * variable get + an operation applied to whatever is right of the assignment, and puts that
 * on a normal assignment node.
 */
static Node *augmented_assignment(Parser *parser, Node *expr) {
    if (expr->type != AST_GET_VAR) {
        parser_error_at(parser, &PEEK(parser), false, "Invalid assignment name.");
    }
    Token augAssign = ADVANCE_PEEK(parser);
    switch (augAssign.type) {
    case TOKEN_PLUS_EQ: augAssign.type = TOKEN_PLUS; break;
    case TOKEN_MINUS_EQ: augAssign.type = TOKEN_MINUS; break;
    case TOKEN_STAR_EQ: augAssign.type = TOKEN_STAR; break;
    case TOKEN_SLASH_EQ: augAssign.type = TOKEN_SLASH; break;
    case TOKEN_PERCENT_EQ: augAssign.type = TOKEN_PERCENT; break;
    case TOKEN_EXPO: augAssign.type = TOKEN_EXPO_EQ; break;
    case TOKEN_LSHIFT_EQ: augAssign.type = TOKEN_LSHIFT; break;
    case TOKEN_RSHIFT_EQ: augAssign.type = TOKEN_RSHIFT; break;
    case TOKEN_BAR_EQ: augAssign.type = TOKEN_BAR; break;
    case TOKEN_AMPER_EQ: augAssign.type = TOKEN_AMPER; break;
    case TOKEN_CARET_EQ: augAssign.type = TOKEN_CARET; break;
    default: UNREACHABLE_ERROR();
    }
    Node *value = assignment(parser);
    value = new_binary_node(parser->program, expr, augAssign, value);
    return new_assign_var_node(parser->program, AS_PTR(GetVarNode, expr)->name, value);
}

/** Handles assignment expressions. TODO: do multi-assignments and document how they're parsed. */
static Node *assignment(Parser *parser) {
    Node *expr = binary_logical(parser);
    if (CHECK(parser, TOKEN_EQ)) {
        if (expr->type != AST_GET_VAR) {
            parser_error_at(parser, &PEEK(parser), false, "Invalid assignment name.");
        }
        ADVANCE(parser); // Equal token.
        Node *value = assignment(parser);
        return new_assign_var_node(parser->program, AS_PTR(GetVarNode, expr)->name, value);
    } else if (
        CHECK(parser, TOKEN_PLUS_EQ) || CHECK(parser, TOKEN_MINUS_EQ)
        || CHECK(parser, TOKEN_STAR_EQ) || CHECK(parser, TOKEN_SLASH_EQ)
        || CHECK(parser, TOKEN_PERCENT_EQ) || CHECK(parser, TOKEN_EXPO_EQ)
        || CHECK(parser, TOKEN_LSHIFT_EQ) || CHECK(parser, TOKEN_RSHIFT_EQ)
        || CHECK(parser, TOKEN_BAR_EQ) || CHECK(parser, TOKEN_AMPER_EQ)
    ) {
        expr = augmented_assignment(parser, expr);
    }
    return expr;
}

/** A ternary operator, which is syntax sugar for a short if-else statement. */
static Node *ternary(Parser *parser) {
    Node *expr = assignment(parser);
    if (MATCH(parser, TOKEN_QUESTION_MARK)) {
        Node *trueExpr = assignment(parser);
        CONSUME(parser, TOKEN_COLON, "Expected ':' after ternary condtion and true expression.");
        Node *falseExpr = assignment(parser);
        expr = new_ternary_node(parser->program, expr, trueExpr, falseExpr);
    }
    return expr;
}

/** Parses and returns an expression. */
static Node *expression(Parser *parser) {
    return ternary(parser);
}

/** A statement which only holds an expression inside it. */
static Node *expression_stmt(Parser *parser) {
    Node *node = expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after expression.");
    return new_expr_stmt_node(parser->program, node);
}

/** 
 * A block statement which holds other statements/declarations in a deeper scope.
 * 
 * It's basically just an array of declaration statements.
 */
static Node *finish_block(Parser *parser) {
    SourcePosition pos = PEEK_PREVIOUS(parser).pos;
    NodeArray stmts = CREATE_DA();
    while (!CHECK(parser, TOKEN_RCURLY) && !IS_EOF(parser)) {
        APPEND_DA(&stmts, declaration(parser));
    }
    CONSUME(parser, TOKEN_RCURLY, "Expected closing '}' for block.");
    return new_block_node(parser->program, stmts, pos);
}

/** 
 * Holds an if condition expression, a block if it's truthy, and an optional if falsey block.
 * 
 * The falsey branch (else) either contains a block or another if-else statement,
 * which allows if/else-if/else statements.
 */
static Node *parse_if_else(Parser *parser) {
    Node *condition = expression(parser);
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after if statement's condition.");
    Node *ifBranch = finish_block(parser);
    Node *elseBranch = NULL;
    if (MATCH(parser, TOKEN_ELSE_KW)) {
        if (MATCH(parser, TOKEN_LCURLY)) {
            elseBranch = finish_block(parser);
        } else if (MATCH(parser, TOKEN_IF_KW)) {
            elseBranch = parse_if_else(parser);
        } else {
            parser_error_at(parser, &PEEK(parser), true, "Expected 'if' or '{' after else.");
        }
    }
    return new_if_else_node(parser->program, condition, AS_PTR(BlockNode, ifBranch), elseBranch);
}

/** Matches a specific expression with a set labels, if it's one of them, executes their block. */
static Node *parse_match(Parser *parser) {
    Node *matchedExpr = expression(parser);
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after the matched expression.");

    NodeArray caseLabels = CREATE_DA();
    NodeArray caseBlocks = CREATE_DA();
    while (MATCH(parser, TOKEN_CASE_KW)) {
        APPEND_DA(&caseLabels, expression(parser));
        CONSUME(parser, TOKEN_LCURLY, "Expected '{' after 'case'.");
        APPEND_DA(&caseBlocks, finish_block(parser));
    }
    
    Node *defaultCase = NULL;
    if (MATCH(parser, TOKEN_DEFAULT_KW)) {
        CONSUME(parser, TOKEN_LCURLY, "Expected '{' after 'default'.");
        defaultCase = finish_block(parser);
    }
    char *messageOnError = defaultCase == NULL ? "Expected '}' or 'case' or 'default' in match."
        : "Expected '}' after match's default.";
    CONSUME(parser, TOKEN_RCURLY, messageOnError);
    return new_match_node(
        parser->program, matchedExpr, caseLabels, caseBlocks, AS_PTR(BlockNode, defaultCase)
    );
}

/** A while loop executes its block statement as long as the loop's condition evalutes to true. */
static Node *parse_while(Parser *parser) {
    Node *condition = expression(parser);
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after while loop's condition.");
    Node *body = finish_block(parser);
    return new_while_node(parser->program, condition, AS_PTR(BlockNode, body));
}

/** Executes a block, then decides to do it again or not depending on a condition at the bottom. */
static Node *parse_do_while(Parser *parser) {
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after 'do'.");
    Node *body = finish_block(parser);
    CONSUME(parser, TOKEN_WHILE_KW, "Expected 'while' after do while loop's body");
    Node *condition = expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after do while loop's condition.");
    return new_do_while_node(parser->program, condition, AS_PTR(BlockNode, body));
}

/** Zymux uses for-in loops, which iterate over the elements of an iterable object. */
static Node *parse_for(Parser *parser) {
    Node *loopVar = new_declare_var_node(
        parser->program, CONSUME(parser, TOKEN_IDENTIFIER, "Expected loop variable after 'for'."),
        NULL_NODE(parser), false
    );
    
    CONSUME(parser, TOKEN_IN_KW, "Expected 'in' after for loop's variable name.");
    Node *iterable = expression(parser);

    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after for loop's iterable.");
    Node *body = finish_block(parser);
    return new_for_node(
        parser->program, AS_PTR(DeclareVarNode, loopVar), iterable, AS_PTR(BlockNode, body)
    );
}

/** A keyword statement followed by a semicolon to jump somewhere in a loop (like continue). */
static Node *parse_loop_control(Parser *parser) {
    const Token keyword = PEEK_PREVIOUS(parser);
    Node *node = new_loop_control_node(parser->program, keyword);
    CONSUME(
        parser, TOKEN_SEMICOLON, "Expected ';' after '%.*s'.", keyword.pos.length, keyword.lexeme
    );
    return node;
}

/** Parses a return statement and its optional value (defaults to null if nothing is provided). */
static Node *parse_return(Parser *parser) {
    Node *value;
    if (CHECK(parser, TOKEN_SEMICOLON)) {
        value = NULL_NODE(parser);
    } else {
        value = expression(parser);
    }
    CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after return statement.");
    return new_return_node(parser->program, value);
}

/** Parses and returns a statement or expression-statement. */
static Node *statement(Parser *parser) {
    Node *node;
    switch (ADVANCE_PEEK(parser).type) {
    case TOKEN_LCURLY: node = finish_block(parser); break;
    case TOKEN_IF_KW: node = parse_if_else(parser); break;
    case TOKEN_MATCH_KW: node = parse_match(parser); break;
    case TOKEN_WHILE_KW: node = parse_while(parser); break;
    case TOKEN_DO_KW: node = parse_do_while(parser); break;
    case TOKEN_FOR_KW: node = parse_for(parser); break;
    case TOKEN_CONTINUE_KW: node = parse_loop_control(parser); break;
    case TOKEN_BREAK_KW: node = parse_loop_control(parser); break;
    case TOKEN_RETURN_KW: node = parse_return(parser); break;
    default:
        RETREAT(parser);
        node = expression_stmt(parser);
        break;
    }
    return node;
}

/** 
 * Variable declarations initialize with an expression or default to null if nothing is provided.
 * 
 * TODO: add documentation for multi-variable declaration.
 */
static Node *parse_var_declaration(Parser *parser, const bool isConst) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected declared variable's name.");
    Node *value;
    if (MATCH(parser, TOKEN_SEMICOLON)) {
        value = NULL_NODE(parser);   
    } else if (MATCH(parser, TOKEN_EQ)){
        value = expression(parser);
        CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after variable's value.");
    } else {
        // TODO: later add a comma option that turns into multiple declaration
        // TODO: also, make this if-else chain a switch after the comma option is added.
        parser_error_missing(
            parser, &PEEK(parser), true, "Expected ';' or '=' after variable declaration."
        );
    }
    return new_declare_var_node(parser->program, name, value, isConst);
}

/** Declares an enum with an array of enum values that are named but just represent integers. */
static Node *parse_enum(Parser *parser) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected enum name.");
    DeclareVarNode *declaration = AS_PTR(
        DeclareVarNode, new_declare_var_node(parser->program, name, NULL, false)
    );
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after enum name.");

    TokenArray members = CREATE_DA();
    if (!MATCH(parser, TOKEN_RCURLY)) {
        const Token firstMember = CONSUME(parser, TOKEN_IDENTIFIER, "Expected enum member or '}'.");
        APPEND_DA(&members, firstMember);

        while (!MATCH(parser, TOKEN_RCURLY) && !IS_EOF(parser) && !parser->isPanicking) {
            CONSUME(parser, TOKEN_COMMA, "Expected ',' or '}' after enum member.");
            const Token member = CONSUME(
                parser, TOKEN_IDENTIFIER, "Expected enum member after ','."
            );
            APPEND_DA(&members, member);
        }
    }
    return new_enum_node(parser->program, declaration, members);
}

/** Returns a parsed parameter for a function. TODO: expand later with optional parameters. */
static Node *one_param(Parser *parser) {
    const Token paramName = CONSUME(parser, TOKEN_IDENTIFIER, "Expected parameter name");
    return new_literal_node(parser->program, paramName);
}

/** Handles parsing and returning the array of parameters for any form of function. */
static NodeArray parse_params(Parser *parser) {
    NodeArray params = CREATE_DA();
    CONSUME(parser, TOKEN_LPAR, "Expected '(' before parameters.");
    if (!MATCH(parser, TOKEN_RPAR)) {
        APPEND_DA(&params, one_param(parser));
        while (!MATCH(parser, TOKEN_RPAR) && !IS_EOF(parser) && !parser->isPanicking) {
            CONSUME(parser, TOKEN_COMMA, "Expected ',' or ')' after parameter.");
            APPEND_DA(&params, one_param(parser));
        }
    }
    return params;
}

/** Parses any type of function. TODO: Handle the few differences between funcs, methods, etc. */
static Node *parse_func(Parser *parser) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected function name.");
    DeclareVarNode *declaration = AS_PTR(
        DeclareVarNode, new_declare_var_node(parser->program, name, NULL, false)
    );
    
    NodeArray params = parse_params(parser);
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after function parameters.");
    Node *body = finish_block(parser);
    return new_func_node(parser->program, declaration, params, AS_PTR(BlockNode, body));
}

/** 
 * Synchronizes the parses when panicking.
 * 
 * Synchronization is done by skipping until a token that can begin a statement,
 * or a token that ends a statement. If it's one that ends the statement,
 * then that token should be skipped too (as a lone semicolon isn't a statement for example).
 */
static void synchronize(Parser *parser) {
    parser->isPanicking = false;
    parser->current = parser->syncSpot;
    parser->syncSpot = NULL;

    while (!IS_EOF(parser)) {
        switch (PEEK(parser).type) {
        case TOKEN_IF_KW:
        case TOKEN_FOR_KW:
        case TOKEN_WHILE_KW:
        case TOKEN_DO_KW:
        case TOKEN_CLASS_KW:
        case TOKEN_FUNC_KW:
        case TOKEN_LET_KW:
        case TOKEN_CONST_KW:
        case TOKEN_RETURN_KW:
        case TOKEN_PRIVATE_KW:
        case TOKEN_CONTINUE_KW:
        case TOKEN_INIT_KW:
        case TOKEN_ABSTRACT_KW:
        case TOKEN_MATCH_KW:
        case TOKEN_FROM_KW:
        case TOKEN_IMPORT_KW:
            parser->isPanicking = false;
            return;

        default:
            ADVANCE(parser);
            break;
        }
    }
}

/** Parses and returns a declaration of something or a statement otherwise. */
static Node *declaration(Parser *parser) {
    Node *node;
    switch (ADVANCE_PEEK(parser).type) {
    case TOKEN_LET_KW: node = parse_var_declaration(parser, false); break;
    case TOKEN_CONST_KW: node = parse_var_declaration(parser, true); break;
    case TOKEN_ENUM_KW: node = parse_enum(parser); break;
    case TOKEN_FUNC_KW: node = parse_func(parser); break;
    default:
        RETREAT(parser);
        node = statement(parser);
        break;
    }

    if (parser->isPanicking) {
        synchronize(parser);
    }
    return node;
}

/**
 * Parses the array of tokens in the parser.
 * 
 * Places the parsed nodes inside the passed parser's ast field.
 * Returns whether or not parsing was successful.
 */
bool parse(Parser *parser) {
    while (!IS_EOF(parser)) {
        APPEND_DA(&parser->ast, declaration(parser));
    }
    APPEND_DA(&parser->ast, new_eof_node(parser->program, PEEK(parser).pos));

#if DEBUG_AST
    print_ast(&parser->ast);
#endif
    return !parser->program->hasErrored;
}
