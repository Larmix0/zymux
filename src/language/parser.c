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
        : (parser_error_missing(parser, PEEK_PREVIOUS(parser), true, __VA_ARGS__), PEEK(parser)))
            
#define IS_EOF(parser) (CHECK(parser, TOKEN_EOF))

static Node *expression(Parser *parser);
static Node *declaration(Parser *parser);
static Node *multi_declaration(Parser *parser, const bool isConst, const bool scanValue);

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

/** Reports a parsing error on a specific, erroneous token, then panics if that is set to true. */
static void parser_error_at(
    Parser *parser, Token erroredToken, const bool shouldPanic, const char *format, ...
) {
    if (parser->isPanicking) {
        return;
    }

    if (shouldPanic) {
        ASSERT(erroredToken.lexedIdx >= 0, "Expected positive number for parser error index.");
        parser->syncSpot = &parser->tokens.data[erroredToken.lexedIdx];
        parser->isPanicking = true;
    }
    va_list args;
    va_start(args, format);
    zmx_user_error(parser->program, erroredToken.pos, "Syntax error", format, &args);
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
    Parser *parser, Token beforeMissing, const bool shouldPanic, const char *format, ...
) {
    if (parser->isPanicking) {
        return;
    }

    if (shouldPanic) {
        ASSERT(beforeMissing.lexedIdx >= 0, "Expected positive number for parser error index.");
        parser->syncSpot = &parser->tokens.data[beforeMissing.lexedIdx + 1];
        parser->isPanicking = true;
    }
    // Errors out the character after the token located before the missing one.
    const SourcePosition errorPos = create_src_pos(
        beforeMissing.pos.line, beforeMissing.pos.column + beforeMissing.pos.length, 1
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
    SourcePosition pos = PEEK(parser).pos;
    bool nextIsString = true;
    while (!MATCH(parser, TOKEN_STRING_END) && !parser->isPanicking) {
        ASSERT(!IS_EOF(parser), "No string end token to parse for string.");
        if (nextIsString) {
            const Token literal = CONSUME(parser, TOKEN_STRING_LIT, "Expected a string literal.");
            const bool emptyString = literal.stringVal.length > 0;
            const bool onlyString = CHECK(parser, TOKEN_STRING_END) && exprs.length == 0;
            if (emptyString || onlyString) {
                APPEND_DA(&exprs, new_literal_node(parser->program, literal));
            }
        } else {
            APPEND_DA(&exprs, expression(parser));
        }
        nextIsString = !nextIsString;
        MATCH(parser, TOKEN_INTERPOLATE);
    }
    return new_string_node(parser->program, exprs, pos);
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
    case TOKEN_THIS_KW:
        return new_get_var_node(parser->program, PEEK_PREVIOUS(parser));
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
        parser_error_at(parser, PEEK_PREVIOUS(parser), true, "Invalid expression.");
        return new_error_node(parser->program);
    }
}

/** A list expression, which holds multiple values/expressions. */
static Node *list(Parser *parser) {
    if (!MATCH(parser, TOKEN_LSQUARE)) {
        return primary(parser);
    }
    NodeArray items = CREATE_DA();
    Token leftBracket = PEEK_PREVIOUS(parser);
    if (!CHECK(parser, TOKEN_RSQUARE) && !CHECK(parser, TOKEN_EOF)) {
        APPEND_DA(&items, expression(parser));
    }
    while (!CHECK(parser, TOKEN_RSQUARE) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or ']' after list element.");
        APPEND_DA(&items, expression(parser));
    }
    CONSUME(parser, TOKEN_RSQUARE, "Expected ',' or ']' in list.");
    return new_list_node(parser->program, items, leftBracket.pos);
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
    Token leftCurly = ADVANCE_PEEK(parser);
    if (!CHECK(parser, TOKEN_RCURLY) && !CHECK(parser, TOKEN_EOF)) {
        map_entry(parser, &keys, &values);   
    }
    while (!CHECK(parser, TOKEN_RCURLY) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or '}' after map entry.");
        map_entry(parser, &keys, &values);
    }
    CONSUME(parser, TOKEN_RCURLY, "Expected ',' or '}' in map.");
    return new_map_node(parser->program, keys, values, leftCurly.pos);
}

/** Errors out if the passed token (presumably a keyword argument name) is already in kwargs. */
static void check_keyword_arg_repeated(Parser *parser, const Token name, MapNode *kwargs) {
    for (u32 i = 0; i < kwargs->keys.length; i++) {
        if (equal_token(name, AS_PTR(LiteralNode, kwargs->keys.data[i])->value)) {
            parser_error_at(parser, name, false, "Repeated keyword argument.");
            return;
        }
    }
}

/** 
 * Parses one argument in a function call, and appends it to its appropriate array/map of args.
 * 
 * Returns a boolean of whether or not keyword args have started so that boolean can be passed
 * to the next call to this function. This is because when keyword arguments start
 * we only allow parsing them after, as positionals are done.
 */
static bool one_arg(
    Parser *parser, NodeArray *positionals, MapNode *kwargs, const bool keywordsStarted
) {
    if (keywordsStarted || CHECK(parser, TOKEN_DOT)) {
        CONSUME(parser, TOKEN_DOT, "Expected '.' and then a name for a keyword argument.");
        const Token name = as_string_token(
            CONSUME(parser, TOKEN_IDENTIFIER, "Expected keyword argument name.")
        );
        check_keyword_arg_repeated(parser, name, kwargs);
        CONSUME(parser, TOKEN_EQ, "Expected '=' after keyword argument name.");

        APPEND_DA(&kwargs->keys, new_literal_node(parser->program, name));
        APPEND_DA(&kwargs->values, expression(parser));
        return true;
    } else {
        APPEND_DA(positionals, expression(parser));
        return false;
    }
}

/** Returns an array of parsed argument expressions that should end in a closing parenthesis. */
static void parse_args(Parser *parser, NodeArray *positionals, MapNode *kwargs) {
    if (MATCH(parser, TOKEN_RPAR)) {
        return; // No args to append.
    }
    bool keywordsStarted = one_arg(parser, positionals, kwargs, false);
    while (!CHECK(parser, TOKEN_RPAR) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or ')' after argument.");
        keywordsStarted = one_arg(parser, positionals, kwargs, keywordsStarted);
    }
    CONSUME(parser, TOKEN_RPAR, "Expected ',' or ')' in call arguments.");
}

/** Returns a parsed subscript expression. Either a get or an assign on some subscript. */
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
 * Handles and returns call precedence, which are expressions tied to a tight thing on their right.
 * 
 * Calls can be recursive, like calling a function which is returned by a called function,
 * or subscripting a 2d list.
 * 
 * Example: someFunc()();
 */
static Node *call(Parser *parser) {
    Node *expr = map(parser);
    while (true) {
        if (MATCH(parser, TOKEN_LPAR)) {
            NodeArray positionalArgs = CREATE_DA(), kwNames = CREATE_DA(), kwValues = CREATE_DA();
            MapNode *kwArgs = AS_PTR(
                MapNode, new_map_node(parser->program, kwNames, kwValues, get_node_pos(expr))
            );
            parse_args(parser, &positionalArgs, kwArgs);
            expr = new_call_node(parser->program, expr, positionalArgs, kwArgs);
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
    while (CHECK(parser, TOKEN_IS_KW) || CHECK(parser, TOKEN_AS_KW)) {
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
                parser, PEEK_PREVIOUS(parser), false, "Expected data type after '%s'.",
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
 * so all binary bitwise operationss execute left to right.
 * This does not include the tilde (~), as it's considered a unary operator instead.
 */
static Node *binary_bitwise(Parser *parser) {
    Node *expr = term(parser);
    while (
        CHECK(parser, TOKEN_LSHIFT) || CHECK(parser, TOKEN_RSHIFT)
        || CHECK(parser, TOKEN_BAR) || CHECK(parser, TOKEN_AMPER) || CHECK(parser, TOKEN_CARET)
    ) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = term(parser);
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
    Node *expr = binary_bitwise(parser);
    if (MATCH(parser, TOKEN_DOT_DOT)) {
        Node *end = binary_bitwise(parser);
        rangePos = extended_position(rangePos, PEEK_PREVIOUS(parser));

        Node *step;
        if (MATCH(parser, TOKEN_DOT_DOT)) {
            step = binary_bitwise(parser);
            rangePos = extended_position(rangePos, PEEK_PREVIOUS(parser));
        } else {
            // Default step of 1.
            step = new_literal_node(parser->program, create_int_token("1", 10, -1));            
        }
        expr = new_range_node(parser->program, expr, end, step, rangePos);
    }
    return expr;
}

/** Comparisons handle any comparison operations other than equal and unequal. */
static Node *comparison(Parser *parser) {
    Node *expr = range(parser);
    while (
        CHECK(parser, TOKEN_GREATER) || CHECK(parser, TOKEN_GREATER_EQ)
        || CHECK(parser, TOKEN_LESS) || CHECK(parser, TOKEN_LESS_EQ)
    ) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = range(parser);
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

/** Handles binary logical operators (like "||" and "&&"). */
static Node *binary_logical(Parser *parser) {
    Node *expr = equality(parser);
    if (CHECK(parser, TOKEN_BAR_BAR) || CHECK(parser, TOKEN_AMPER_AMPER)) {
        const Token operation = ADVANCE_PEEK(parser);
        Node *rhs = expression(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** 
 * Handles verifying that a list node is filled with valid assignable names as elements.
 * 
 * This function expects to be called right after consuming the closing ']' bracket.
 * It's primary purpose is just to throw errors at the user if their assignment list is invalid,
 * but otherwise doesn't affect the flow of the program.
 */
static Node *multi_assignment(Parser *parser, ListNode *list) {
    Token leftBracket = PEEK_PREVIOUS(parser);
    if (list->items.length == 0) {
        parser_error_at(parser, leftBracket, false, "Assignment list can't be empty.");
    }
    ADVANCE(parser); // Equal token.
    Node *value = expression(parser);

    NodeArray assigns = CREATE_DA();
    for (u32 i = 0; i < list->items.length; i++) {
        Node *assignVar = list->items.data[i];
        if (assignVar->type == AST_GET_VAR) {
            const Token name = AS_PTR(GetVarNode, list->items.data[i])->name;
            APPEND_DA(&assigns, new_assign_var_node(parser->program, name, NULL));
        } else if (assignVar->type == AST_GET_PROPERTY) {
            GetPropertyNode *get = AS_PTR(GetPropertyNode, list->items.data[i]);
            APPEND_DA(&assigns, new_set_property_node(parser->program, get, NULL));
        } else {
            parser_error_at(parser, leftBracket, false, "Expected only valid assignment names.");
        }
    }
    return new_multi_assign_node(parser->program, assigns, value, list->pos);
}

/** 
 * Handles assignments with an operation augment.
 * 
 * Simply desugars the augmented assignment to a normal assignment with an extra
 * variable get + an operation applied to whatever is right of the assignment, and puts that
 * on a normal assignment node.
 */
static Node *augmented_assignment(Parser *parser, Node *expr) {
    const Token assignName = PEEK_PREVIOUS(parser);
    Token augAssign = ADVANCE_PEEK(parser);
    if (expr->type != AST_GET_VAR) {
        parser_error_at(parser, assignName, false, "Invalid assignment name.");
    }
    switch (augAssign.type) {
    case TOKEN_PLUS_EQ: augAssign.type = TOKEN_PLUS; break;
    case TOKEN_MINUS_EQ: augAssign.type = TOKEN_MINUS; break;
    case TOKEN_STAR_EQ: augAssign.type = TOKEN_STAR; break;
    case TOKEN_SLASH_EQ: augAssign.type = TOKEN_SLASH; break;
    case TOKEN_PERCENT_EQ: augAssign.type = TOKEN_PERCENT; break;
    case TOKEN_EXPO_EQ: augAssign.type = TOKEN_EXPO; break;
    case TOKEN_LSHIFT_EQ: augAssign.type = TOKEN_LSHIFT; break;
    case TOKEN_RSHIFT_EQ: augAssign.type = TOKEN_RSHIFT; break;
    case TOKEN_BAR_EQ: augAssign.type = TOKEN_BAR; break;
    case TOKEN_AMPER_EQ: augAssign.type = TOKEN_AMPER; break;
    case TOKEN_CARET_EQ: augAssign.type = TOKEN_CARET; break;
    default: UNREACHABLE_ERROR();
    }
    Node *value = expression(parser);
    value = new_binary_node(parser->program, expr, augAssign, value);
    return new_assign_var_node(parser->program, AS_PTR(GetVarNode, expr)->name, value);
}

/** Handles assignment expressions. Includes augmented assigns and multi-variable assigns. */
static Node *assignment(Parser *parser) {
    Node *expr = binary_logical(parser);
    if (CHECK(parser, TOKEN_EQ)) {
        const Token assignName = PEEK_PREVIOUS(parser);
        if (expr->type == AST_LIST) {
            return multi_assignment(parser, AS_PTR(ListNode, expr));
        }
        ADVANCE(parser); // Equal token.
        Node *value = expression(parser);

        if (expr->type == AST_GET_VAR) {
            return new_assign_var_node(parser->program, AS_PTR(GetVarNode, expr)->name, value);
        } else if (expr->type == AST_GET_PROPERTY) {
            return new_set_property_node(parser->program, AS_PTR(GetPropertyNode, expr), value);
        } else {
            parser_error_at(parser, assignName, false, "Invalid assignment name.");
        }
    } else if (
        CHECK(parser, TOKEN_PLUS_EQ) || CHECK(parser, TOKEN_MINUS_EQ)
        || CHECK(parser, TOKEN_STAR_EQ) || CHECK(parser, TOKEN_SLASH_EQ)
        || CHECK(parser, TOKEN_PERCENT_EQ) || CHECK(parser, TOKEN_EXPO_EQ)
        || CHECK(parser, TOKEN_LSHIFT_EQ) || CHECK(parser, TOKEN_RSHIFT_EQ)
        || CHECK(parser, TOKEN_BAR_EQ) || CHECK(parser, TOKEN_AMPER_EQ)
        || CHECK(parser, TOKEN_CARET_EQ)
    ) {
        expr = augmented_assignment(parser, expr);
    }
    return expr;
}

/** A ternary operator, which is syntax sugar for a short if-else statement. */
static Node *ternary(Parser *parser) {
    Node *expr = assignment(parser);
    if (MATCH(parser, TOKEN_QUESTION_MARK)) {
        Node *trueExpr = expression(parser);
        CONSUME(
            parser, TOKEN_COLON, "Expected ':' after ternary condtion and true case expression."
        );
        Node *falseExpr = expression(parser);
        expr = new_ternary_node(parser->program, expr, trueExpr, falseExpr);
    }
    return expr;
}

/** Parses and returns an expression. General function that starts with the lowest precedence. */
static Node *expression(Parser *parser) {
    if (IS_EOF(parser)) {
        parser_error_missing(parser, PEEK_PREVIOUS(parser), true, "Expected expression.");
        return new_error_node(parser->program);
    }
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
    while (!CHECK(parser, TOKEN_RCURLY) && !IS_EOF(parser) && !parser->isPanicking) {
        APPEND_DA(&stmts, declaration(parser));
    }
    CONSUME(parser, TOKEN_RCURLY, "Expected closing '}'.");
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
    Node *ifBlock = finish_block(parser);
    Node *elseBlock = NULL;
    if (MATCH(parser, TOKEN_ELSE_KW)) {
        if (MATCH(parser, TOKEN_LCURLY)) {
            elseBlock = finish_block(parser);
        } else if (MATCH(parser, TOKEN_IF_KW)) {
            elseBlock = parse_if_else(parser);
        } else {
            parser_error_at(parser, PEEK(parser), true, "Expected 'if' or '{' after else.");
        }
    }
    return new_if_else_node(parser->program, condition, AS_PTR(BlockNode, ifBlock), elseBlock);
}

/** 
 * Wraps a block in a try statement and when it errors, executes a catch block and continues.
 * The catch is a mandatory part after the try block.
 */
static Node *parse_try_catch(Parser *parser) {
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after 'try'.");
    Node *tryBlock = finish_block(parser);

    CONSUME(parser, TOKEN_CATCH_KW, "Expected 'catch' after 'try' block.");
    DeclareVarNode *catchVar = NULL;
    if (MATCH(parser, TOKEN_AS_KW)) {
        const Token name = CONSUME(
            parser, TOKEN_IDENTIFIER, "Expected error message variable name after 'as'."
        );
        catchVar = NO_VALUE_DECLARATION(parser->program, name);
    }
    
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after 'catch'.");
    Node *catchBlock = finish_block(parser);
    return new_try_catch_node(
        parser->program, AS_PTR(BlockNode, tryBlock), AS_PTR(BlockNode, catchBlock), catchVar
    );
}

/** A statement which induces an error with a message at runtime. */
static Node *parse_raise(Parser *parser) {
    const SourcePosition pos = PEEK_PREVIOUS(parser).pos; // The raise keyword.
    Node *message = expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after raise message.");
    return new_raise_node(parser->program, message, pos);
}

/** Parses one case into the case labels and case blocks arrays. */
static Node *parse_case(Parser *parser) {
    NodeArray labelVals = CREATE_DA();
    APPEND_DA(&labelVals, expression(parser));
    const SourcePosition pos = get_node_pos(labelVals.data[0]); // First label value we just parsed.

    while (!CHECK(parser, TOKEN_LCURLY) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected '{' or ',' after case label value.");
        APPEND_DA(&labelVals, expression(parser));
    }
    CONSUME(parser, TOKEN_LCURLY, "Expected block after case label.");
    Node *caseBlock = finish_block(parser);
    return new_case_node(parser->program, labelVals, AS_PTR(BlockNode, caseBlock), pos);
}

/** Matches a specific expression with a set labels, if it's one of them, executes their block. */
static Node *parse_match(Parser *parser) {
    Node *matchedExpr = expression(parser);
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after the matched expression.");

    NodeArray cases = CREATE_DA();
    while (MATCH(parser, TOKEN_CASE_KW)) {
        APPEND_DA(&cases, parse_case(parser));
    }
    
    Node *defaultCase = NULL;
    if (MATCH(parser, TOKEN_DEFAULT_KW)) {
        CONSUME(parser, TOKEN_LCURLY, "Expected '{' after 'default'.");
        defaultCase = finish_block(parser);
    }
    char *messageOnError = defaultCase == NULL ? "Expected '}' or 'case' or 'default' in match."
        : "Expected '}' after match's default.";
    CONSUME(parser, TOKEN_RCURLY, messageOnError);
    
    return new_match_node(parser->program, matchedExpr, cases, AS_PTR(BlockNode, defaultCase));
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
    CONSUME(parser, TOKEN_WHILE_KW, "Expected 'while' after do while loop's body.");
    Node *condition = expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after do while loop's condition.");
    return new_do_while_node(parser->program, condition, AS_PTR(BlockNode, body));
}

/** Zymux uses for-in loops, which iterate over the elements of an iterable object. */
static Node *parse_for(Parser *parser) {
    Node *loopVar;
    if (MATCH(parser, TOKEN_IDENTIFIER)) {
        loopVar = new_declare_var_node(
            parser->program, PEEK_PREVIOUS(parser), NULL_NODE(parser->program), false
        );
    } else if (MATCH(parser, TOKEN_LSQUARE)) {
        loopVar = multi_declaration(parser, false, false);
    } else {
        parser_error_at(parser, PEEK(parser), true, "Expected for loop variable.");
        return new_error_node(parser->program);
    }
    
    CONSUME(parser, TOKEN_IN_KW, "Expected 'in' after for loop's variable.");
    Node *iterable = expression(parser);

    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after for loop's iterable.");
    Node *body = finish_block(parser);
    return new_for_node(parser->program, loopVar, iterable, AS_PTR(BlockNode, body));
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

/** Parses a return statement and its optional value (defaults to NULL if nothing is provided). */
static Node *parse_return(Parser *parser) {
    const SourcePosition pos = PEEK_PREVIOUS(parser).pos;
    Node *value = CHECK(parser, TOKEN_SEMICOLON) ? NULL : expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after return.");
    return new_return_node(parser->program, value, pos);
}

/** Parses and returns a statement or expression-statement. */
static Node *statement(Parser *parser) {
    Node *node;
    switch (ADVANCE_PEEK(parser).type) {
    case TOKEN_LCURLY: node = finish_block(parser); break;
    case TOKEN_IF_KW: node = parse_if_else(parser); break;
    case TOKEN_TRY_KW: node = parse_try_catch(parser); break;
    case TOKEN_RAISE_KW: node = parse_raise(parser); break;
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
 * Finishes the parsing of some variable declaration and returns the value parsed.
 * 
 * Either an actual parsed node, or an implicit null node is returned as the value.
 * Only returns a C NULL address if it errored.
 */
static Node *declaration_value(Parser *parser) {
    Node *value = NULL;
    if (MATCH(parser, TOKEN_SEMICOLON)) {
        value = NULL_NODE(parser->program);   
    } else if (MATCH(parser, TOKEN_EQ)){
        value = expression(parser);
        CONSUME(parser, TOKEN_SEMICOLON, "Expected ';' after declaration's value.");
    } else {
        parser_error_at(parser, PEEK(parser), true, "Expected ';' or '=' after declaration.");
    }
    return value;
}

/** Returns a node of one name declaration that is a part of a multi-variable declaration. */
static Node *one_declaration(Parser *parser) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected variable name.");
    return AS_NODE(NO_VALUE_DECLARATION(parser->program, name));
}

/** 
 * Returns a node of multiple declarations to one (presumably) iterable value.
 * Assumes the left bracket was consumed.
 */
static Node *multi_declaration(Parser *parser, const bool isConst, const bool scanValue) {
    const SourcePosition pos = PEEK_PREVIOUS(parser).pos; // Left bracket.
    if (MATCH(parser, TOKEN_RSQUARE)) {
        parser_error_at(
            parser, PEEK_PREVIOUS(parser), false,
            "Expected at least one variable in multi-variable declaration."
        );
        return new_error_node(parser->program);
    }

    NodeArray declarations = CREATE_DA();
    APPEND_DA(&declarations, one_declaration(parser));
    while (!CHECK(parser, TOKEN_RSQUARE) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or ']' after declared variable's name.");
        APPEND_DA(&declarations, one_declaration(parser));
    }
    CONSUME(parser, TOKEN_RSQUARE, "Expected closing ']' in multi-variable declaration.");

    Node *value = !scanValue || MATCH(parser, TOKEN_SEMICOLON) ? NULL : declaration_value(parser);
    return new_multi_declare_node(parser->program, declarations, value, isConst, pos);
}

/** 
 * Variable declarations initialize with an expression or default to null if nothing is provided.
 * This includes normal declarations and multi-variable declarations.
 */
static Node *parse_var_declaration(Parser *parser, const bool isConst) {
    if (MATCH(parser, TOKEN_LSQUARE)) {
        return multi_declaration(parser, isConst, true);
    } else {
        const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected declared variable's name.");
        return new_declare_var_node(parser->program, name, declaration_value(parser), isConst);
    }
}

/** Declares an enum with an array of enum values that are named but just represent integers. */
static Node *parse_enum(Parser *parser) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected enum name.");
    DeclareVarNode *declaration = NO_VALUE_DECLARATION(parser->program, name);

    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after enum name.");
    TokenArray members = CREATE_DA();
    if (MATCH(parser, TOKEN_RCURLY)) {
        return new_enum_node(parser->program, declaration, members);
    }

    const Token firstMember = CONSUME(parser, TOKEN_IDENTIFIER, "Expected enum member or '}'.");
    APPEND_DA(&members, firstMember);
    while (!CHECK(parser, TOKEN_RCURLY) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or '}' after enum member.");
        const Token member = CONSUME(parser, TOKEN_IDENTIFIER, "Expected enum member after ','.");
        APPEND_DA(&members, member);
    }
    CONSUME(parser, TOKEN_RCURLY, "Expected ',' or '}' in enum.");
    return new_enum_node(parser->program, declaration, members);
}

/** 
 * Parses one parameter of a function and appends it to its appropriate parameter array.
 * 
 * Returns a boolean of whether or not optional parameters have started so that boolean
 * can be passed to the next call to this function. This is because when optional parameters start
 * we only allow parsing them after, as mandatories are done.
 */
static bool one_param(
    Parser *parser, NodeArray *mandatories, NodeArray *optionals, bool optionalsStarted
) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected parameter name.");
    Node *value = NULL;
    if (optionalsStarted) {
        CONSUME(parser, TOKEN_EQ, "Expected default values for the rest of the parameters.");
        value = expression(parser);
    } else if (MATCH(parser, TOKEN_EQ)) {
        optionalsStarted = true;
        value = expression(parser);
    }

    Node *parameter = new_declare_var_node(parser->program, name, value, false);
    if (optionalsStarted) {
        APPEND_DA(optionals, parameter);
    } else {
        APPEND_DA(mandatories, parameter);
    }
    return optionalsStarted;
}

/** Handles parsing the arrays of mandatory and optional parameters for a function. */
static void parse_params(Parser *parser, NodeArray *mandatories, NodeArray *optionals) {
    CONSUME(parser, TOKEN_LPAR, "Expected '(' before parameters.");
    if (MATCH(parser, TOKEN_RPAR)) {
        return; // No parameters.
    }

    bool optionalsStarted = one_param(parser, mandatories, optionals, false);
    while (!CHECK(parser, TOKEN_RPAR) && !IS_EOF(parser) && !parser->isPanicking) {
        CONSUME(parser, TOKEN_COMMA, "Expected ',' or ')' after parameter.");
        optionalsStarted = one_param(parser, mandatories, optionals, optionalsStarted);
    }
    CONSUME(parser, TOKEN_RPAR, "Expected ',' or ')' in parameters.");
}

/** Parses any type of function whose name was already parsed and passed to this. */
static Node *named_func(Parser *parser, const Token name) {
    DeclareVarNode *declaration = NO_VALUE_DECLARATION(parser->program, name);
    NodeArray mandatoryParams = CREATE_DA(), optionalParams = CREATE_DA();
    parse_params(parser, &mandatoryParams, &optionalParams);

    CONSUME(parser, TOKEN_LCURLY, "Expected '{' after function parameters.");
    Node *body = finish_block(parser);
    return new_func_node(
        parser->program, declaration, mandatoryParams, optionalParams, AS_PTR(BlockNode, body)
    );
}

/** Parses a function and its name before the function itself for convenience. */
static Node *parse_func(Parser *parser) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected function name.");
    return named_func(parser, name);
}

/** Handles parsing one method (or initializers) and putting it inside the class node. */
static void class_method(Parser *parser, ClassNode *classNode) {
    if (CHECK(parser, TOKEN_INIT_KW)) {
        if (classNode->init) {
            parser_error_at(
                parser, PEEK(parser), false, "Can't have multiple intializers in class."
            );
        }
        classNode->init = AS_PTR(FuncNode, named_func(parser, ADVANCE_PEEK(parser)));
    } else {
        APPEND_DA(&classNode->methods, parse_func(parser));
    }
}

/** Handles parsing a class's body, and adds its information to the class node. */
static void class_body(Parser *parser, ClassNode *classNode) {
    CONSUME(parser, TOKEN_LCURLY, "Expected '{' for the class body.");
    while (!CHECK(parser, TOKEN_RCURLY) && !IS_EOF(parser) && !parser->isPanicking) {
        class_method(parser, classNode);
    }
    CONSUME(parser, TOKEN_RCURLY, "Expected '}' at the end of class.");
}

/** Parses and returns an entire class with all of its information. */
static Node *parse_class(Parser *parser) {
    const Token name = CONSUME(parser, TOKEN_IDENTIFIER, "Expected class name.");
    DeclareVarNode *declaration = NO_VALUE_DECLARATION(parser->program, name);
    ClassNode *classNode = AS_PTR(ClassNode, new_class_node(parser->program, declaration));

    class_body(parser, classNode);
    return AS_NODE(classNode);
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
        case TOKEN_FROM_KW: // TODO: does "from" start an import statement, or is it in the middle?
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
    case TOKEN_CLASS_KW: node = parse_class(parser); break;
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
