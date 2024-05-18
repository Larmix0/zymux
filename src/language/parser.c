#include "data_structures.h"
#include "parser.h"
#include "report_error.h"

#define PEEK(parser) (*(parser)->current)
#define PEEK_PREVIOUS(parser) (*((parser)->current - 1))
#define CHECK(parser, expected) (PEEK(parser).type == (expected))

#define ADVANCE_PEEK(parser) (*(parser)->current++)
#define ADVANCE(parser) ((parser)->current++)
#define RETREAT(parser) ((parser)->current--)

#define MATCH(parser, expected) (CHECK(parser, expected) ? (ADVANCE(parser), true) : false)
#define CONSUME(parser, expected, message) \
    (CHECK(parser, expected) \
        ? ADVANCE_PEEK(parser) \
        : (raise_parser_error_missing(parser, &PEEK_PREVIOUS(parser), message), \
            PEEK(parser)))
            
#define IS_EOF(parser) (CHECK(parser, TOKEN_EOF))

static Node *expression(Parser *parser);

/** Returns an initialized parser. */
Parser create_parser(ZmxProgram *program, const TokenArray tokens) {
    Parser parser = {
        .isPanicking = false, .ast = CREATE_DA(), .program = program,
        .tokens = tokens, .current = tokens.data, .syncSpot = NULL
    };
    return parser;
}

/** Frees all the memory the passed parser owns. */
void free_parser(Parser *parser) {
    FREE_DA(&parser->ast);
}

/** Reports a parsing error on a specific, erroneous token, then starts panicking. */
static void raise_parser_error_at(Parser *parser, Token *erroredToken, const char *message) {
    if (parser->isPanicking) {
        return;
    }
    parser->isPanicking = true;
    parser->syncSpot = erroredToken;
    SYNTAX_ERROR(
        parser->program, erroredToken->pos, message
    );
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
static void raise_parser_error_missing(Parser *parser, Token *beforeMissing, const char *message) {
    if (parser->isPanicking) {
        return;
    }
    parser->isPanicking = true;
    parser->syncSpot = beforeMissing;

    // Errors out the character after the token located before the missing one.
    SourcePosition errorPos = create_src_pos(
        beforeMissing->pos.line, beforeMissing->pos.column + beforeMissing->pos.length, 1
    );
    SYNTAX_ERROR(parser->program, errorPos, message);
}

/**
 * Parse out a full string.
 * 
 * This is done by alternating between parsing a string literal and an interpolated expression
 * until the STRING_END token is seen.
 */
static Node *string(Parser *parser) {
    NodeArray exprs = CREATE_DA();
    bool nextIsString = true;
    while (!MATCH(parser, TOKEN_STRING_END)) {
        if (nextIsString) {
            Token literal = CONSUME(parser, TOKEN_STRING_LIT, "Expected a string literal.");
            APPEND_DA(&exprs, new_literal_node(parser->program, literal));
        } else {
            APPEND_DA(&exprs, expression(parser));
        }
        nextIsString = nextIsString ? false : true;
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
    case TOKEN_TRUE_KW:
    case TOKEN_FALSE_KW:
        return new_keyword_node(parser->program, PEEK_PREVIOUS(parser));
    case TOKEN_INT_LIT:
    case TOKEN_FLOAT_LIT:
        return new_literal_node(parser->program, PEEK_PREVIOUS(parser));
    case TOKEN_STRING_LIT:
        RETREAT(parser); // Leave parsing the whole string to the string parser.
        return string(parser);
    default:
        raise_parser_error_at(parser, &PEEK_PREVIOUS(parser), "Invalid expression.");
        return new_error_node(parser->program);
    }
}

/** A unary is just a primary with a some operation to the left of it. */
static Node *unary(Parser *parser) {
    if (CHECK(parser, TOKEN_MINUS) || CHECK(parser, TOKEN_BANG)) {
        Token operation = ADVANCE_PEEK(parser);
        return new_unary_node(parser->program, operation, unary(parser));
    } else {
        return primary(parser);
    }    
}

/** Handles the precedence of exponents, which is tighter than terms and factors. */
static Node *exponent(Parser *parser) {
    Node *expr = unary(parser);
    while (CHECK(parser, TOKEN_EXPO)) {
        Token operation = ADVANCE_PEEK(parser);
        Node *rhs = unary(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** A factor in Zymux is multiplication, division, or modulo. */
static Node *factor(Parser *parser) {
    Node *expr = exponent(parser);
    while (CHECK(parser, TOKEN_STAR) || CHECK(parser, TOKEN_SLASH) || CHECK(parser, TOKEN_MODULO)) {
        Token operation = ADVANCE_PEEK(parser);
        Node *rhs = exponent(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** A term encapsulates the + and - in precedence. */
static Node *term(Parser *parser) {
    Node *expr = factor(parser);
    while (CHECK(parser, TOKEN_PLUS) || CHECK(parser, TOKEN_MINUS)) {
        Token operation = ADVANCE_PEEK(parser);
        Node *rhs = factor(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** Comparisons handle any comparison operations other than equal and unequal. */
static Node *comparison(Parser *parser) {
    Node *expr = term(parser);
    while (CHECK(parser, TOKEN_GREATER) || CHECK(parser, TOKEN_GREATER_EQ)
            || CHECK(parser, TOKEN_LESS) || CHECK(parser, TOKEN_LESS_EQ)) {
        Token operation = ADVANCE_PEEK(parser);
        Node *rhs = term(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

/** Handles equal and not equal's precedence (which are executed after other comparisons). */
static Node *equality(Parser *parser) {
    Node *expr = comparison(parser);
    while (CHECK(parser, TOKEN_EQ_EQ) || CHECK(parser, TOKEN_BANG_EQ)) {
        Token operation = ADVANCE_PEEK(parser);
        Node *rhs = comparison(parser);
        expr = new_binary_node(parser->program, expr, operation, rhs);
    }
    return expr;
}

// TODO: find out if compound assignment is just underneath normal assignments in precedence.

/** Parses and returns an expression. */
static Node *expression(Parser *parser) {
    return equality(parser);
}

/** A statement which only holds an expression inside it. */
static Node *expression_stmt(Parser *parser) {
    Node *node = expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected \";\" after expression.");
    return new_expr_stmt_node(parser->program, node);
}

/** Parses and returns a statement or expression-statement. */
static Node *statement(Parser *parser) {
    Node *node;
    switch (ADVANCE_PEEK(parser).type) {
    default:
        RETREAT(parser);
        node = expression_stmt(parser);
        break;
    }
    return node;
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
        case TOKEN_RCURLY:
        case TOKEN_SEMICOLON:
            parser->isPanicking = false;
            ADVANCE(parser);
            return;
            
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
        case TOKEN_BREAK_KW:
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

/** Parses and returns a declaration or statement. */
static Node *declaration(Parser *parser) {
    Node *node;
    switch (ADVANCE_PEEK(parser).type) {
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
 * Returns whether or not the parsing was successful.
 */
bool parse(Parser *parser) {
    while (!IS_EOF(parser)) {
        APPEND_DA(&parser->ast, declaration(parser));
    }
    APPEND_DA(&parser->ast, new_eof_node(parser->program, PEEK(parser).pos));

    return !parser->program->hasErrored;
}
