#include <stdlib.h>

#include "data_structures.h"
#include "parser.h"
#include "report_error.h"

// All macros which change a parser's current token stop changing if we're on panic mode.

#define CHECK(parser, expected) (PEEK((parser)).type == (expected))
#define PEEK(parser) (*(parser)->current)
#define PEEK_PREVIOUS(parser) (*((parser)->current - 1))

#define ADVANCE_PEEK(parser) (!(parser)->isPanicking ? *(parser)->current++ : *(parser)->current)
#define ADVANCE(parser) (!(parser)->isPanicking ? (parser)->current++ : (parser)->current)
#define ADVANCE_AMOUNT(parser, amount) \
    (!(parser)->isPanicking ? (parser)->current += (amount) : (parser)->current)
#define RETREAT(parser) (!(parser)->isPanicking ? (parser)->current-- : (parser)->current)

#define MATCH(parser, expected) \
    (!(parser)->isPanicking && CHECK((parser), (expected)) ? (ADVANCE((parser)), true) : false)
#define CONSUME(parser, expected, message) \
    (CHECK((parser), (expected)) \
        ? ADVANCE_PEEK((parser)) \
        : (raise_parser_error_missing((parser), PEEK_PREVIOUS((parser)), (message)), PEEK(parser)))

#define IS_EOF(parser) (CHECK((parser), TOKEN_EOF))

/** Allocates a new node of actualType. */
#define NEW_NODE(program, astType, actualType) \
    ((actualType *)new_node(program, astType, sizeof(actualType)))

/** Allocates a new node of the passed type. */
static AstNode *new_node(ZmxProgram *program, AstType type, const size_t size) {
    AstNode *node = ZMX_ALLOC(size);
    node->type = type;    

    node->next = program->allNodes;
    program->allNodes = node;
    return node;
}

/** Returns an initialized parser. */
Parser create_parser(ZmxProgram *program, TokenArray tokens) {
    Parser parser = {
        .isPanicking = false,
        .ast = CREATE_DA(), .program = program,
        .tokens = tokens, .current = tokens.data
    };
    return parser;
}

/** Frees all the memory the passed parser owns. */
void free_parser(Parser *parser) {
    FREE_DA(&parser->ast);
}

/** Reports a parsing error on a specific, erroneous token, then starts panicking. */
static void raise_parser_error_at(Parser *parser, Token erroredToken, const char *message) {
    if (parser->isPanicking) {
        return;
    }
    parser->isPanicking = true;

    SYNTAX_ERROR(
        parser->program, erroredToken.line, erroredToken.column, erroredToken.length, message
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
static void raise_parser_error_missing(Parser *parser, Token beforeMissing, const char *message) {
    if (parser->isPanicking) {
        return;
    }
    parser->isPanicking = true;

    // Errors the character after the token located before the missing one.
    SYNTAX_ERROR(
        parser->program, beforeMissing.line, beforeMissing.column + beforeMissing.length, 1, message
    );   
}

/** Allocates a new literal node which just holds the literal value token. */
static AstNode *new_literal_node(ZmxProgram *program, Token value) {
    LiteralNode *literal = NEW_NODE(program, AST_LITERAL, LiteralNode);
    literal->value = value;
    return AS_PTR(literal, AstNode);
}

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
static AstNode *new_error_node(ZmxProgram *program) {
    ErrorNode *error = NEW_NODE(program, AST_ERROR, ErrorNode);
    return AS_PTR(error, AstNode);
}

/** 
 * Parses and returns a primary.
 * 
 * The highest priority precedence. Includes basic things like integer literals and identifiers.
 */
static AstNode *primary(Parser *parser) {
    switch (ADVANCE_PEEK(parser).type) {
    case TOKEN_INT_LIT: return new_literal_node(parser->program, PEEK_PREVIOUS(parser));
    default:
        raise_parser_error_at(parser, PEEK_PREVIOUS(parser), "Invalid syntax usage.");
        return new_error_node(parser->program);
    }
}

/** Parses and returns an expression. */
static AstNode *expression(Parser *parser) {
    return primary(parser);
}

/** A statement which only holds an expression inside it. */
static AstNode *expression_stmt(Parser *parser) {
    AstNode *node = expression(parser);
    CONSUME(parser, TOKEN_SEMICOLON, "Expected \";\" after expression.");
    return node;
}

/** Parses and returns a statement or expression-statement. */
static AstNode *statement(Parser *parser) {
    AstNode *node;
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
static AstNode *declaration(Parser *parser) {
    AstNode *node;
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
 * Places the parsed nodes inside the passed parser's ast field.
 */
bool parse(Parser *parser) {
    while (!IS_EOF(parser)) {
        APPEND_DA(&parser->ast, declaration(parser));
    }
    return true;
}
