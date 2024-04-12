#include <stdlib.h>

#include "data_structures.h"
#include "parser.h"

/** Converts a node to type. */
#define AS_NODE(node, type) ((type *)node)

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
        .ast = CREATE_DA(), .program = program,
        .tokens = tokens, .current = tokens.data
    };
    return parser;
}

/** Frees all the memory the passed parser owns. */
void free_parser(Parser *parser) {
    FREE_DA(&parser->ast);
}

/** Allocates a new literal node which just holds the literal value token. */
static AstNode *new_literal_node(ZmxProgram *program, Token value) {
    LiteralNode *literal = NEW_NODE(program, AST_LITERAL, LiteralNode);
    literal->value = value;
    return AS_NODE(literal, AstNode);
}

/** Returns an erroneous node which holds the invalid token and a message. */
static AstNode *new_error_node(ZmxProgram *program, Token erroredToken, char *message) {
    ErrorNode *error = NEW_NODE(program, AST_ERROR, ErrorNode);
    error->erroredToken = erroredToken;
    error->message = message;
    return AS_NODE(error, AstNode);
}

/** 
 * Parses and returns a primary.
 * 
 * The highest priority precedence. Includes basic things like integer literals and identifiers.
 */
static AstNode *primary(Parser *parser) {
    switch (parser->current->type) {
    case TOKEN_INT_LIT: return new_literal_node(parser->program, *parser->current);
    default: return new_error_node(parser->program, *parser->current, "Invalid expression.");
    }
}

/** Parses and returns an expression. */
static AstNode *expression(Parser *parser) {
    return primary(parser);
}

/**
 * Parses the array of tokens in the parser.
 * Places the parsed nodes inside the passed parser's ast field.
 */
bool parse(Parser *parser) {
    APPEND_DA(&parser->ast, expression(parser));
    return true;
}
