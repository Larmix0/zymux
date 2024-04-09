#include <stdlib.h>

#include "data_structures.h"
#include "parser.h"

#define AS_NODE(node, type) ((type *)node)
#define NEW_NODE(program, astType, actualType) \
    ((actualType *)new_node(program, astType, sizeof(actualType)))

/** Allocates a new node of the passed type. */
static AstNode *new_node(ZymuxProgram *program, AstType type, const size_t size) {
    AstNode *node = ZMX_ALLOC_BYTES(size);
    node->type = type;    

    node->next = program->allNodes;
    program->allNodes = node;
    return node;
}

/** Returns an initialized NodeArray. */
static NodeArray create_node_array() {
    NodeArray nodes = {.capacity = 0, .length = 0, .nodes = NULL};
    return nodes;
}

static void append_node(NodeArray *nodes, AstNode *node) {
    APPEND_DA(nodes->nodes, nodes->length, nodes->capacity, node, AstNode);
}

/** Frees the memory of the passed array of nodes. Doesn't free the individual nodes themselves. */
static void free_node_array(NodeArray *nodes) {
    free(nodes->nodes);
}

/** Returns an initialized Parser. */
Parser create_parser(ZymuxProgram *program, TokenArray tokens) {
    Parser parser = {
        .ast = create_node_array(), .program = program,
        .tokens = tokens, .current = tokens.tokens
    };
    return parser;
}

/** Frees all the memory the passed parser owns. */
void free_parser(Parser *parser) {
    free_node_array(&parser->ast);
}

AstNode *new_literal_node(ZymuxProgram *program, Token value) {
    LiteralNode *literal = NEW_NODE(program, AST_LITERAL, LiteralNode);
    literal->value = value;
    return AS_NODE(literal, AstNode);
}

AstNode *new_error_node(ZymuxProgram *program, Token erroredToken, char *message) {
    ErrorNode *error = NEW_NODE(program, AST_ERROR, ErrorNode);
    error->erroredToken = erroredToken;
    error->message = message;
    return AS_NODE(error, AstNode);
}

AstNode *primary(Parser *parser) {
    switch (parser->current->type) {
    case TOKEN_INT_LIT: return new_literal_node(parser->program, *parser->current);
    default: return new_error_node(parser->program, *parser->current, "Invalid expression.");
    }
}

AstNode *expression(Parser *parser) {
    return primary(parser);
}

/** Parses the array of tokens in the parser. */
bool parse(Parser *parser) {
    append_node(&parser->ast, expression(parser));
    return true;
}
