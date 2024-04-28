#include <stdlib.h>

#include "constants.h"
#include "node.h"
#include "program.h"

/** Frees all nodes that were allocated and placed inside the passed program. */
void free_all_nodes(ZmxProgram *program) {
    if (program->allNodes == NULL) {
        return;
    }

    AstNode *current = program->allNodes;
    AstNode *next = current->next;
    while (current != NULL) {
        free(current);
        current = next;
        if (next != NULL) {
            next = next->next;
        }
    }
}

/** Allocates a new node of the passed type. */
AstNode *new_node(ZmxProgram *program, const AstType type, const size_t size) {
    AstNode *node = ZMX_ALLOC(size);
    node->type = type;    

    node->next = program->allNodes;
    program->allNodes = node;
    return node;
}

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
AstNode *new_error_node(ZmxProgram *program) {
    ErrorNode *node = NEW_NODE(program, AST_ERROR, ErrorNode);
    return AS_PTR(node, AstNode);
}

/** Allocates a new literal node which just holds the literal value. */
AstNode *new_literal_node(ZmxProgram *program, Token value) {
    LiteralNode *node = NEW_NODE(program, AST_LITERAL, LiteralNode);
    node->value = value;
    return AS_PTR(node, AstNode);
}

/** Allocates a unary node, which is something that has one operation done on it. */
AstNode *new_unary_node(ZmxProgram *program, Token operation, AstNode *rhs) {
    UnaryNode *node = NEW_NODE(program, AST_UNARY, UnaryNode);
    node->operation = operation;
    node->rhs = rhs;
    return AS_PTR(node, AstNode);
}

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
AstNode *new_binary_node(ZmxProgram *program, AstNode *lhs, Token operation, AstNode *rhs) {
    BinaryNode *node = NEW_NODE(program, AST_BINARY, BinaryNode);
    node->lhs = lhs;
    node->operation = operation;
    node->rhs = rhs;
    return AS_PTR(node, AstNode);
}
