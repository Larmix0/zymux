#include <stdlib.h>

#include "constants.h"
#include "node.h"
#include "program.h"

/** Allocates a new node of actualType. */
#define NEW_NODE(program, astType, actualType) \
    ((actualType *)new_node(program, astType, sizeof(actualType)))

/** Allocates a new node of the passed type. */
static AstNode *new_node(ZmxProgram *program, const AstType type, const size_t size) {
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


/** Returns a keyword node that is denoted by the token type. */
AstNode *new_keyword_node(ZmxProgram *program, Token keyword) {
    KeywordNode *node = NEW_NODE(program, AST_KEYWORD, KeywordNode);
    node->keyword = keyword.type;
    node->pos = keyword.pos;
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

/** Allocates an expression statement node, which just holds an expression. */
AstNode *new_expr_stmt_node(ZmxProgram *program, AstNode *expr) {
    ExprStmtNode *node = NEW_NODE(program, AST_EXPR_STMT, ExprStmtNode);
    node->expr = expr;
    return AS_PTR(node, AstNode);
}


/** Returns a node which holds the position an EOF token. */
AstNode *new_eof_node(ZmxProgram *program, SourcePosition eofPos) {
    EofNode *node = NEW_NODE(program, AST_EOF, EofNode);
    node->pos = eofPos;
    return AS_PTR(node, AstNode);
}

/** 
 * Returns the source position of an arbitrary node.
 * 
 * This function becomes recursive when getting the lines of a node that doesn't directly have a
 * token inside it.
 */
SourcePosition get_node_pos(AstNode *node) {
    switch (node->type) {
    case AST_ERROR: return create_src_pos(0, 0, 0);
    case AST_LITERAL: return AS_PTR(node, LiteralNode)->value.pos;
    case AST_KEYWORD: return AS_PTR(node, KeywordNode)->pos;
    case AST_UNARY: return AS_PTR(node, UnaryNode)->operation.pos;
    case AST_BINARY: return AS_PTR(node, BinaryNode)->operation.pos;
    case AST_EXPR_STMT: return get_node_pos(AS_PTR(node, ExprStmtNode)->expr);
    case AST_EOF: return AS_PTR(node, EofNode)->pos;
    
    default: UNREACHABLE_ERROR(); return create_src_pos(0, 0, 0);
    }
}

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
