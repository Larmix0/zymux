#include <stdlib.h>

#include "allocator.h"
#include "constants.h"
#include "node.h"

/** Allocates a new node of actualType. */
#define NEW_NODE(program, astType, actualType) \
    ((actualType *)new_node(program, astType, sizeof(actualType)))

/** Allocates a new node of the passed type. */
static Node *new_node(ZmxProgram *program, const AstType type, const size_t size) {
    Node *node = ALLOC(size);
    node->type = type;    

    node->next = program->allNodes;
    program->allNodes = node;
    return node;
}

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
Node *new_error_node(ZmxProgram *program) {
    ErrorNode *node = NEW_NODE(program, AST_ERROR, ErrorNode);
    return AS_NODE(node);
}

/** Allocates a new literal node which just holds the literal value. */
Node *new_literal_node(ZmxProgram *program, const Token value) {
    LiteralNode *node = NEW_NODE(program, AST_LITERAL, LiteralNode);
    node->value = value;
    return AS_NODE(node);
}

/** Returns a string node: An array of nodes that alternate between string literals and exprs. */
Node *new_string_node(ZmxProgram *program, const NodeArray exprs) {
    StringNode *node = NEW_NODE(program, AST_STRING, StringNode);
    node->exprs = exprs;
    return AS_NODE(node);
}


/** Returns a keyword node that is denoted by the token type. */
Node *new_keyword_node(ZmxProgram *program, const Token keyword) {
    KeywordNode *node = NEW_NODE(program, AST_KEYWORD, KeywordNode);
    node->keyword = keyword.type;
    node->pos = keyword.pos;
    return AS_NODE(node);
}

/** Allocates a unary node, which is something that has one operation done on it. */
Node *new_unary_node(ZmxProgram *program, const Token operation, Node *rhs) {
    UnaryNode *node = NEW_NODE(program, AST_UNARY, UnaryNode);
    node->operation = operation;
    node->rhs = rhs;
    return AS_NODE(node);
}

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
Node *new_binary_node(ZmxProgram *program, Node *lhs, const Token operation, Node *rhs) {
    BinaryNode *node = NEW_NODE(program, AST_BINARY, BinaryNode);
    node->lhs = lhs;
    node->operation = operation;
    node->rhs = rhs;
    return AS_NODE(node);
}

/** Allocates a parentheses node which wraps an expression inside it to dictate precedence. */
Node *new_parentheses_node(ZmxProgram *program, Node *expr) {
    ParenthesesNode *node = NEW_NODE(program, AST_PARENTHESES, ParenthesesNode);
    node->expr = expr;
    return AS_NODE(node);
}

/** Allocates an expression statement node, which just holds an expression. */
Node *new_expr_stmt_node(ZmxProgram *program, Node *expr) {
    ExprStmtNode *node = NEW_NODE(program, AST_EXPR_STMT, ExprStmtNode);
    node->expr = expr;
    return AS_NODE(node);
}

/** ALlocates a block that holds multiple statements/declarations. */
Node *new_block_node(ZmxProgram *program, const NodeArray stmts) {
    BlockNode *node = NEW_NODE(program, AST_BLOCK, BlockNode);
    node->stmts = stmts;
    return AS_NODE(node);
}

/** 
 * Allocates a variable declaration node.
 * 
 * Variable delcaration is the first variable assignment which also includes constness.
 * It doesn't include whether or not it's private as private is a node that wraps around names.
 */
Node *new_var_decl_node(ZmxProgram *program, const Token name, Node *value, const bool isConst) {
    VarDeclNode *node = NEW_NODE(program, AST_VAR_DECL, VarDeclNode);
    node->name = name;
    node->value = value;
    node->isConst = isConst;
    return AS_NODE(node);
}

/** Allocates an assignment expression, which changes the value of a variable already set. */
Node *new_var_assign_node(ZmxProgram *program, const Token name, Node *value) {
    VarAssignNode *node = NEW_NODE(program, AST_VAR_ASSIGN, VarAssignNode);
    node->name = name;
    node->value = value;
    return AS_NODE(node);
}

/** Allocates a node which holds the name of a variable to get its value. */
Node *new_var_get_node(ZmxProgram *program, const Token name) {
    VarGetNode *node = NEW_NODE(program, AST_VAR_GET, VarGetNode);
    node->name = name;
    return AS_NODE(node);
}

/** Returns a node which holds the position an EOF token. */
Node *new_eof_node(ZmxProgram *program, const SourcePosition eofPos) {
    EofNode *node = NEW_NODE(program, AST_EOF, EofNode);
    node->pos = eofPos;
    return AS_NODE(node);
}

/** 
 * Returns the source position of an arbitrary node.
 * 
 * This function becomes recursive when getting the lines of a node that doesn't directly have a
 * token inside it.
 */
SourcePosition get_node_pos(const Node *node) {
    // TODO: is indexing the first element potentially gonna make us index an empty array?
    switch (node->type) {
    case AST_ERROR: return create_src_pos(0, 0, 0);
    case AST_LITERAL: return AS_PTR(LiteralNode, node)->value.pos;
    case AST_KEYWORD: return AS_PTR(KeywordNode, node)->pos;
    case AST_UNARY: return AS_PTR(UnaryNode, node)->operation.pos;
    case AST_BINARY: return AS_PTR(BinaryNode, node)->operation.pos;
    case AST_STRING: return get_node_pos(AS_PTR(StringNode, node)->exprs.data[0]);
    case AST_PARENTHESES: return get_node_pos(AS_PTR(ParenthesesNode, node)->expr);
    case AST_EXPR_STMT: return get_node_pos(AS_PTR(ExprStmtNode, node)->expr);
    case AST_BLOCK: return get_node_pos(AS_PTR(BlockNode, node)->stmts.data[0]);
    case AST_VAR_DECL: return AS_PTR(VarDeclNode, node)->name.pos;
    case AST_VAR_ASSIGN: return AS_PTR(VarAssignNode, node)->name.pos;
    case AST_VAR_GET: return AS_PTR(VarGetNode, node)->name.pos;
    case AST_EOF: return AS_PTR(EofNode, node)->pos;
    default: UNREACHABLE_ERROR();
    }
}

/** Frees all the contents of the passed node. */
static void free_node_contents(Node *node) {
    switch (node->type) {
    case AST_STRING:
        FREE_DA(&AS_PTR(StringNode, node)->exprs);
        break;
    case AST_BLOCK:
        FREE_DA(&AS_PTR(BlockNode, node)->stmts);
        break;

    default:
        break; // Nothing to free.
    }
}


/** Frees all nodes that were allocated and placed inside the passed program. */
void free_all_nodes(ZmxProgram *program) {
    if (program->allNodes == NULL) {
        return;
    }

    Node *current = program->allNodes;
    Node *next = current->next;
    while (current != NULL) {
        free_node_contents(current);
        free(current);
        current = next;
        if (next != NULL) {
            next = next->next;
        }
    }
}
