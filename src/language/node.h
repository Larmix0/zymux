#ifndef NODE_H
#define NODE_H

#include "data_structures.h"
#include "token.h"

/** Allocates a new node of actualType. */
#define NEW_NODE(program, astType, actualType) \
    ((actualType *)new_node(program, astType, sizeof(actualType)))

/** An enum to represent different types of AST nodes. */
typedef enum {
    AST_ERROR,
    AST_LITERAL,
    AST_UNARY,
    AST_BINARY
} AstType;

/** Generic AST node struct for type punning. */
typedef struct AstNode {
    AstNode *next;
    AstType type;
} AstNode;

/** An array of AST nodes. */
DECLARE_DA_STRUCT(NodeArray, AstNode *);

/** An erroneous node. This is mostly just a placeholder for returning after erroring out. */
typedef struct {
    AstNode node;
} ErrorNode;

/** A node which holds one literal value. */
typedef struct {
    AstNode node;
    Token value;
} LiteralNode;

/**
 * A unary with an operation and a node as the RHS.
 * 
 * This is because anything could be RHS in a unary operation. Even a nested unary like (---10).
 */
typedef struct {
    AstNode node;
    Token operation;
    AstNode *rhs;
} UnaryNode;

/** Holds the right and left value of some operation, as well as that operation itself. */
typedef struct {
    AstNode node;
    AstNode *lhs;
    Token operation;
    AstNode *rhs;
} BinaryNode;

/** Frees all the nodes allocated in the program. */
void free_all_nodes(ZmxProgram *program);

/** Allocates a new node of the passed type. Prefer to use the macro over this.*/
AstNode *new_node(ZmxProgram *program, const AstType type, const size_t size);

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
AstNode *new_error_node(ZmxProgram *program);

/** Allocates a new literal node which just holds the literal value. */
AstNode *new_literal_node(ZmxProgram *program, Token value);

/** Allocates a unary node, which is something that has one operation done on it. */
AstNode *new_unary_node(ZmxProgram *program, Token operation, AstNode *rhs);

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
AstNode *new_binary_node(ZmxProgram *program, AstNode *lhs, Token operation, AstNode *rhs);

#endif
