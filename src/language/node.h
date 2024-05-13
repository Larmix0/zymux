#ifndef NODE_H
#define NODE_H

#include "data_structures.h"
#include "token.h"
#include "program.h"

typedef struct AstNode AstNode; // So an AST node can have other AST nodes inside it.

/** An enum to represent different types of AST nodes. */
typedef enum {
    AST_ERROR,
    AST_LITERAL,
    AST_STRING,
    AST_KEYWORD,
    AST_UNARY,
    AST_BINARY,
    AST_EXPR_STMT,
    AST_EOF
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
 * Holds the series of nodes that represent the full string, formatted or not.
 * 
 * Since a string at lexing is a series alternating between string literal nodes and an expr node
 * representing an expression (for f-strings), we'll simply hold an array of nodes
 * alternating between a string literal and an expression too at parse time.
 * 
 * its position is considered the first node in exprs.
 */
typedef struct {
    AstNode node;
    NodeArray exprs;
} StringNode;

/** Represents a keyword that is parsed as alone like true, false, null, super, etc. */
typedef struct {
    AstNode node;
    TokenType keyword;
    SourcePosition pos;
} KeywordNode;

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

/**
 * An expression statement is a statement that just holds an expression followed by a semicolon.
 * 
 * This is needed so the compiler knows to throw some extra opcodes after (like a pop).
 */
typedef struct {
    AstNode node;
    AstNode *expr;
} ExprStmtNode;

/** Node that simply represents EOF and holds the its position. */
typedef struct {
    AstNode node;
    SourcePosition pos;
} EofNode;

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
AstNode *new_error_node(ZmxProgram *program);

/** Returns a string node: An array of nodes that alternate between string literals and exprs. */
AstNode *new_string_node(ZmxProgram *program, NodeArray exprs);

/** Allocates a new literal node which just holds the literal value. */
AstNode *new_literal_node(ZmxProgram *program, Token value);

/** Returns a keyword node that is denoted by the token type. */
AstNode *new_keyword_node(ZmxProgram *program, Token keyword);

/** Allocates a unary node, which is something that has one operation done on it. */
AstNode *new_unary_node(ZmxProgram *program, Token operation, AstNode *rhs);

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
AstNode *new_binary_node(ZmxProgram *program, AstNode *lhs, Token operation, AstNode *rhs);

/** Allocates an expression statement node, which just holds an expression. */
AstNode *new_expr_stmt_node(ZmxProgram *program, AstNode *expr);

/** Returns a node which holds the position an EOF token. */
AstNode *new_eof_node(ZmxProgram *program, SourcePosition eofPos);

/** Returns the source position of an arbitrary node. */
SourcePosition get_node_pos(AstNode *node);

/** Frees all the nodes allocated in the program. */
void free_all_nodes(ZmxProgram *program);

#endif
