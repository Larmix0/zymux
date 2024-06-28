#ifndef NODE_H
#define NODE_H

#include "dynamic_array.h"
#include "program.h"
#include "token.h"

/** Converts anything that "inherits" from node to its type punning base form. */
#define AS_NODE(node) ((Node *)node)

/** An enum to represent different types of AST nodes. */
typedef enum {
    AST_ERROR,
    AST_LITERAL,
    AST_STRING,
    AST_KEYWORD,
    AST_UNARY,
    AST_BINARY,
    AST_EXPR_STMT,
    AST_VAR_DECL,
    AST_VAR_ASSIGN,
    AST_VAR_GET,
    AST_EOF
} AstType;

/** Generic AST node struct for type punning. */
typedef struct Node {
    Node *next;
    AstType type;
} Node;

/** An array of AST nodes. */
DECLARE_DA_STRUCT(NodeArray, Node *);

/** An erroneous node. This is mostly just a placeholder for returning after erroring out. */
typedef struct {
    Node node;
} ErrorNode;

/** A node which holds one literal value. */
typedef struct {
    Node node;
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
    Node node;
    NodeArray exprs;
} StringNode;

/** Represents a keyword that is parsed as alone like true, false, null, super, etc. */
typedef struct {
    Node node;
    TokenType keyword;
    SourcePosition pos;
} KeywordNode;

/**
 * A unary with an operation and a node as the RHS.
 * 
 * This is because anything could be RHS in a unary operation. Even a nested unary like (---10).
 */
typedef struct {
    Node node;
    Token operation;
    Node *rhs;
} UnaryNode;

/** Holds the right and left value of some operation, as well as that operation itself. */
typedef struct {
    Node node;
    Node *lhs;
    Token operation;
    Node *rhs;
} BinaryNode;

/**
 * An expression statement is a statement that just holds an expression followed by a semicolon.
 * 
 * This is needed so the compiler knows to throw some extra opcodes after (like a pop).
 */
typedef struct {
    Node node;
    Node *expr;
} ExprStmtNode;

/** Holds a variable declaration statement */
typedef struct {
    Node node;
    Token name;
    Node *value;
    bool isConst;
} VarDeclNode;

/** An assignment expression used to change the value of an already declared variable. */
typedef struct {
    Node node;
    Token name;
    Node *value;
} VarAssignNode;

/** Represents an expression that attempts to get the value of a specific variable. */
typedef struct {
    Node node;
    Token name;
} VarGetNode;

/** Node that simply represents EOF and holds the its position. */
typedef struct {
    Node node;
    SourcePosition pos;
} EofNode;

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
Node *new_error_node(ZmxProgram *program);

/** Returns a string node: An array of nodes that alternate between string literals and exprs. */
Node *new_string_node(ZmxProgram *program, const NodeArray exprs);

/** Allocates a new literal node which just holds the literal value. */
Node *new_literal_node(ZmxProgram *program, const Token value);

/** Returns a keyword node that is denoted by the token type. */
Node *new_keyword_node(ZmxProgram *program, const Token keyword);

/** Allocates a unary node, which is something that has one operation done on it. */
Node *new_unary_node(ZmxProgram *program, const Token operation, Node *rhs);

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
Node *new_binary_node(ZmxProgram *program, Node *lhs, const Token operation, Node *rhs);

/** Allocates an expression statement node, which just holds an expression. */
Node *new_expr_stmt_node(ZmxProgram *program, Node *expr);

/** 
 * Allocates a variable declaration node.
 * 
 * Variable delcaration is the first variable assignment which also includes constness.
 * It doesn't include whether or not it's private as private is a node that wraps around names.
 */
Node *new_var_decl_node(ZmxProgram *program, const Token name, Node *value, const bool isConst);

/** Allocates an assignment expression, which changes the value of a variable already set. */
Node *new_var_assign_node(ZmxProgram *program, const Token name, Node *value);

/** Allocates a node which holds the name of a variable to get its value. */
Node *new_var_get_node(ZmxProgram *program, const Token name);

/** Allocates a node which holds the position an EOF token. */
Node *new_eof_node(ZmxProgram *program, const SourcePosition eofPos);

/** Returns the source position of an arbitrary node. */
SourcePosition get_node_pos(const Node *node);

/** Frees all the nodes allocated in the program. */
void free_all_nodes(ZmxProgram *program);

#endif
