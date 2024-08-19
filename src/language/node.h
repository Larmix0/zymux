#ifndef NODE_H
#define NODE_H

#include <stdbool.h>

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
    AST_PARENTHESES,
    AST_RANGE,
    AST_CALL,
    AST_EXPR_STMT,
    AST_BLOCK,
    AST_VAR_DECL,
    AST_VAR_ASSIGN,
    AST_VAR_GET,
    AST_IF_ELSE,
    AST_WHILE,
    AST_DO_WHILE,
    AST_FOR,
    AST_LOOP_CONTROL,
    AST_FUNC,
    AST_RETURN,
    AST_EOF
} AstType;

/** The type of scope a resolved name is under. */
typedef enum {
    NAME_BUILT_IN,
    NAME_GLOBAL,
    NAME_LOCAL,
    NAME_CLOSURE,
    NAME_UNRESOLVED
} NameScope;

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

/** Holds an expression in parentheses in order to change precedence. */
typedef struct {
    Node node;
    Node *expr;
} ParenthesesNode;

/** 
 * Holds a number range with optional steps in the range.
 * The numbers are stored as expression nodes which should later resolve to integers.
 */
typedef struct {
    Node node;
    Node *start; /** Where the range starts. */
    Node *end; /** Where the range ends. */
    Node *step; /** How many numbers change per step/iteration. */
    SourcePosition pos; /** The location of the range in source code. */
} RangeNode;

/** Wraps something in a call to it. */
typedef struct {
    Node node;
    Node *callee;
    NodeArray args;
} CallNode;

/**
 * An expression statement is a statement that just holds an expression followed by a semicolon.
 * 
 * This is needed so the compiler knows to throw some extra opcodes after (like a pop).
 */
typedef struct {
    Node node;
    Node *expr;
} ExprStmtNode;

/** A block, which holds multiple statements/declarations. */
typedef struct {
    Node node;
    NodeArray stmts;
    SourcePosition pos;

    i64 varsAmount; /** How many variables are declared inside this specific block. */
} BlockNode;

/** Holds a variable declaration statement */
typedef struct {
    Node node;
    Token name;
    Node *value;
    bool isConst;

    NameScope varScope; /** The type of scope which we're declaring this variable on. */
} VarDeclNode;

/** An assignment expression used to change the value of an already declared variable. */
typedef struct {
    Node node;
    Token name;
    Node *value;

    i64 assignIndex; /** For the compiler if varScope was to emit an assign on some array. */
    NameScope varScope; /** What type of scope the assignment should be done to. */
} VarAssignNode;

/** Represents an expression that attempts to get the value of a specific variable. */
typedef struct {
    Node node;
    Token name;

    i64 getIndex; /** For the compiler if varScope was to emit an assign on some array. */
    NameScope varScope; /** What type of scope the assignment should be done to. */
} VarGetNode;

/** Represents a full conditional if-else statement in a node where the else part is optional. */
typedef struct {
    Node node;
    Node *condition;
    BlockNode *ifBranch;
    Node *elseBranch; /** Could be a block or another if-else. */
} IfElseNode;

/** A while loop which executes a block repeatedly while its condition evaluates to true. */
typedef struct {
    Node node;
    Node *condition;
    BlockNode *body;
} WhileNode;

/** 
 * A do while loop. Same as a normal while loop, but the condition check is at the bottom.
 * We could reuse the while node and add an "isDo" bool, but that would make the node larger.
 */
typedef struct {
    Node node;
    Node *condition;
    BlockNode *body;
} DoWhileNode;

/** A for in loop, which executes until we fully iterated through the loop's iterable. */
typedef struct {
    Node node;
    Token loopVar;
    Node *iterable; /** An expression which should result in an iterable object at runtime. */
    BlockNode *body;
} ForNode;

/** 
 * Represents a loop control statement (like break or continue).
 * 
 * Differs from the "keyword" node because it has to store extra information regarding skipping
 * the loop or an iteration after the resolution phase
 * (how many variables to pop out of scope after break or continue).
 */
typedef struct {
    Node node;
    TokenType keyword;
    SourcePosition pos;
    
    /** Amount of variables declared inside the loop block before this node is encountered. */
    u32 loopVarsAmount;
} LoopControlNode;

/** Holds some form of a function (normal function, method, initializer, etc.). */
typedef struct {
    Node node;
    VarDeclNode *nameDecl;
    NodeArray params;
    BlockNode *body;
} FuncNode;

/** For exiting a function with a certain value (null by default if nothing is provided). */
typedef struct {
    Node node;
    Node *returnValue;
} ReturnNode;

/** Node that simply represents EOF and holds its position. */
typedef struct {
    Node node;
    SourcePosition pos;
} EofNode;

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
Node *new_error_node(ZmxProgram *program);

/** Returns a string node: An array of nodes that alternate between string literals and exprs. */
Node *new_string_node(ZmxProgram *program, const NodeArray exprs);

/** Returns a boolean of whether or not the string node starts with an literal. */
bool string_node_starts_with_literal(const StringNode *node);

/** Allocates a new literal node which just holds the literal value. */
Node *new_literal_node(ZmxProgram *program, const Token value);

/** Returns a keyword node that is denoted by the token type. */
Node *new_keyword_node(ZmxProgram *program, const Token keyword);

/** Allocates a unary node, which is something that has one operation done on it. */
Node *new_unary_node(ZmxProgram *program, const Token operation, Node *rhs);

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
Node *new_binary_node(ZmxProgram *program, Node *lhs, const Token operation, Node *rhs);

/** Allocates a parentheses node, which wraps an expression inside it to dictate precedence. */
Node *new_parentheses_node(ZmxProgram *program, Node *expr);

/** Allocates a number range node, which serves as an iterable over numbers. */
Node *new_range_node(
    ZmxProgram *program, Node *start, Node *end, Node *step, const SourcePosition pos
);

/** Allocates a call node, which simply wraps around a callee expression with some optional args. */
Node *new_call_node(ZmxProgram *program, Node *callee, const NodeArray args);

/** Allocates an expression statement node, which just holds an expression. */
Node *new_expr_stmt_node(ZmxProgram *program, Node *expr);

/** ALlocates a block that holds multiple statements/declarations. */
Node *new_block_node(ZmxProgram *program, const NodeArray stmts, const SourcePosition pos);

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

/** Allocates an if-else node with their condition. The else branch can optionally be NULL. */
Node *new_if_else_node(ZmxProgram *program, Node *condition, BlockNode *ifBranch, Node *elseBranch);

/** Allocates a while loop node. */
Node *new_while_node(ZmxProgram *program, Node *condition, BlockNode *body);

/** Allocates a do while loop node. */
Node *new_do_while_node(ZmxProgram *program, Node *condition, BlockNode *body);

/** Allocates a for loop node. */
Node *new_for_node(ZmxProgram *program, const Token loopVar, Node *iterable, BlockNode *body);

/** Allocates a node for some loop control statement (break or continue). */
Node *new_loop_control_node(ZmxProgram *program, const Token keyword);

/** Allocates a general node for any type of function written from the user. */
Node *new_func_node(
    ZmxProgram *program, VarDeclNode *nameDecl, const NodeArray params, BlockNode *body
);

/** Allocates a return node, which exits a functino with a specific object/value. */
Node *new_return_node(ZmxProgram *program, Node *returnValue);

/** Allocates a node which holds the position an EOF token. */
Node *new_eof_node(ZmxProgram *program, const SourcePosition eofPos);

/** Returns the source position of an arbitrary node. */
SourcePosition get_node_pos(const Node *node);

/** Frees all the nodes allocated in the program. */
void free_all_nodes(ZmxProgram *program);

#endif
