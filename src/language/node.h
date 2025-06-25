#ifndef NODE_H
#define NODE_H

#include <stdbool.h>

#include "dynamic_array.h"
#include "program.h"
#include "token.h"

/** Converts anything that "inherits" from node to its type punning base form. */
#define AS_NODE(node) ((Node *)node)

/** An automatic null node for convenience (as they're all identical). */
#define NULL_NODE(program) (new_keyword_node(program, create_token("null", TOKEN_NULL_KW)))

/** A declaration with only a name, and should not load a value by itself. */
#define NO_VALUE_DECL_NODE(program, name) \
    (AS_PTR(DeclareVarNode, new_declare_var_node(program, name, NULL, false)))

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
    AST_ASSIGN_SUBSCR,
    AST_GET_SUBSCR,
    AST_TERNARY,
    AST_LIST,
    AST_MAP,
    AST_CALL,
    AST_EXPR_STMT,
    AST_BLOCK,
    AST_DECLARE_VAR,
    AST_ASSIGN_VAR,
    AST_GET_VAR,
    AST_SET_PROPERTY,
    AST_GET_PROPERTY,
    AST_GET_SUPER,
    AST_MULTI_DECLARE,
    AST_MULTI_ASSIGN,
    AST_IF_ELSE,
    AST_CASE,
    AST_MATCH,
    AST_WHILE,
    AST_DO_WHILE,
    AST_FOR,
    AST_LOOP_CONTROL,
    AST_ENUM,
    AST_IMPORT,
    AST_FROM_IMPORT,
    AST_TRY_CATCH,
    AST_RAISE,
    AST_RETURN,
    AST_FUNC,
    AST_CLASS,
    AST_EXIT,
    AST_EOF
} AstType;

/** The type of scope a resolved name is under. */
typedef enum {
    VAR_BUILT_IN,
    VAR_GLOBAL,
    VAR_LOCAL,
    VAR_CAPTURED,
    VAR_UNRESOLVED
} VarType;

/** Resolution information for any type of name node (assignment, declaration, etc.). */
typedef struct {
    i64 index;
    VarType scope;
} VarResolution;

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
    SourcePosition pos;
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

/** Sets a value assignment on something that's getting subscripted. */
typedef struct {
    Node node;
    Node *callee;
    Node *subscript;
    Node *value;
} AssignSubscrNode;

/** Gets a subscript, which cuts a part off of some object that supports it. */
typedef struct {
    Node node;
    Node *callee;
    Node *subscript;
} GetSubscrNode;

/** A ternary conditional, syntax sugar for an if-else. */
typedef struct {
    Node node;
    Node *condition;
    Node *trueExpr;
    Node *falseExpr;
} TernaryNode;

/** Represents a list of multiple expressions separated by commas. */
typedef struct {
    Node node;
    NodeArray items;
    SourcePosition pos;
} ListNode;

/** Represents a map of key-value pairs where the key is hashable. */
typedef struct {
    Node node;
    NodeArray keys;
    NodeArray values;
    SourcePosition pos;
} MapNode;

/** Wraps something in a call which has some positional args followed by a map of keyword args. */
typedef struct {
    Node node;
    Node *callee; /** Whatever thing at runtime which is gonna get called. */
    NodeArray positionalArgs; /** Arguments that are supplied in order with no naming. */
    MapNode *keywordArgs; /** Arguments that are tied to an optional argument. After positionals. */
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

    i64 localsAmount; /** How many variables are within the block (excludes captured ones). */
    i64 capturedAmount; /** How many captured variables were declared within this block. */
} BlockNode;

/** Holds a variable declaration statement. Value is a C NULL if it doesn't have an initializer. */
typedef struct {
    Node node;
    Token name;
    Node *value;
    bool isConst;

    VarResolution resolution;
} DeclareVarNode;

/** An assignment expression used to change the value of an already declared variable. */
typedef struct {
    Node node;
    Token name;
    Node *value;

    VarResolution resolution;
} AssignVarNode;

/** Represents an expression that attempts to get the value of a specific variable. */
typedef struct {
    Node node;
    Token name;

    VarResolution resolution;
} GetVarNode;

/** Represents a node which gets a field/method inside some object that has it. */
typedef struct {
    Node node;
    Token property;
    Node *originalObj;
} GetPropertyNode;

/** Sets (declare or assign) a new value to a field/method inside some object. */
typedef struct {
    Node node;
    GetPropertyNode *get;
    Node *value;
} SetPropertyNode;

/** 
 * Represents a "super" keyword with a mandatory "." and property after it.
 * 
 * The reason a property is mandatory after usage of super is that if the keyword simply resolved
 * to the superclass, then it would be calling methods with a class (not an instance),
 * which is invalid.
 */
typedef struct {
    Node node;
    Token property; /** The token name of the property that comes after the mandatory ".". */
    GetVarNode *instanceGet; /** The implicit instance variable get to find the superclass. */
    SourcePosition pos; /** The actual position of the "super" keyword token. */
} GetSuperNode;

/** 
 * Declares multiple variables at once (which are supposed to have not been declared beforehand).
 * 
 * They're set to elements in an iterable value, and if the value is a C NULL,
 * it will treat it as having no initializer.
 */
typedef struct {
    Node node;
    NodeArray declarations; /** Array variable declarations that don't have explicit values set. */
    Node *value; /** The iterable each one of them is set to one of its elements. */
    bool isConst; /** Applies to all variables whether they're all const or none of them are. */
    SourcePosition pos;
} MultiDeclareNode;

/** 
 * Assigns values to multiple variables using some iterable, which must be a valid, parsable value.
 * The array of assignments allow anything assignable (like variables and fields).
 */
typedef struct {
    Node node;
    NodeArray assignments; /** Array of all assignments (which could be of different types). */
    Node *value; /** The iterable value they're set to. */
    SourcePosition pos;
} MultiAssignNode;

/** Represents a full conditional if-else statement in a node where the else part is optional. */
typedef struct {
    Node node;
    Node *condition;
    BlockNode *ifBlock;
    Node *elseBlock; /** Could be a block or another if-else. */
} IfElseNode;

/** Represents one case inside a match statement. */
typedef struct {
    Node node;
    NodeArray labelVals; /** The label might have multiple values in one given case. */
    BlockNode *block;
    SourcePosition pos;
} CaseNode;

/** A match-case statement, which matches a value to a case value/default if there's one. */
typedef struct {
    Node node;
    Node *matchedExpr; /** The main expression being matched with the cases. */
    NodeArray cases; /** All non-default cases. */
    BlockNode *defaultBlock; /** the "default" block in matches. NULL if unused. */
} MatchNode;

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
    Node *loopVar; /** Could be a normal declaration or a multi-declaration. */
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
    
    i64 localsAmount; /** Amount of locals declared inside the loop block before this node. */
    i64 capturedAmount; /** Amount of declared locals captured within the loop before this node. */
} LoopControlNode;

/** Holds an array of names that correspond to ordered numbers. */
typedef struct {
    Node node;
    DeclareVarNode *nameDecl;
    TokenArray members; /** Array of enumerated member names of the enum. */
} EnumNode;

/** Represents a statement which imports a zymux file and binds its contents to a variable name. */
typedef struct {
    Node node;
    char *path;
    u32 pathLength;
    DeclareVarNode *importVar;
} ImportNode;

/** Represents an import which only tries to grab some specific names from the imported module. */
typedef struct {
    Node node;
    char *path;
    u32 pathLength;
    TokenArray importedNames; /** The names being imported. */
    NodeArray namesAs; /** Their alias in the importing file as an array of declarations. */

    SourcePosition pos;
} FromImportNode;

/** 
 * An error handling statement which goes to the catch block if an error occurs inside the try.
 * The always block simply executes every time, regardless of whether or not the catch was executed.
 */
typedef struct {
    Node node;
    BlockNode *tryBlock;
    BlockNode *catchBlock;
    DeclareVarNode *catchVar; /** Optional caught message at runtime. NULL if omitted. */
} TryCatchNode;

/** Induces a runtime error with the error message it holds. */
typedef struct {
    Node node;
    Node *message;
    SourcePosition pos;
} RaiseNode;

/** For exiting a function with a certain value. Empty return values get resolved later. */
typedef struct {
    Node node;
    Node *value; /** Normal return value parsed (NULL if returning nothing). */
    SourcePosition pos; /** Return keyword position. Very needed especially when value is NULL. */

    Node *defaultVal; /** The default value given to empty returns depending on function type. */
    i64 capturedPops; /** How many captured should be popped when returning. 0 if none. */
} ReturnNode;

/** Holds some form of a function (normal function, method, initializer, etc.). */
typedef struct {
    Node node;
    bool isMethod; /** Whether or not this func belongs to a class, or is a lone function. */
    Token name; /** Just the name of the function that the user wrote in the code. */
    DeclareVarNode *outerDecl; /** Declaration which binds it to a variable after it's loaded. */
    DeclareVarNode *innerDecl; /** Declaration for the func inside itself ("this" in methods). */

    NodeArray mandatoryParams; /** Parameters that don't have a default value. Must be provided. */
    NodeArray optionalParams; /** Parameters that have a default value when not provided. */

    BlockNode *body; /** Block that holds the statements of the function. */
    ReturnNode *defaultReturn; /** The return which is auto emitted at the end of this func. */

    bool isClosure; /** Whether or not this'll need to be treated as a closure at runtime. */
    U32Array capturedParams; /** An array of each index into the params array that is captured. */
} FuncNode;

/** Holds a Zymux class with all of its information. */
typedef struct {
    Node node;
    bool isAbstract;
    NodeArray abstractMethods; /** Filled in abstract classes. Name decls (must be overridden). */
    
    DeclareVarNode *nameDecl;
    GetVarNode *superclass; /** NULL if the class doesn't inherit anything. */
    FuncNode *init; /** NULL if there isn't an initializer. */
    NodeArray methods;
} ClassNode;

/** Node that exits the program immediately with an exit code. */
typedef struct {
    Node node;
    Node *exitCode; /** Expression for the exit code (should resolve to integer at runtime). */
    SourcePosition pos;
} ExitNode;

/** Node that simply represents EOF and holds its position. */
typedef struct {
    Node node;
    SourcePosition pos;
} EofNode;

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
Node *new_error_node(ZmxProgram *program);

/** Returns a string node: An array of nodes that alternate between string literals and exprs. */
Node *new_string_node(ZmxProgram *program, const NodeArray exprs, const SourcePosition pos);

/** Returns a boolean of whether or not the string node starts with a literal. */
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

/** Allocates a subscript assignment, which assigns to some subscripted thing. */
Node *new_assign_subscr_node(ZmxProgram *program, Node *callee, Node *subscript, Node *value);

/** Allocates a subscript get, which grabs a part of the thing it's being used on. */
Node *new_get_subscr_node(ZmxProgram *program, Node *callee, Node *subscript);

/** Allocates a ternary conditional expression node. */
Node *new_ternary_node(ZmxProgram *program, Node *condition, Node *trueExpr, Node *falseExpr);

/** Allocates a list node to hold an array of multiple nodes. */
Node *new_list_node(ZmxProgram *program, const NodeArray items, const SourcePosition pos);

/** Allocates a map of key-value pairs represented in 2 equally long arrays. */
Node *new_map_node(
    ZmxProgram *program, const NodeArray keys, const NodeArray values, const SourcePosition pos
);

/** Allocates a call node, which calls a callee with some positional, then keyword args. */
Node *new_call_node(
    ZmxProgram *program, Node *callee, const NodeArray positionalArgs, MapNode *keywordArgs
);

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
Node *new_declare_var_node(ZmxProgram *program, const Token name, Node *value, const bool isConst);

/** Allocates an assignment expression, which changes the value of a variable already set. */
Node *new_assign_var_node(ZmxProgram *program, const Token name, Node *value);

/** Allocates a node which holds the name of a variable to get its value. */
Node *new_get_var_node(ZmxProgram *program, const Token name);

/** Allocates a node which declares/assigns a property in some object. */
Node *new_set_property_node(ZmxProgram *program, GetPropertyNode *get, Node *value);

/** Returns a "super" keyword usage with a property that's gotten from the superclass. */
Node *new_get_super_node(
    ZmxProgram *program, const Token property, GetVarNode *instanceGet, const SourcePosition pos
);

/** Allocates a node which holds a property access into some object. */
Node *new_get_property_node(ZmxProgram *program, const Token property, Node *originalObj);

/** Allocates a node for declaring multiple variables in one declarative statement. */
Node *new_multi_declare_node(
    ZmxProgram *program, const NodeArray declarations, Node *value,
    const bool isConst, const SourcePosition pos
);

/** Allocates a node for multiple assignments on an iterable value. */
Node *new_multi_assign_node(
    ZmxProgram *program, const NodeArray assignments, Node *value, const SourcePosition pos
);

/** Allocates an if-else node with their condition. The else branch can optionally be NULL. */
Node *new_if_else_node(ZmxProgram *program, Node *condition, BlockNode *ifBlock, Node *elseBlock);

/** Allocates one new case for some match statement. */
Node *new_case_node(
    ZmxProgram *program, const NodeArray labelVals, BlockNode *block, const SourcePosition pos
);

/** Allocates a new match-case statement, with an optional default case. */
Node *new_match_node(
    ZmxProgram *program, Node *matchedExpr, const NodeArray cases, BlockNode *defaultBlock
);

/** Allocates a while loop node. */
Node *new_while_node(ZmxProgram *program, Node *condition, BlockNode *body);

/** Allocates a do while loop node. */
Node *new_do_while_node(ZmxProgram *program, Node *condition, BlockNode *body);

/** Allocates a for loop node. */
Node *new_for_node(ZmxProgram *program, Node *loopVar, Node *iterable, BlockNode *body);

/** Allocates a node for some loop control statement (break or continue). */
Node *new_loop_control_node(ZmxProgram *program, const Token keyword);

/** Allocates an enum with members that represent a text/readable number. */
Node *new_enum_node(ZmxProgram *program, DeclareVarNode *nameDecl, const TokenArray members);

/** 
 * Allocates a statement which imports some file and binds its contents to a variable.
 * 
 * Makese a copy of the passed path, therefore freeing the passed string isn't its responsibility.
 */
Node *new_import_node(
    ZmxProgram *program, char *path, const u32 pathLength, DeclareVarNode *importVar
);

/** 
 * Allocates an import which only declares an array of names from the imported module.
 * 
 * Makese a copy of the passed path, therefore freeing the passed string isn't its responsibility.
 */
Node *new_from_import_node(
    ZmxProgram *program, char *path, const u32 pathLength,
    const TokenArray importedNames, const NodeArray namesAs, const SourcePosition pos
);

/** Allocates a statement which doesn't immediately exit and print a message on error. */
Node *new_try_catch_node(
    ZmxProgram *program, BlockNode *tryBlock, BlockNode *catchBlock, DeclareVarNode *catchVar
);

/** Allocates a node which will try to raise an error at runtime. */
Node *new_raise_node(ZmxProgram *program, Node *message, const SourcePosition pos);

/** Allocates a return node, which exits a function with a specific object/value. */
Node *new_return_node(ZmxProgram *program, Node *value, const SourcePosition pos);

/** Allocates a general node for any type of function written from the user. */
Node *new_func_node(
    ZmxProgram *program, const Token name, DeclareVarNode *outerDecl, DeclareVarNode *innerDecl,
    const bool isMethod, const NodeArray mandatoryParams, const NodeArray optionalParams,
    BlockNode *body
);

/** 
 * Allocates a node which represents a class and all of its information.
 * 
 * Note that this only takes information before the body of the class as arguments,
 * while the information inside the body should be added to the node whilst parsing for convenience
 * (and not having to create too many variables).
 */
Node *new_class_node(ZmxProgram *program, DeclareVarNode *nameDecl, const bool isAbstract);

/** Allocates a node that instantly exits the program with an exit code. */
Node *new_exit_node(ZmxProgram *program, Node *exitCode, const SourcePosition pos);

/** Allocates a node which holds the position an EOF token. */
Node *new_eof_node(ZmxProgram *program, const SourcePosition eofPos);

/** Returns the source position of an arbitrary node. */
SourcePosition get_node_pos(const Node *node);

/** Frees all the nodes allocated in the program. */
void free_all_nodes(ZmxProgram *program);

#endif
