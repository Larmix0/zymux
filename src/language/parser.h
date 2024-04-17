#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "program.h"

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

/**
 * The parser, which uses panicking when encountering an error.
 * 
 * Panicking is done by immediately exiting the current statement being parsed,
 * and then we keep skipping tokens until we see one that definitely denotes the end/start
 * of a statement.
 * We go back to normally parsing afterwards.
 */
typedef struct {
    bool isPanicking; /** Denotes whether or not the parser's panicking. */
    ZmxProgram *program;
    TokenArray tokens;
    Token *current;
    NodeArray ast;
} Parser;

/** Returns an initialized parser with the passed tokens and the program. */
Parser create_parser(ZmxProgram *program, TokenArray tokens);

/** Frees all the memory the parser owns. */
void free_parser(Parser *parser);

/** Parses the array of tokens in the passed parser. */
bool parse(Parser *parser);

#endif
