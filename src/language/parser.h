#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "program.h"

typedef enum {
    AST_ERROR,
    AST_LITERAL,
} AstType;

typedef struct AstNode {
    AstNode *next;
    AstType type;
} AstNode;

DECLARE_DA_STRUCT(NodeArray, AstNode *);

typedef struct {
    AstNode node;
} ErrorNode;

typedef struct {
    AstNode node;
    Token value;
} LiteralNode;

typedef struct {
    bool isPanicking;

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
