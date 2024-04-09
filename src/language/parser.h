#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "zymux_program.h"

typedef enum {
    AST_ERROR,
    AST_LITERAL,
} AstType;

typedef struct AstNode {
    AstNode *next;
    AstType type;
} AstNode;

typedef struct {
    int length;
    int capacity;
    AstNode **nodes;
} NodeArray;

typedef struct {
    AstNode node;
    Token erroredToken;
    char *message;
} ErrorNode;

typedef struct {
    AstNode node;
    Token value;
} LiteralNode;

typedef struct {
    ZymuxProgram *program;
    TokenArray tokens;
    Token *current;
    NodeArray ast;
} Parser;

/** Returns an initialized parser with the passed tokens and the program. */
Parser create_parser(ZymuxProgram *program, TokenArray tokens);

/** Frees all the memory the parser owns. */
void free_parser(Parser *parser);

/** Parses the array of tokens in the passed parser. */
bool parse(Parser *parser);

#endif
