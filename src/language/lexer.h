#ifndef LEXER_H
#define LEXER_H

#include "program.h"
#include "token.h"

/** A lexer for a given piece of source code to produce its array of tokens for parsing later. */
typedef struct {
    char *tokenStart;
    int tokenColumn;

    char *current;
    int line;
    int column;

    char *source;
    size_t sourceLength;
    
    ZmxProgram *program;
    TokenArray tokens;
} Lexer;

/** Returns an initialized lexer. */
Lexer create_lexer(ZmxProgram *program, char *source);

/** Frees all the memory the lexer has used (including the tokens it generated). */
void free_lexer(Lexer *lexer);

/** Lexes the passed lexer's source code into its tokens array and returns whether it errored. */
bool lex(Lexer *lexer);

#endif
