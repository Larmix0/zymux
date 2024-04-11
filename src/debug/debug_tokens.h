#ifndef DEBUG_TOKENS_H
#define DEBUG_TOKENS_H

#include "lexer.h"

/** Returns the passed token type as a string literal in all uppercase. */
char *type_to_string(const TokenType type);

/** Prints the passed token array's tokens. */
void print_tokens(const TokenArray tokens);

#endif
