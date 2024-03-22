#ifndef DEBUG_TOKENS_H
#define DEBUG_TOKENS_H

#include "lexer.h"

char *type_to_string(const TokenType type);
void print_tokens(const TokenArray tokens);

#endif
