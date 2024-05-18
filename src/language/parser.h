#ifndef PARSER_H
#define PARSER_H

#include "node.h"
#include "program.h"
#include "token.h"

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
    ZmxProgram *program; /** Information about the Zymux program. */
    TokenArray tokens; /** Lexed tokens we're parsing. */
    Token *current; /** The token we're currently parsing on. */
    Token *syncSpot; /** Pointer to the token we should start syncing from in case of an error. */
    NodeArray ast; /** The array of nested nodes that were parsed. */
} Parser;

/** Returns an initialized parser with the passed tokens and the program. */
Parser create_parser(ZmxProgram *program, const TokenArray tokens);

/** Frees all the memory the parser owns. */
void free_parser(Parser *parser);

/** Parses the array of tokens in the passed parser. */
bool parse(Parser *parser);

#endif
