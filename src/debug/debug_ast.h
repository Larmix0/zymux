#ifndef DEBUG_AST_H
#define DEBUG_AST_H

#include "parser.h"

/** Prints the passed ast to the console. */
void print_ast(NodeArray *ast);

/** Allocates the string representation of the passed ast in a char buffer */
CharBuffer allocate_ast_string(NodeArray *ast);

#endif
