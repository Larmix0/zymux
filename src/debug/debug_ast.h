#ifndef DEBUG_AST_H
#define DEBUG_AST_H

#include "char_buffer.h"
#include "node.h"

/** A delimiter instead of newlines when grabbing the char buffer from the AST debugger. */
#define AST_DEBUG_DELIMITER "|-|"

/** Prints the passed ast to the console. */
void print_ast(const NodeArray *ast);

/** Allocates the string representation of the passed ast in a char buffer. */
CharBuffer get_ast_string(const NodeArray *ast);

#endif
