#ifndef DEBUG_BYTECODE_H
#define DEBUG_BYTECODE_H

#include "object.h"

/**
 * Prints all of the bytecode stored inside function.
 * 
 * It will not print the line of each bytecode instruction if the function's
 * source positions array is empty, which should be the case if it was compiled without
 * debugging information set to true on the compiler.
 */
void print_bytecode(FuncObj *function);

#endif
