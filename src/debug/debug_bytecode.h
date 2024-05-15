#ifndef DEBUG_BYTECODE_H
#define DEBUG_BYTECODE_H

#include "object.h"
#include "emitter.h"

typedef enum {
    INSTR_NO_LINE_OR_PAD,
    INSTR_NO_LINE,
    INSTR_NORMAL
} InstrFormat;

/** Prints a single instruction depending on format, and modifies idx's value accordingly. */
u32 print_instr(FuncObj *func, u32 idx, InstrSize *size, InstrFormat format);

/**
 * Prints all of the bytecode stored inside function.
 * 
 * It will not print the line of each bytecode instruction if the function's
 * source positions array is empty, which should be the case if it was compiled without
 * debugging information set to true on the compiler.
 */
void print_bytecode(FuncObj *function);

#endif
