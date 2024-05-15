#ifndef DEBUG_RUNTIME_H
#define DEBUG_RUNTIME_H

#include "object.h"

/** Prints the current instruction and the stack */
void print_runtime_state(
    FuncObj *func, Obj **stack, const int stackLength, const u32 bytecodeOffset, InstrSize size
);

#endif
