#ifndef DEBUG_RUNTIME_H
#define DEBUG_RUNTIME_H

#include "emitter.h"
#include "object.h"

/** Prints the current instruction and the stack */
void print_runtime_state(
    const FuncObj *func, Obj **stack, const int stackLength,
    const u32 bytecodeOffset, InstrSize size
);

/** Prints information about a caught runtime error, its effects, and the error message . */
void print_caught_runtime_error(
    ZmxProgram *program, FuncObj *setFunc, const u32 poppedLocals, const u32 poppedCaptures,
    const u32 poppedOpenCaptures, const char *errorMessage
);

#endif
