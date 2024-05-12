#ifndef EMITTER_H
#define EMITTER_H

#include "constants.h"
#include "data_structures.h"
#include "object.h"
#include "report_error.h"

typedef struct Compiler Compiler;

/** 
 * Represents each opcode in Zymux's bytecode.
 * 
 * Read the opcodes text file in docs for more information about each opcode.
 */
typedef enum {
    OP_LOAD_CONST,
    OP_TRUE,
    OP_FALSE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MODULO,
    OP_EXPONENT,
    OP_EQ,
    OP_NOT_EQ,
    OP_GREATER,
    OP_GREATER_EQ,
    OP_LESS,
    OP_LESS_EQ,
    OP_MINUS,
    OP_NOT,
    OP_POP,
    OP_END
} OpCode;

/** Emits a single byte of some kind to the compiler's bytecode. */
void emit(Compiler *compiler, u8 byte, SourcePosition bytePos);

/** Emits a number that's at most the size of an integer. TODO: make it handle ints over 1 byte. */
void emit_number(Compiler *compiler, const int number, SourcePosition bytePos);

/** Reads a number which begins from numStart. TODO: make it handle numbers over 1 byte. */
int read_number(FuncObj *function, const int numStart);

/** Emits an instruction that has a constant to read immediately after. */
void emit_const(Compiler *compiler, u8 byte, Obj *constant, SourcePosition bytePos);

#endif
