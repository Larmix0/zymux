#ifndef EMITTER_H
#define EMITTER_H

#include "constants.h"
#include "data_structures.h"
#include "object.h"
#include "report_error.h"

typedef struct Compiler Compiler;

/** 
 * Represents how many bytes the next number to be read is.
 * 
 * This enum is also intended to have its members directly used as integers in operations.
 */
typedef enum {
    INSTR_ONE_BYTE = 1,
    INSTR_TWO_BYTES = 2,
    INSTR_FOUR_BYTES = 4
} InstrSize;

/** Represents each data type in Zymux. */
typedef enum {
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
    TYPE_STRING
} DataType;

/** 
 * Represents each opcode in Zymux's bytecode.
 * 
 * Read the opcodes text file in docs for more information about each opcode.
 */
typedef enum {
    OP_LOAD_CONST,
    OP_ARG_16,
    OP_ARG_32,
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
    OP_AS,
    OP_FINISH_STRING,
    OP_POP,
    OP_END
} OpCode;

/** Emits a single byte of some kind to the compiler's bytecode. */
void emit_instr(Compiler *compiler, u8 byte, SourcePosition bytePos);

/** Emits a 1 byte instruction followed by a number that's at most the size of a U32. */
void emit_number(Compiler *compiler, u8 byte, const u32 number, SourcePosition pos);

/** Reads a number that's a max size of a U32 which begins from numStart. */
u32 read_number(FuncObj *function, const u32 numStart, InstrSize *size);

/** Emits an instruction followed by an idx after to be used for an object in the const pool. */
void emit_const(Compiler *compiler, u8 byte, Obj *constant, SourcePosition bytePos);

#endif
