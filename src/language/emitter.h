#ifndef EMITTER_H
#define EMITTER_H

#include "constants.h"
#include "dynamic_array.h"
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
    OP_NULL,
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
    OP_DECLARE_GLOBAL,
    OP_ASSIGN_GLOBAL,
    OP_GET_GLOBAL,
    OP_ASSIGN_LOCAL,
    OP_GET_LOCAL,
    OP_JUMP,
    OP_POP_JUMP_IF_FALSE,
    OP_POP,
    OP_POP_AMOUNT,
    OP_END
} OpCode;

/** Represents the information of a jump instruction in bytecode (which may change to patching). */
typedef struct {
    u32 index; /** The index of the first byte of the jump in bytecode. */
    u32 size; /** The amount of bytes to jump over. */
    InstrSize instrSize; /** The bytes after the instruction which represent the jump. */
    bool isForward; /** Whether the jump is forwards or backwards. */
} Jump;

/** An array of jumps that may change later. */
DECLARE_DA_STRUCT(JumpArray, Jump);

/** Emits a single byte of some kind to the compiler's bytecode. */
void emit_instr(Compiler *compiler, u8 instr, SourcePosition pos);

/** Emits a 1 byte instruction followed by a number that's at most the size of a U32. */
void emit_number(Compiler *compiler, u8 instr, const u32 number, SourcePosition pos);

/** Reads a number that's a max size of a U32 which begins from numStart as an index in bytecode. */
u32 read_number(const FuncObj *function, const u32 numStart, InstrSize *size);

/** Emits an instruction followed by an idx after to be used for an object in the const pool. */
void emit_const(Compiler *compiler, u8 instr, Obj *constant, SourcePosition pos);

/** Emits a normal jump instruction with a number after that represents the size of the jump. */
void emit_jump(
    Compiler *compiler, u8 instr, const u32 jumpSize, const bool isForward, SourcePosition pos
);

/** 
 * Emits a default unfinished jump, which will have the actual jump itself inserted/patched later.
 * 
 * Returns the index where the unpatched jump is located in the bytecode.
 */
u32 emit_unpatched_jump(Compiler *compiler, u8 instr, SourcePosition pos);

/** Patches a jump in the compiler from the passed jump information. */
void patch_jump(Compiler *compiler, const u32 start, const u32 end, const bool isForward);

/** 
 * Writes all the jumps of the compiler in the actual bytecode.
 * 
 * Should only be called once the function has finished compiling.
 */
void write_jumps(Compiler *compiler);

#endif
