#ifndef EMITTER_H
#define EMITTER_H

#include "constants.h"
#include "dynamic_array.h"
#include "report_error.h"

typedef struct Compiler Compiler;
typedef struct Obj Obj;

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

/** Represents each data type at runtime. */
typedef enum {
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
    TYPE_STRING
} DataType;

/** Represents each opcode in Zymux's bytecode. */
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
    OP_LSHIFT,
    OP_RSHIFT,
    OP_BITWISE_OR,
    OP_BITWISE_AND,
    OP_XOR,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_GREATER,
    OP_GREATER_EQ,
    OP_LESS,
    OP_LESS_EQ,
    OP_MINUS,
    OP_NOT,
    OP_TILDE,
    OP_IS,
    OP_AS,
    OP_FINISH_STRING,
    OP_RANGE,
    OP_TERNARY,
    OP_LIST,
    OP_MAP,
    OP_DESTRUCTURE,
    OP_FOR_ASSIGN_VARS,
    OP_MAKE_ITER,
    OP_FOR_ITER_ASSIGN,
    OP_FOR_ITER_LOAD,
    OP_GET_BUILT_IN,
    OP_DECLARE_GLOBAL,
    OP_ASSIGN_GLOBAL,
    OP_GET_GLOBAL,
    OP_ASSIGN_LOCAL,
    OP_GET_LOCAL,
    OP_CAPTURE_DEPTH,
    OP_CAPTURE_AT,
    OP_ASSIGN_CAPTURED,
    OP_GET_CAPTURED,
    OP_SET_PROPERTY,
    OP_GET_PROPERTY,
    OP_GET_SUPER,
    OP_CALL,
    OP_ASSIGN_SUBSCR,
    OP_GET_SUBSCR,
    OP_FUNC,
    OP_OPTIONALS_FUNC,
    OP_MAKE_CLOSURE,
    OP_CLASS,
    OP_ADD_INIT,
    OP_METHODS,
    OP_ABSTRACT_METHODS,
    OP_INHERIT,
    OP_JUMP,
    OP_JUMP_BACK,
    OP_JUMP_IF,
    OP_JUMP_IF_NOT,
    OP_POP_JUMP_IF,
    OP_POP_JUMP_IF_NOT,
    OP_POP_JUMP_BACK_IF,
    OP_POP_LOCAL,
    OP_POP_LOCALS,
    OP_POP_CAPTURES,
    OP_RETURN,
    OP_CLOSURE_RETURN,
    OP_COPY_TOP,
    OP_IMPORT,
    OP_IMPORT_NAMES,
    OP_START_TRY,
    OP_FINISH_TRY,
    OP_RAISE,
    OP_END_MODULE,
    OP_EXIT,
    OP_EOF
} OpCode;

/** Represents the information of a jump instruction in bytecode (which may change to patching). */
typedef struct {
    u32 index; /** The index of the first byte of the jump in bytecode. */
    u32 size; /** The amount of bytes to jump over. */
    i64 indexOffset; /** The change in index that occurred during fixing. */
    i64 sizeOffset; /** The change in size that occurred during fixing. */
    InstrSize instrSize; /** The bytes after the instruction which represent the jump. */
    bool isForward; /** Whether the jump is forwards or backwards. */
    bool isResolved; /** Whether its offsets for other jumps have been resolved or not. */
} Jump;

/** An array of jumps that may change later. */
DECLARE_DA_STRUCT(JumpArray, Jump);

/** Emits a single byte of some kind to the compiler's bytecode. */
void emit_instr(Compiler *compiler, u8 instr, const SourcePosition pos);

/** Emits a 1 byte instruction followed by a number that's at most the size of a U32. */
void emit_number(Compiler *compiler, u8 instr, const u32 number, const SourcePosition pos);

/** Reads a number that's a max size of a U32 which begins from numStart as an index in bytecode. */
u32 bytecode_number(const ByteArray *bytecode, const u32 numStart, InstrSize *size);

/** Emits an instruction followed by an idx after to be used for an object in the const pool. */
void emit_const(Compiler *compiler, u8 instr, Obj *constant, const SourcePosition pos);

/** 
 * Emits a default unfinished jump, which will have the actual jump itself inserted/patched later.
 * 
 * Returns the index where the jump is supposed to be inserted in the bytecode.
 */
u32 emit_unpatched_jump(Compiler *compiler, u8 instr, const SourcePosition pos);

/** Patches a jump in the compiler from the passed jump information. */
void patch_jump(Compiler *compiler, const u32 start, const u32 end, const bool isForward);

/** Patches a u32 array of different opcode locations that all resolve to one place. */
void patch_all_jumps(
    Compiler *compiler, const U32Array starts, const u32 end, const bool isForward
);

/** 
 * Emits a normal jump instruction which is already patched.
 * 
 * The jump starts from the current byte and uses the passed "to" as the jump destination.
 */
void emit_jump(
    Compiler *compiler, u8 instr, const u32 to, const bool isForward, const SourcePosition pos
);

/** 
 * Emits some amount of local variable pops in a compiler optimized manner.
 * Doesn't emit anything if poppedAmount is 0.
 */
void emit_local_pops(Compiler *compiler, const u32 poppedAmount, const SourcePosition pos);

/** Emits a number of captured pops. Emits none if poppedAmount is 0. */
void emit_captured_pops(Compiler *compiler, const u32 poppedAmount, const SourcePosition pos);

/** 
 * Writes all the jumps of the compiler in the actual bytecode.
 * 
 * Should only be called once the function has finished compiling.
 */
void write_jumps(Compiler *compiler);

#endif
