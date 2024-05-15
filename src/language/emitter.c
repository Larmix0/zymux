#include "compiler.h"
#include "emitter.h"

/** Appends pos needed amount of times for func's positions length to match bytecode's length. */
static void append_instr_pos(FuncObj *func, SourcePosition pos) {
    while (func->positions.length < func->bytecode.length) {
        APPEND_DA(&func->positions, pos);
    }
}

/** 
 * Emits/appends a single byte instruction to the passed compiler's current function.
 * Also appends bytePos as debugging info to the positions array if enabled.
 */
void emit_instr(Compiler *compiler, u8 byte, SourcePosition bytePos) {
    APPEND_DA(&compiler->func->bytecode, byte);
    if (compiler->isDebugging) {
        append_instr_pos(compiler->func, bytePos);
    }
}

/** 
 * Emits an instruction followed by a number that's at most the size of an integer.
 * 
 * It starts emitting the number in order of the largest (leftmost) byte first,
 * then goes 1 byte to the right till the rightmost byte.
 */
void emit_number(Compiler *compiler, u8 byte, const u32 number, SourcePosition pos) {
    if (number <= U8_MAX) {
        emit_instr(compiler, byte, pos);
        emit_instr(compiler, number, pos);
    } else if (number > U8_MAX && number <= U16_MAX) {
        emit_instr(compiler, OP_ARG_16, pos);
        emit_instr(compiler, byte, pos);
        emit_instr(compiler, number >> 8, pos);
        emit_instr(compiler, number & 0xFF, pos);
    } else {
        // Full integer.
        emit_instr(compiler, OP_ARG_32, pos);
        emit_instr(compiler, byte, pos);
        for (int i = INSTR_FOUR_BYTES - 1; i >= 0; i--) {
            emit_instr(compiler, (number >> 8 * i) & 0xFF, pos);
        }
    }

    if (compiler->isDebugging) {
        append_instr_pos(compiler->func, pos);
    }
}

/** 
 * Reads a number which begins from numStart in the bytecode.
 * 
 * The amount of bytes read depends on the passed instruction size. Worth mentioning that the 
 * instruction size pointer is automatically set to 1 byte for next reads after being called.
 */
u32 read_number(FuncObj *function, const u32 numStart, InstrSize *size) {
    switch (*size) {
    case INSTR_ONE_BYTE:
        return function->bytecode.data[numStart];
    case INSTR_TWO_BYTES: {
        *size = INSTR_ONE_BYTE;
        u16 total = function->bytecode.data[numStart];
        total = (total << 8) | function->bytecode.data[numStart + 1];
        return (u32)total;
    }
    case INSTR_FOUR_BYTES: {
        *size = INSTR_ONE_BYTE;
        u32 total = 0;
        for (int i = 0; i < INSTR_FOUR_BYTES; i++) {
            total = (total << 8) | function->bytecode.data[numStart + i];
        }
        return total;
    }
    default: UNREACHABLE_ERROR(); return 0;
    }
}

/** Emits an instruction followed by an idx after to be used for an object in the const pool. */
void emit_const(Compiler *compiler, u8 byte, Obj *constant, SourcePosition bytePos) {
    APPEND_DA(&compiler->func->constPool, constant);
    u32 constIdx = compiler->func->constPool.length - 1;
    emit_number(compiler, byte, constIdx, bytePos);
}
