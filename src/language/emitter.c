#include "compiler.h"
#include "emitter.h"

/** 
 * Emits/appends a single byte instruction to the passed compiler's current function.
 * Also appends bytePos as debugging info to the positions array if enabled.
 */
void emit_instr(Compiler *compiler, u8 byte, SourcePosition bytePos) {
    APPEND_DA(&compiler->func->bytecode, byte);
    if (compiler->isDebugging) {
        APPEND_DA(&compiler->func->positions, bytePos);
    }
}

/** Emits an instruction followed by a number that's at most the size of an integer. */
// TODO: handle sizes over 1 byte.
void emit_number(Compiler *compiler, u8 byte, const u32 number, SourcePosition pos) {
    emit_instr(compiler, byte, pos);
    APPEND_DA(&compiler->func->bytecode, number);
    if (compiler->isDebugging) {
        APPEND_DA(&compiler->func->positions, pos);
    }
}

// TODO: make it handle numbers over 1 byte.
/** Reads a number which begins from numStart in the bytecode */
u32 read_number(FuncObj *function, const int numStart) {
    return function->bytecode.data[numStart];
}

/** Emits an instruction followed by an idx after to be used for an object in the const pool. */
void emit_const(Compiler *compiler, u8 byte, Obj *constant, SourcePosition bytePos) {
    APPEND_DA(&compiler->func->constPool, constant);
    int constIdx = compiler->func->constPool.length - 1;
    emit_number(compiler, byte, constIdx, bytePos);
}
