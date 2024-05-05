#include "compiler.h"
#include "emitter.h"

/** 
 * Emits/appends a single byte to the passed compiler's current function.
 * Also appends bytePos as debugging info to the positions array if enabled.
 */
void emit(Compiler *compiler, u8 byte, SourcePosition bytePos) {
    APPEND_DA(&compiler->func->bytecode, byte);
    if (compiler->isDebugging) {
        APPEND_DA(&compiler->func->positions, bytePos);
    }
}

/** Emits a number that's at most the size of an integer. TODO: make it handle ints over 1 byte. */
void emit_number(Compiler *compiler, const int number, SourcePosition bytePos) {
    APPEND_DA(&compiler->func->bytecode, number);
    if (compiler->isDebugging) {
        APPEND_DA(&compiler->func->positions, bytePos);
    }
}

/** Reads a number which begins from numStart. TODO: make it handle numbers over 1 byte. */
int read_number(FuncObj *function, const int numStart) {
    return function->bytecode.data[numStart];
}

/** Emits an instruction that has a constant to read immediately after. */
void emit_const(Compiler *compiler, u8 byte, Obj *constant, SourcePosition bytePos) {
    emit(compiler, byte, bytePos);

    APPEND_DA(&compiler->func->constPool, constant);
    int constIdx = compiler->func->constPool.length - 1;
    emit_number(compiler, constIdx, bytePos);
}
