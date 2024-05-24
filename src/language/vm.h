#ifndef VM_H
#define VM_H

#include "object.h"
#include "emitter.h"

/** Holds a frame around of information around the currently executing function object. */
typedef struct {
    FuncObj *func; /** Current executing function. */
    u8 *ip; /** Instruction pointer. Points to the byte that should be read next. */
    Obj **sp; /** Stack pointer. Points to an object (which itself is an allocated pointer). */
} StackFrame;

/** An array of frames that hold each function call's information. */
DECLARE_DA_STRUCT(CallStack, StackFrame);

/** Zymux's virtual machine for interpreting the bytecode. */
typedef struct {
    ZmxProgram *program; /** The Zymux program for general information and allocations. */
    InstrSize instrSize; /** The size of the number instruction to be read after the opcode. */
    CallStack callStack; /** Holds the entire call stack of stack frames. */
    StackFrame *frame; /** The current frame which holds the bytecode we're executing. */

    /** 
     * The stack that keeps track of objects while executing.
     * 
     * This doesn't use the default dynamic array, but a special array without length.
     * The reason is because we want maximum performance when operating on the stack
     * with a stack pointer (SP), which means that the stack gets specialized macros that don't
     * increase or decrease the length, but instead rely on pointer arithmetic
     * between SP and the stack to figure out the length.
     * 
     * Also, we have to change everything that points somewhere in the stack
     * if an append's reallocation causes the stack's address to move.
     * So Zymux's default reallocation used in normal dynamic arrays won't cut it.
     */
    struct {
        Obj **objects;
        u32 capacity;
    } stack;
} Vm;

/** Returns a VM initialized from the passed func. */
Vm create_vm(ZmxProgram *program, FuncObj *func);

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm);

/** Executes all the bytecode in the passed VM's function object. */
bool interpret(Vm *vm);

/** Returns a vm created from the compilation of the source (not executed). */
Vm vm_from_source(ZmxProgram *program, char *source);

/** 
 * Simply executes the passed source string and frees all memory used except the program's.
 * 
 * This is because Zymux sometimes extra VMs for imported modules.
 */
bool interpret_source(ZmxProgram *program, char *source);

#endif
