#ifndef VM_H
#define VM_H

#include "emitter.h"
#include "hash_table.h"
#include "object.h"

/** Resolves to the length of the stack. */
#define STACK_LENGTH(vm) ((vm)->frame->sp - (vm)->stack.objects)

/** 
 * The macro for a normal VM runtime error call.
 * 
 * Appends the formatted message to a temporary buffer, then creates a string object
 * which is GC protected and passes it to the base runtime error call.
 * 
 * Unprotects the string object and frees the buffer after the call has finished.
 */
#define RUNTIME_ERROR(vm, ...) \
    do { \
        CharBuffer buffer_ = create_char_buffer(); \
        buffer_append_format(&buffer_, __VA_ARGS__); \
        GC_FREEZE(&(vm)->program->gc); \
        base_runtime_error(vm, new_string_obj((vm)->program, buffer_.text, strlen(buffer_.text))); \
        GC_END_FREEZE(&(vm)->program->gc); \
        free_char_buffer(&buffer_); \
    } while (false)

/** 
 * Represents some information of the VM that needs to be reverted to when an error is caught.
 * 
 * Only stores the amount of locals/captures and not their arrays themselves, as any variables
 * modified inside the try-catch should stay modified, the only thing we revert is removing
 * new variables created inside the try-catch.
 */
typedef struct {
    FuncObj *func; /** Function that has the try-catch block. */
    u8 *ip; /** Points to the first instruction in the catch block of func. */

    u32 localsAmount; /** Amount of locals in the VM's stack when this was created. */
    u32 capturesAmount; /** 0 if func isn't a closure to begin with. */
    u32 openCapturesAmount; /** The amount of captures not closed at the time of the try-catch. */

    u32 frameAmount; /** The amount of frames the VM had when this was created. */
    u32 moduleAmount; /** The amount of module contexts the VM had when this was created. */
    bool saveError; /** Whether or not the error message should be stored as a local variable. */
} CatchState;

/** An array of catch states for being inside try-catch statements. */
DECLARE_DA_STRUCT(CatchStateArray, CatchState);

/** Holds a frame around of information around a currently executing function object. */
typedef struct {
    FuncObj *func; /** Current executing function. */
    u8 *ip; /** Instruction pointer. Points to the byte that should be read next. */
    Obj **bp; /** Base pointer. Points to the first object in the stack frame. */
    Obj **sp; /** Stack pointer. Points to the topmost object in the stack + 1. */
} StackFrame;

/** An array of frames that hold each function call's information at runtime. */
DECLARE_DA_STRUCT(CallStack, StackFrame);

/** Holds the context of one module at runtime. */
typedef struct ModuleContext {
    StringObj *name; /** The name of this module. */
    Table globals; /** A table of global variable key names and values. */
    ObjArray openCaptures; /** Captures whose locals are still alive on the stack. */
} ModuleContext;

DECLARE_DA_STRUCT(ModuleContextArray, ModuleContext);

/** Zymux's virtual machine for interpreting the bytecode. */
typedef struct Vm {
    ZmxProgram *program; /** The Zymux program for general information and allocations. */
    InstrSize instrSize; /** The size of the number instruction to be read after the opcode. */
    CatchStateArray catches; /** Saved states for caught errors where last item is closest catch. */

    CallStack callStack; /** Holds the entire call stack of stack frames during runtime. */
    StackFrame *frame; /** The current frame which holds the bytecode we're executing. */

    ModuleContextArray modules; /** This holds context of other executing modules in an array. */
    Table *globals; /** A pointer to the global variables of the current module. */
    ObjArray *openCaptures; /** A pointer to the open captures of the current module. */

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
     * So the default reallocation used in normal dynamic arrays won't cut it.
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

/** Executes the bytecode inside the passed, already created VM. */
bool interpret_vm(Vm *vm);

/** 
 * Simply executes the passed source string and frees all memory used except the program's.
 * 
 * This is because Zymux sometimes extra VMs for imported modules.
 */
bool interpret_file_source(ZmxProgram *program, char *source, const bool isMain);

/** 
 * Base function that tries to raise a runtime error with an object error message.
 * 
 * Sets a catch state and doesn't error if there's one. Meant only as a base error function,
 * so prefer to use macros that wrap this instead of directly calling it.
 */
void base_runtime_error(Vm *vm, StringObj *errorMessage);

#endif
