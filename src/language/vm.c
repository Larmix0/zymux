#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "debug_runtime.h"
#include "char_buffer.h"
#include "compiler.h"
#include "emitter.h"
#include "file.h"
#include "vm.h"

/** 
 * A user error that occurred at runtime, which means during the VM's execution of bytecode.
 * 
 * Resolves to a value of false so it can also be returned in the interpreter as a fail.
 */
#define RUNTIME_ERROR(vm, ...) \
    (zmx_user_error((vm)->program, runtime_error_pos(vm), "Runtime error", __VA_ARGS__), false)

/** Reads the current instruction and increments IP to prepare for the next one. */
#define READ_INSTR(vm) (*(vm)->frame->ip++)

// TODO: turn the read_number function fully to a macro for performance?
// Pretty considerable since it's also used by the const reader.
/** Reads the upcoming number in the bytecode. Prefer this over the function in the VM. */
#define READ_NUMBER(vm) \
    ((vm)->frame->ip += (vm)->instrSize, \
    read_number( \
        (vm)->frame->func, (vm)->frame->ip - (vm)->frame->func->bytecode.data - (vm)->instrSize, \
        &(vm)->instrSize \
    ))

/** Reads the upcoming number and uses it to index an object in the current constant pool. */
#define READ_CONST(vm) ((vm)->frame->func->constPool.data[READ_NUMBER(vm)])

#define CREATE_STACK() {.objects = NULL, .capacity = 0}
#define STACK_LENGTH(vm) ((vm)->frame->sp - (vm)->stack.objects)
#define PUSH(vm, obj) \
    do { \
        if ((vm)->stack.capacity < STACK_LENGTH(vm) + 1) { \
            reallocate_stack(vm); \
        } \
        *(vm)->frame->sp++ = (obj); \
    } while (false)
#define POP(vm) (*(--(vm)->frame->sp))
#define DROP(vm) (--(vm)->frame->sp)
#define DROP_AMOUNT(vm, amount) ((vm)->frame->sp -= (amount))
#define PEEK(vm) (*((vm)->frame->sp - 1))
#define PEEK_DEPTH(vm, depth) (*((vm)->frame->sp - (depth) - 1))
#define FREE_STACK(vm) (free((vm)->stack.objects))

/** Resolves to whether or not the passed object is considered a number (integer or float). */
#define IS_NUM(obj) ((obj)->type == OBJ_INT || (obj)->type == OBJ_FLOAT)

/** Returns the number the passed object holds, assuming it's either an integer or float object. */
#define NUM_VAL(obj) \
    ((obj)->type == OBJ_INT ? AS_PTR(IntObj, obj)->number : AS_PTR(FloatObj, obj)->number)

#define BIN_LEFT(vm) (PEEK_DEPTH(vm, 1)) /** Left hand side object of a binary in the stack. */
#define BIN_RIGHT(vm) (PEEK_DEPTH(vm, 0)) /** Right hand side object of a binary in the stack. */

/** Resolves to a boolean of whether or not either of the binary numbers in the stack are floats. */
#define BIN_HAS_FLOAT(vm) (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT)

/** 
 * Pushes resultNum and drops the last 2 elements (from the binary operation).
 * 
 * The resultNum passed is a C number (integer or float) which will be pushed
 * as the appropriate integer or float object after the binary operands have been dropped.
 * 
 * Used by passing a binary operation but not popping its operands from the stack while doing that.
 */
#define BIN_OP_MATH(vm, resultNum) \
    do { \
        if (!IS_NUM(BIN_LEFT(vm)) || !IS_NUM(BIN_RIGHT(vm))) { \
            return RUNTIME_ERROR(vm, "Expected number in operation."); \
        } \
        Obj *resultObj = BIN_HAS_FLOAT(vm) ? AS_OBJ(new_float_obj((vm)->program, resultNum)) \
            : AS_OBJ(new_int_obj((vm)->program, resultNum)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** 
 * Pushes number and drops the last 2 elements (from the binary operation).
 * 
 * The number passed is a resulting C boolean which will be pushed as a bool object
 * after the binary operands have been dropped.
 * 
 * Used by passing a binary operation but not popping its operands from the stack while doing that.
 */
#define BIN_OP_BOOL(vm, resultBool) \
    do { \
        if (!IS_NUM(BIN_LEFT(vm)) || !IS_NUM(BIN_RIGHT(vm))) { \
            return RUNTIME_ERROR(vm, "Expected number in operation."); \
        } \
        Obj *resultObj = AS_OBJ(new_bool_obj(vm->program, resultBool)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** An array of just integers to store the indices of each function in the call stack. */
DECLARE_DA_STRUCT(IntArray, int);

// TODO: Add better documentation describing the method used for recompiling with debugging.
/**
 * From an integer array representing the stack trace + starting file,
 * retrace and compile the whole program and walk through the constant pool of each function
 * one by one in frameIndices in order to find the original one.
 */
static FuncObj *copy_errored_func(ZmxProgram *program, IntArray framesIndices) {
    char *mainSource = alloc_source(program->currentFile->string);
    FuncObj *current = compile_source(program, mainSource, true);
    free(mainSource);
    for (u32 i = 0; i < framesIndices.length; i++) {
        current = AS_PTR(FuncObj, current->constPool.data[framesIndices.data[i]]);
    }
    return current;
}

/** 
 * Copies the indices of each constant in the passed call stack into an integer array.
 * 
 * Does not copy the const index of the first (top-level) function as that's the starting point,
 * and it isn't present in any const pools. 
 */
IntArray copy_const_indices(CallStack callStack) {
    IntArray framesIndices = CREATE_DA();
    for (u32 i = 1; i < callStack.length; i++) {
        APPEND_DA(&framesIndices, callStack.data[i].func->constIdx);
    }
    return framesIndices;
}

/** 
 * Reports a the position of a runtime error.
 * 
 * Because the original compilation doesn't include bytecode positions (for optimization),
 * we first delete the VM so we have enough memory to recompile the main file with positions on.
 * 
 * After that, we traverse the call stack and check the constant index of each frame's function.
 * This is a way to find out exactly where the function that errored and its bytecode lies,
 * even if multiple functions in different scopes can have the same name.
 * 
 * Also, this is only possible since Zymux only creates function at compilation time,
 * meaning every executable function is in a constant pool of objects created at compilation time.
 */
static SourcePosition runtime_error_pos(Vm *vm) {
    ZmxProgram *program = vm->program;
    const int bytecodeIdx = vm->frame->ip - vm->frame->func->bytecode.data;
    IntArray framesIndices = copy_const_indices(vm->callStack);

    char *mainName = ZMX_ARRAY_ALLOC(program->mainFile->length + 1, char);
    strncpy(mainName, program->mainFile->string, program->mainFile->length);
    mainName[program->mainFile->length] = '\0';
    free_zmx_program(program);
    *program = create_zmx_program(mainName, true);
    free_vm(vm);
    free(mainName);

    // Recreate the VM to avoid a double free.
    *vm = create_vm(
        program, new_func_obj(program, new_string_obj(program, "<Error>"), -1)
    );
    FuncObj *erroredFunc = copy_errored_func(program, framesIndices);
    FREE_DA(&framesIndices);
    return erroredFunc->positions.data[bytecodeIdx];
}

/** 
 * Special reallocation function for the VM's stack.
 * 
 * The thing about the stack is there are a bunch of things that point in the middle of it
 * (like the stack pointer for example), but reallocation of memory might move the address
 * of the allocated memory and free the original, old one.
 * This in turn means that every pointer that points to an object inside the stack needs
 * to also be updated to the same index relative to the potentially new stack address.
 */
static void reallocate_stack(Vm *vm) {
    INCREASE_CAP(vm->stack.capacity);
    Obj **originalStack = vm->stack.objects;
    vm->stack.objects = ZMX_REALLOC(
        vm->stack.objects, vm->stack.capacity * sizeof(*vm->stack.objects)
    );
    if (vm->stack.objects == originalStack) {
        // Stack address didn't change, no need to move pointers in the stack to a new location.
        return;
    }
    
    for (u32 i = 0; i < vm->callStack.length; i++) {
        vm->callStack.data[i].sp = &vm->stack.objects[vm->callStack.data[i].sp - originalStack];
    }
}

/** Returns a stack frame to hold information about the currently executing function. */
static StackFrame create_frame(FuncObj *func, Obj **sp) {
    StackFrame frame = {.func = func, .ip = func->bytecode.data, .sp = sp};
    return frame;
}

/** Returns a VM initialized from the passed func. */
Vm create_vm(ZmxProgram *program, FuncObj *func) {
    Vm vm = {
        .program = program, .instrSize = INSTR_ONE_BYTE,
        .callStack = CREATE_DA(), .frame = NULL, .stack = CREATE_STACK()
    };

    // Set current frame and its SP after their arrays have been initialized.
    APPEND_DA(&vm.callStack, create_frame(func, vm.stack.objects));
    vm.frame = vm.callStack.data;
    return vm;
}

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm) {
    FREE_DA(&vm->callStack);
    FREE_STACK(vm);
}

/** Executes all the bytecode in the passed VM's function object. */
bool interpret(Vm *vm) {
    while (true) {
#if DEBUG_RUNTIME == 1
        print_runtime_state(
            vm->frame->func, vm->stack.objects, STACK_LENGTH(vm),
            vm->frame->ip - vm->frame->func->bytecode.data, vm->instrSize
        );
#endif
        switch (READ_INSTR(vm)) {
        case OP_LOAD_CONST:
            PUSH(vm, READ_CONST(vm));
            break;
        case OP_ARG_16: vm->instrSize = INSTR_TWO_BYTES; break;
        case OP_ARG_32: vm->instrSize = INSTR_FOUR_BYTES; break;
        case OP_TRUE: PUSH(vm, AS_OBJ(new_bool_obj(vm->program, true))); break;
        case OP_FALSE: PUSH(vm, AS_OBJ(new_bool_obj(vm->program, false))); break;
        case OP_ADD: BIN_OP_MATH(vm, NUM_VAL(BIN_LEFT(vm)) + NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_SUBTRACT: BIN_OP_MATH(vm, NUM_VAL(BIN_LEFT(vm)) - NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_MULTIPLY: BIN_OP_MATH(vm, NUM_VAL(BIN_LEFT(vm)) * NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_DIVIDE: BIN_OP_MATH(vm, NUM_VAL(BIN_LEFT(vm)) / NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_MODULO:
            // Modulo doesn't handle floats in C, so use fmod() if there's a float in the operation.
            BIN_OP_MATH(
                vm,
                BIN_HAS_FLOAT(vm) ? fmod(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                    : AS_PTR(IntObj, BIN_LEFT(vm))->number % AS_PTR(IntObj, BIN_RIGHT(vm))->number
            );
            break;
        case OP_EXPONENT:
            BIN_OP_MATH(vm, pow(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm))));
            break;
        case OP_EQ: BIN_OP_BOOL(vm, equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm))); break;
        case OP_NOT_EQ: BIN_OP_BOOL(vm, !equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm))); break;
        case OP_GREATER: BIN_OP_BOOL(vm, NUM_VAL(BIN_LEFT(vm)) > NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_GREATER_EQ: BIN_OP_BOOL(vm, NUM_VAL(BIN_LEFT(vm)) >= NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_LESS: BIN_OP_BOOL(vm, NUM_VAL(BIN_LEFT(vm)) < NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_LESS_EQ: BIN_OP_BOOL(vm, NUM_VAL(BIN_LEFT(vm)) >= NUM_VAL(BIN_RIGHT(vm))); break;
        case OP_MINUS:
            if (PEEK(vm)->type == OBJ_INT) {
                const ZmxInt negated = -AS_PTR(IntObj, POP(vm))->number;
                PUSH(vm, AS_OBJ(new_int_obj(vm->program, negated)));
            } else if (PEEK(vm)->type == OBJ_FLOAT) {
                const ZmxFloat negated = -AS_PTR(FloatObj, POP(vm))->number;
                PUSH(vm, AS_OBJ(new_float_obj(vm->program, negated)));
            }
            break;
        case OP_NOT: {
            BoolObj *asBool = new_bool_obj(vm->program, !as_bool(vm->program, POP(vm))->boolean);
            PUSH(vm, AS_OBJ(asBool));
            break;
        }
        case OP_AS: {
            const Obj *original = POP(vm);
            switch (READ_NUMBER(vm)) {
            // TODO: implement other types.
            case TYPE_BOOL: PUSH(vm, AS_OBJ(as_bool(vm->program, original))); break;
            case TYPE_STRING: PUSH(vm, AS_OBJ(as_string(vm->program, original))); break;
            default: UNREACHABLE_ERROR();
            }
            break;
        }
        case OP_FINISH_STRING: {
            const u32 amount = READ_NUMBER(vm);
            StringObj *string = new_string_obj(vm->program, "");

            // Build a string from the deepest/oldest till the outermost/newest one in the stack.
            for (u32 i = 1; i <= amount; i++) {
                string = concatenate(
                    vm->program, string, AS_PTR(StringObj, PEEK_DEPTH(vm, amount - i))
                );
            }
            DROP_AMOUNT(vm, amount);
            PUSH(vm, AS_OBJ(string));
            break;
        }
        case OP_POP:
            DROP(vm);
            break;
        case OP_END:
            return true;
        default:
            UNREACHABLE_ERROR();
        }
    }
}

/** Returns a vm created from the compilation of the source (not executed). */
Vm vm_from_source(ZmxProgram *program, char *source) {
    FuncObj *mainFunc = compile_source(program, source, DEBUG_COMPILER);
    return create_vm(program, mainFunc);
}

/** 
 * Simply executes the passed source string and frees all memory used except the program's.
 * 
 * This is because Zymux sometimes extra VMs for imported modules.
 */
bool interpret_source(ZmxProgram *program, char *source) {
    Vm vm = vm_from_source(program, source);
    if (!program->hasErrored) {
        interpret(&vm);
    }
    free_vm(&vm);
    return !program->hasErrored;
}
