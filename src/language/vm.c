#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "char_buffer.h"
#include "compiler.h"
#include "emitter.h"
#include "file.h"
#include "object.h"
#include "vm.h"

#if DEBUG_RUNTIME
    #include <stdio.h>

    #include "debug_runtime.h"
#endif

/** Reads the current instruction and increments IP to prepare for the next one. */
#define READ_INSTR(vm) (*(vm)->frame->ip++)

// TODO: turn the read_number function fully to a macro for performance?
// Pretty considerable since it's also used by the const reader.
/** Reads the upcoming number in the bytecode. Prefer this over the function in the VM. */
#define READ_NUMBER(vm) \
    ((vm)->frame->ip += (vm)->instrSize, \
    read_number( \
        &(vm)->frame->func->bytecode, \
        (vm)->frame->ip - (vm)->frame->func->bytecode.data - (vm)->instrSize, &(vm)->instrSize \
    ))

/** Reads the upcoming number and uses it to index an object in the current constant pool. */
#define READ_CONST(vm) ((vm)->frame->func->constPool.data[READ_NUMBER(vm)])

#define CREATE_STACK() {.objects = NULL, .capacity = 0}
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

#define BIN_LEFT(vm) (PEEK_DEPTH(vm, 1)) /** Left hand side object of a binary in the stack. */
#define BIN_RIGHT(vm) (PEEK_DEPTH(vm, 0)) /** Right hand side object of a binary in the stack. */

/** Resolves to a boolean of whether or not either of the binary numbers in the stack are floats. */
#define BIN_HAS_FLOAT(vm) (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT)

#define BIN_OP_ERROR(vm, opString) \
    (runtime_error( \
        vm, "Can't perform %s between %s and %s.", \
        opString, obj_type_string(BIN_LEFT(vm)->type), obj_type_string(BIN_RIGHT(vm)->type)) \
    )

/** 
 * Pushes resultNum and drops the last 2 elements (from the binary operation).
 * 
 * The resultNum passed is a C number (integer or float) which will be pushed
 * as the appropriate integer or float object after the binary operands have been dropped.
 * 
 * Used by passing a binary operation but not popping its operands from the stack while doing that.
 * 
 * Prints opString as the bugged operation if an error occurs.
 */
#define MATH_BIN_OP(vm, opString, resultNum) \
    do { \
        if (!IS_NUM(BIN_LEFT(vm)) || !IS_NUM(BIN_RIGHT(vm))) { \
            return BIN_OP_ERROR(vm, opString); \
        } \
        Obj *resultObj = BIN_HAS_FLOAT(vm) ? AS_OBJ(new_float_obj((vm)->program, resultNum)) \
            : AS_OBJ(new_int_obj((vm)->program, resultNum)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** 
 * Pushes resultBool and drops the last 2 elements (from the binary operation).
 * 
 * The resultBool passed is a resulting C boolean which will be pushed as a bool object
 * after the binary operands have been dropped.
 * 
 * Allows operands of any type
 * 
 * Used by passing a binary operation but not popping its operands from the stack while doing that.
 */
#define BOOL_BIN_OP(vm, resultBool) \
    do { \
        Obj *resultObj = AS_OBJ(new_bool_obj(vm->program, resultBool)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** BOOL_BIN_OP() wrapper which errors if the operands of the operation aren't both numbers. */
#define NUM_BOOL_BIN_OP(vm, opString, resultBool) \
    do { \
        if (!IS_NUM(BIN_LEFT(vm)) || !IS_NUM(BIN_RIGHT(vm))) { \
            return BIN_OP_ERROR(vm, opString); \
        } \
        BOOL_BIN_OP(vm, resultBool); \
    } while (false)

/** An array of just integers to store the indices of each function in the call stack. */
DECLARE_DA_STRUCT(IntArray, int);

/**
 * From an integer array representing the stack trace + starting file,
 * retrace and compile the whole program and walk through the constant pool of each function
 * one by one in frameIndices in order to find the original one.
 */
static FuncObj *copy_errored_func(ZmxProgram *program, const IntArray framesIndices) {
    char *mainSource = alloc_source(program->currentFile->string);
    Compiler compiled = compile_source(program, mainSource, true);
    FuncObj *current = compiled.func;
    free_compiler(&compiled);
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
static IntArray copy_const_indices(const CallStack callStack) {
    IntArray framesIndices = CREATE_DA();
    for (u32 i = 1; i < callStack.length; i++) {
        APPEND_DA(&framesIndices, callStack.data[i].func->constIdx);
    }
    return framesIndices;
}

/** Frees the passed program's content and sets it to a fresh program with the same main name. */
static void reset_program(ZmxProgram *program) {
    char *mainName = ARRAY_ALLOC(program->mainFile->length + 1, char);
    strncpy(mainName, program->mainFile->string, program->mainFile->length);
    mainName[program->mainFile->length] = '\0';
    Gc gc = program->gc;
    free_zmx_program(program);
    *program = create_zmx_program(mainName, true);
    program->gc = gc;
    free(mainName);
}

/** 
 * Frees the passed VM and reinitializes it.
 * 
 * This is mostly for memory optimization before recompiling the program after it errored,
 * as the VM's allocated memory would be useless after the error.
 */
static void reset_vm(Vm *vm) {
    ZmxProgram *program = vm->program;
    vm->program->gc.protectNewObjs = true;
    program->gc.vm = NULL;
    free_vm(vm);
    *vm = create_vm(program, new_func_obj(program, new_string_obj(program, "<Error>"), -1));
    vm->program->gc.protectNewObjs = false;
    GC_CLEAR_PROTECTED(&vm->program->gc);
    program->gc.vm = vm;
}

/** 
 * Reports a runtime error.
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
static bool runtime_error(Vm *vm, const char *format, ...) {
    const u32 bytecodeIdx = vm->frame->ip - vm->frame->func->bytecode.data;
    IntArray framesIndices = copy_const_indices(vm->callStack);
    reset_program(vm->program);
    reset_vm(vm);
    
    FuncObj *erroredFunc = copy_errored_func(vm->program, framesIndices);
    FREE_DA(&framesIndices);
    va_list args;
    va_start(args, format);
    zmx_user_error(
        vm->program, erroredFunc->positions.data[bytecodeIdx], "Runtime error", format, &args
    );
    va_end(args);
    return false;
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
    INCREASE_CAPACITY(vm->stack.capacity);
    Obj **originalStack = vm->stack.objects;
    vm->stack.objects = REALLOC(
        vm->stack.objects, vm->stack.capacity * sizeof(*vm->stack.objects)
    );
    if (vm->stack.objects == originalStack) {
        // Stack address didn't change, no need to move pointers in the stack to the new location.
        return;
    }
    
    for (u32 i = 0; i < vm->callStack.length; i++) {
        vm->callStack.data[i].sp = &vm->stack.objects[vm->callStack.data[i].sp - originalStack];
        vm->callStack.data[i].bp = &vm->stack.objects[vm->callStack.data[i].bp - originalStack];
    }
}

/** Returns a stack frame to hold information about the currently executing function. */
static StackFrame create_stack_frame(FuncObj *func, Obj **sp) {
    StackFrame frame = {.func = func, .ip = func->bytecode.data, .sp = sp, .bp = sp};
    return frame;
}

/** Returns a VM initialized from the passed func. */
Vm create_vm(ZmxProgram *program, FuncObj *func) {
    Vm vm = {
        .program = program, .instrSize = INSTR_ONE_BYTE, .globals = create_table(),
        .callStack = CREATE_DA(), .frame = NULL, .stack = CREATE_STACK()
    };

    // Set current frame and its SP after their arrays have been initialized.
    APPEND_DA(&vm.callStack, create_stack_frame(func, vm.stack.objects));
    vm.frame = vm.callStack.data;
    return vm;
}

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm) {
    free_table(&vm->globals);
    FREE_DA(&vm->callStack);
    FREE_STACK(vm);
}

/** Attempts to call the passed object. Returns whether or not it managed to call it. */
static bool call(Vm *vm, Obj *callee) {
    switch (callee->type) {
    case OBJ_FUNC:
        return true; // TODO: add actual code for calling funcs once the user can write them.

    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_STRING:
    case OBJ_ITERATOR:
        return false;
    }
    UNREACHABLE_ERROR();
}

/** 
 * Executes all the bytecode in the passed VM's function object.
 * 
 * Returns whether or not executing the VM was successful (no errors).
 */
static bool execute_vm(Vm *vm) {
    while (true) {
#if DEBUG_RUNTIME
        print_runtime_state(
            vm->frame->func, vm->stack.objects, STACK_LENGTH(vm),
            vm->frame->ip - vm->frame->func->bytecode.data, vm->instrSize
        );
#endif
        const OpCode opcode = READ_INSTR(vm);
        switch (opcode) {
        case OP_LOAD_CONST:
            PUSH(vm, READ_CONST(vm));
            break;
        case OP_ARG_16: vm->instrSize = INSTR_TWO_BYTES; break;
        case OP_ARG_32: vm->instrSize = INSTR_FOUR_BYTES; break;
        case OP_TRUE: PUSH(vm, AS_OBJ(new_bool_obj(vm->program, true))); break;
        case OP_FALSE: PUSH(vm, AS_OBJ(new_bool_obj(vm->program, false))); break;
        case OP_NULL: PUSH(vm, AS_OBJ(new_null_obj(vm->program))); break;
        case OP_ADD:
            if (BIN_LEFT(vm)->type == OBJ_STRING && BIN_RIGHT(vm)->type == OBJ_STRING) {
                Obj *result = AS_OBJ(concatenate(
                    vm->program, AS_PTR(StringObj, BIN_LEFT(vm)), AS_PTR(StringObj, BIN_RIGHT(vm))
                ));
                DROP_AMOUNT(vm, 2);
                PUSH(vm, result);
            } else {
                MATH_BIN_OP(vm, "addition", NUM_VAL(BIN_LEFT(vm)) + NUM_VAL(BIN_RIGHT(vm)));
            }
            break;
        case OP_SUBTRACT:
            MATH_BIN_OP(vm, "subtraction", NUM_VAL(BIN_LEFT(vm)) - NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MULTIPLY:
            MATH_BIN_OP(vm, "multiplication", NUM_VAL(BIN_LEFT(vm)) * NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_DIVIDE:
            if (IS_NUM(BIN_LEFT(vm)) && IS_NUM(BIN_RIGHT(vm)) && NUM_VAL(BIN_RIGHT(vm)) == 0) {
                return runtime_error(vm, "Can't divide by 0.");
            }
            MATH_BIN_OP(vm, "division", NUM_VAL(BIN_LEFT(vm)) / NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MODULO:
            if (IS_NUM(BIN_LEFT(vm)) && IS_NUM(BIN_RIGHT(vm)) && NUM_VAL(BIN_RIGHT(vm)) == 0) {
                return runtime_error(vm, "Can't modulo by 0.");
            }
            // Modulo doesn't handle floats in C, so use fmod() if there's a float in the operation.
            MATH_BIN_OP(
                vm, "modulo",
                BIN_HAS_FLOAT(vm) ? fmod(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                    : AS_PTR(IntObj, BIN_LEFT(vm))->number % AS_PTR(IntObj, BIN_RIGHT(vm))->number
            );
            break;
        case OP_EXPONENT:
            MATH_BIN_OP(vm, "exponentiation", pow(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm))));
            break;
        case OP_EQ:
            BOOL_BIN_OP(vm, equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            break;
        case OP_NOT_EQ:
            BOOL_BIN_OP(vm, !equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            break;
        case OP_GREATER:
            NUM_BOOL_BIN_OP(vm, "comparison", NUM_VAL(BIN_LEFT(vm)) > NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_GREATER_EQ:
            NUM_BOOL_BIN_OP(vm, "comparison", NUM_VAL(BIN_LEFT(vm)) >= NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_LESS:
            NUM_BOOL_BIN_OP(vm, "comparison", NUM_VAL(BIN_LEFT(vm)) < NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_LESS_EQ:
            NUM_BOOL_BIN_OP(vm, "comparison", NUM_VAL(BIN_LEFT(vm)) >= NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MINUS:
            if (PEEK(vm)->type == OBJ_INT) {
                const ZmxInt negated = -AS_PTR(IntObj, POP(vm))->number;
                PUSH(vm, AS_OBJ(new_int_obj(vm->program, negated)));
            } else if (PEEK(vm)->type == OBJ_FLOAT) {
                const ZmxFloat negated = -AS_PTR(FloatObj, POP(vm))->number;
                PUSH(vm, AS_OBJ(new_float_obj(vm->program, negated)));
            } else {
                return runtime_error(
                    vm, "Can't negate object of type %s.", obj_type_string(PEEK(vm)->type)
                );
            }
            break;
        case OP_NOT: {
            BoolObj *asBool = new_bool_obj(vm->program, !as_bool(vm->program, POP(vm))->boolean);
            PUSH(vm, AS_OBJ(asBool));
            break;
        }
        case OP_AS: {
            Obj *converted;
            const DataType conversionType = READ_NUMBER(vm);
            switch (conversionType) {
            case TYPE_BOOL: converted = AS_OBJ(as_bool(vm->program, PEEK(vm))); break;
            case TYPE_STRING: converted = AS_OBJ(as_string(vm->program, PEEK(vm))); break;
            // TODO: add as_int() and as_float() implementations once the "as" keyword is added.
            case TYPE_INT: UNREACHABLE_ERROR();
            case TYPE_FLOAT: UNREACHABLE_ERROR();
            TOGGLEABLE_DEFAULT_UNREACHABLE();
            }
            PEEK(vm) = AS_OBJ(converted);
            break;
        }
        case OP_MAKE_ITER: {
            if (!is_iterable(PEEK(vm))) {
                return runtime_error(
                    vm, "Can't iterate over object of type %s.", obj_type_string(PEEK(vm)->type)
                );
            }
            IteratorObj *iterator = new_iterator_obj(vm->program, PEEK(vm));
            PEEK(vm) = AS_OBJ(iterator);
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
        case OP_DECLARE_GLOBAL:
            table_set(&vm->globals, READ_CONST(vm), PEEK(vm));
            DROP(vm);
            break;
        case OP_ASSIGN_GLOBAL:
            table_set(&vm->globals, READ_CONST(vm), PEEK(vm));
            break;
        case OP_GET_GLOBAL: {
            Obj *value = table_get(&vm->globals, READ_CONST(vm));
            PUSH(vm, value);
            break;
        }
        case OP_ASSIGN_LOCAL:
            vm->frame->bp[READ_NUMBER(vm)] = PEEK(vm);
            break;
        case OP_GET_LOCAL: {
            Obj *value = vm->frame->bp[READ_NUMBER(vm)];
            PUSH(vm, value);
            break;
        }
        case OP_CALL: {
            const u32 argAmount = READ_NUMBER(vm);
            if (!call(vm, PEEK_DEPTH(vm, argAmount))) {
                return runtime_error(
                    vm, "Can't call object of type %s.", obj_type_string(PEEK(vm)->type)
                );
            }
            break;
        }
        case OP_JUMP: {
            const u32 jump = READ_NUMBER(vm);
            vm->frame->ip += jump;
            break;
        }
        case OP_JUMP_BACK: {
            const u32 jump = READ_NUMBER(vm);
            vm->frame->ip -= jump;
            break;
        }
        case OP_ITER_OR_JUMP: {
            ASSERT(PEEK(vm)->type == OBJ_ITERATOR, "Must iterate with an iterator.");
            IteratorObj *iterator = AS_PTR(IteratorObj, PEEK(vm));
            ASSERT(is_iterable(iterator->iterable),"Can't iterate over non-iterable.");

            const u32 jump = READ_NUMBER(vm);
            Obj *element = iterate(vm->program, iterator);
            if (element == NULL) {
                vm->frame->ip += jump;
            } else {
                PEEK_DEPTH(vm, 1) = element;
            }
            break;
        }
        case OP_POP_JUMP_IF_NOT: {
            const u32 jump = READ_NUMBER(vm);
            if (!(as_bool(vm->program, PEEK(vm))->boolean)) {
                vm->frame->ip += jump;
            }
            DROP(vm);
            break;
        }
        case OP_POP_JUMP_BACK_IF: {
            const u32 jump = READ_NUMBER(vm);
            if (as_bool(vm->program, PEEK(vm))->boolean) {
                vm->frame->ip -= jump;
            }
            DROP(vm);
            break;
        }
        case OP_POP:
            DROP(vm);
            break;
        case OP_POP_AMOUNT:
            DROP_AMOUNT(vm, READ_NUMBER(vm));
            break;
        case OP_END:
            return true;
        TOGGLEABLE_DEFAULT_UNREACHABLE();
        }
    }
}

/** 
 * Main function for interpreting a virtual machine.
 * 
 * Wrapper around execute_vm() for debugging.
 * Returns whether or not we encountered a runtime error during execution.
 */
bool interpret(Vm *vm) {
#if DEBUG_RUNTIME
    printf("-------------------- VM START --------------------\n");
#endif
    const bool succeeded = execute_vm(vm);
#if DEBUG_RUNTIME
    printf("-------------------- VM END --------------------\n");
#endif
    return succeeded;
}

/** Simply executes the passed source string and frees all used memory except the program's. */
bool interpret_source(ZmxProgram *program, char *source) {
    Compiler compiler = compile_source(program, source, DEBUG_BYTECODE);
    if (compiler.func == NULL) {
        return false; // Failed to compile.
    }
    Vm vm = create_vm(program, compiler.func);
    program->gc.vm = &vm;
    program->gc.compiler = NULL;
    free_compiler(&compiler);

    interpret(&vm);
    program->gc.vm = NULL;
    free_vm(&vm);
    return !program->hasErrored;
}
