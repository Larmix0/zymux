#include <stdlib.h>

#include "debug_bytecode.h"
#include "debug_runtime.h"
#include "emitter.h"
#include "vm.h"

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

#define BIN_LEFT(vm) (PEEK_DEPTH(vm, 1)) /** Left hand side object of a binary in the stack. */
#define BIN_RIGHT(vm) (PEEK_DEPTH(vm, 0)) /** Right hand side object of a binary in the stack. */

// TODO: ensure that we don't blindly cast to a float object if it's not an integer object.
/** Returns the number the passed object holds, assuming it's either an int, or float object. */
#define NUM_VAL(obj) \
    ((obj)->type == OBJ_INT ? AS_PTR(IntObj, obj)->number : AS_PTR(FloatObj, obj)->number)

/** Creates a float from an operation done on 2 values, one of which at least is a float. */
#define FLOAT_FROM_BIN_OP(vm, left, op, right) \
    (new_float_obj( \
        (vm)->program, ((ZmxFloat)NUM_VAL(left) op (ZmxFloat)NUM_VAL(right)) \
    ))

/** Creates an integer object from an operation done on 2 integer objects. */
#define INT_FROM_BIN_OP(vm, left, op, right) \
    (new_int_obj((vm)->program, NUM_VAL(left) op NUM_VAL(right)))

// TODO: perform a check that the top 2 elements of the stack are even integers or floats.
/** Applies the passed op as a binary operation to the last 2 values of the stack, pushes result. */
#define BIN_OP(vm, op) \
    do { \
        if (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT) { \
            FloatObj *result = FLOAT_FROM_BIN_OP(vm, BIN_LEFT(vm), op, BIN_RIGHT(vm)); \
            DROP_AMOUNT(vm, 2); \
            PUSH(vm, AS_OBJ(result)); \
        } else { \
            IntObj *result = INT_FROM_BIN_OP(vm, BIN_LEFT(vm), op, BIN_RIGHT(vm)); \
            DROP_AMOUNT(vm, 2); \
            PUSH(vm, AS_OBJ(result)); \
        } \
    } while (false)

/** Binary operation that pops the last 2 values and pushes the resulting bool value from op. */
#define BIN_OP_BOOL(vm, op) \
    do { \
        BoolObj *result = new_bool_obj( \
            vm->program, NUM_VAL(BIN_LEFT(vm)) op NUM_VAL(BIN_RIGHT(vm)) \
        ); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, AS_OBJ(result)); \
    } while (false)

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
    vm->stack.capacity = vm->stack.capacity < MIN_DA_CAP ? MIN_DA_CAP : (vm)->stack.capacity * 2;
    Obj **previousStackAddr = vm->stack.objects;
    vm->stack.objects = ZMX_REALLOC(
        vm->stack.objects, vm->stack.capacity * sizeof(*vm->stack.objects)
    );
    if (vm->stack.objects == previousStackAddr) {
        // Stack address didn't change, no need to move pointers in the stack to a new location.
        return;
    }
    
    for (u32 i = 0; i < vm->callStack.length; i++) {
        vm->callStack.data[i].sp = &vm->stack.objects[vm->callStack.data[i].sp - previousStackAddr];
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
        case OP_ADD: BIN_OP(vm, +); break;
        case OP_SUBTRACT: BIN_OP(vm, -); break;
        case OP_MULTIPLY: BIN_OP(vm, *); break;
        case OP_DIVIDE: BIN_OP(vm, /); break;
        case OP_MODULO: {
            if (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT) {
                FloatObj *result = new_float_obj(
                    vm->program, zmx_float_modulo(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_OBJ(result));
            } else {
                IntObj *result = new_int_obj(
                    vm->program,
                    AS_PTR(IntObj, (BIN_LEFT(vm)))->number % AS_PTR(IntObj, (BIN_RIGHT(vm)))->number
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_OBJ(result));
            }
            break;
        }
        case OP_EXPONENT: {
            if (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT) {
                FloatObj *result = new_float_obj(
                    vm->program, zmx_float_power(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_OBJ(result));
            } else {
                IntObj *result = new_int_obj(
                    vm->program, zmx_int_power(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_OBJ(result));
            }
            break;
        }
        case OP_EQ: {
            BoolObj *result = new_bool_obj(vm->program, equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            DROP_AMOUNT(vm, 2);
            PUSH(vm, AS_OBJ(result));
            break;
        }
        case OP_NOT_EQ: {
            BoolObj *result = new_bool_obj(vm->program, !equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            DROP_AMOUNT(vm, 2);
            PUSH(vm, AS_OBJ(result));
            break;
        }
        case OP_GREATER: BIN_OP_BOOL(vm, >); break;
        case OP_GREATER_EQ: BIN_OP_BOOL(vm, >=); break;
        case OP_LESS: BIN_OP_BOOL(vm, <); break;
        case OP_LESS_EQ: BIN_OP_BOOL(vm, <=); break;
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
            BoolObj *asBool = as_bool(vm->program, POP(vm));
            asBool->boolean = asBool->boolean ? false : true; // Simply reverse after converting.
            PUSH(vm, AS_OBJ(asBool));
            break;
        }
        case OP_AS: {
            const Obj *original = POP(vm);
            switch (READ_NUMBER(vm)) {
            // TODO: implement other types.
            case TYPE_BOOL: PUSH(vm, AS_OBJ(as_bool(vm->program, original))); break;
            case TYPE_STRING: PUSH(vm, AS_OBJ(as_string(vm->program, original))); break;
            default: UNREACHABLE_ERROR(); break;
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