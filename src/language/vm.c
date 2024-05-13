#include <stdlib.h>

#include "debug_bytecode.h"
#include "debug_runtime.h"
#include "emitter.h"
#include "vm.h"

#define MIN_STACK_SIZE 16

/** Reads the current instruction and increments IP to prepare for the next one. */
#define NEXT_INSTRUCTION(vm) (*(vm)->frame->ip++)

// TODO: turn the read_number function fully to a macro for performance?
// Pretty considerable since it's also used by the const reader.
/** Reads the upcoming number in the bytecode. Prefer this over the function in the VM. */
#define READ_NUMBER(vm) \
    (read_number((vm)->frame->func, (vm)->frame->ip++ - (vm)->frame->func->bytecode.data))

/** Reads the upcoming number and uses it to index an object in the current constant pool. */
#define READ_CONST(vm) ((vm)->frame->func->constPool.data[READ_NUMBER((vm))])

#define CREATE_STACK() {.objects = NULL, .capacity = 0}
#define STACK_LENGTH(vm) ((vm)->frame->sp - vm->stack.objects)
#define PUSH(vm, obj) \
    do { \
        if ((vm)->stack.capacity < STACK_LENGTH(vm) + 1) { \
            (vm)->stack.capacity = (vm)->stack.capacity < MIN_STACK_SIZE \
                ? MIN_STACK_SIZE : (vm)->stack.capacity * 2; \
            reallocate_stack((vm)); \
        } \
        *(vm)->frame->sp++ = (obj); \
    } while (false)
#define POP(vm) (*(--(vm)->frame->sp))
#define DROP(vm) (--(vm)->frame->sp)
#define DROP_AMOUNT(vm, amount) ((vm)->frame->sp -= (amount))
#define PEEK(vm) (*((vm)->frame->sp - 1))
#define PEEK_DEPTH(vm, depth) (*((vm)->frame->sp - (depth) - 1))
#define FREE_STACK(stack) (free((stack)->objects))

#define BIN_LEFT(vm) (PEEK_DEPTH((vm), 1)) /** Left hand side object of a binary in the stack. */
#define BIN_RIGHT(vm) (PEEK_DEPTH((vm), 0)) /** Right hand side object of a binary in the stack. */

/** Returns the number the passed object holds, assuming it's either an int, or float object. */
#define NUM_VAL(obj) \
    ((obj)->type == OBJ_INT ? AS_PTR((obj), IntObj)->number : AS_PTR((obj), FloatObj)->number)

/** Creates a float from an operation done on 2 values, one of which at least is a float. */
#define FLOAT_FROM_BIN_OP(vm, left, op, right) \
    (new_float_obj( \
        (vm)->program, ((ZmxFloat)NUM_VAL((left)) op (ZmxFloat)NUM_VAL((right))) \
    ))

/** Creates an integer object from an operation done on 2 integer objects. */
#define INT_FROM_BIN_OP(vm, left, op, right) \
    (new_int_obj((vm)->program, NUM_VAL((left)) op NUM_VAL((right))))

/** Applies the passed op as a binary operation to the last 2 values of the stack, pushes result. */
#define BIN_OP(vm, op) \
    do { \
        if (BIN_LEFT((vm))->type == OBJ_FLOAT || BIN_RIGHT((vm))->type == OBJ_FLOAT) { \
            FloatObj *result = FLOAT_FROM_BIN_OP((vm), BIN_LEFT((vm)), op, BIN_RIGHT((vm))); \
            DROP_AMOUNT((vm), 2); \
            PUSH(vm, AS_PTR(result, Obj)); \
        } else { \
            IntObj *result = INT_FROM_BIN_OP((vm), BIN_LEFT((vm)), op, BIN_RIGHT((vm))); \
            DROP_AMOUNT((vm), 2); \
            PUSH(vm, AS_PTR(result, Obj)); \
        } \
    } while (false)

/** Binary operation that pops the last 2 values and pushes the resulting bool value from op. */
#define BIN_OP_BOOL(vm, op) \
    do { \
        BoolObj *result = new_bool_obj( \
            vm->program, NUM_VAL(BIN_LEFT((vm))) op NUM_VAL(BIN_RIGHT((vm))) \
        ); \
        DROP_AMOUNT((vm), 2); \
        PUSH(vm, AS_PTR(result, Obj)); \
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
    Obj **previousStackAddr = vm->stack.objects;
    vm->stack.objects = ZMX_REALLOC(
        vm->stack.objects, vm->stack.capacity * sizeof(*vm->stack.objects)
    );
    if (vm->stack.objects == previousStackAddr) {
        // Stack address didn't change, no need to move pointers in the stack to a new location.
        return;
    }
    
    for (int i = 0; i < vm->callStack.length; i++) {
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
    Vm vm = {.program = program, .callStack = CREATE_DA(), .frame = NULL, .stack = CREATE_STACK()};

    // Set current frame and its SP after their arrays have been initialized.
    APPEND_DA(&vm.callStack, create_frame(func, vm.stack.objects));
    vm.frame = vm.callStack.data;
    return vm;
}

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm) {
    FREE_DA(&vm->callStack);
    FREE_STACK(&vm->stack);
}

/** Executes all the bytecode in the passed VM's function object. */
bool interpret(Vm *vm) {
    while (true) {
#if DEBUG_RUNTIME == 1
        print_runtime_state(
            vm->frame->func, vm->stack.objects, STACK_LENGTH(vm),
            vm->frame->ip - vm->frame->func->bytecode.data
        );
#endif
        switch (NEXT_INSTRUCTION(vm)) {
        case OP_LOAD_CONST:
            PUSH(vm, READ_CONST(vm));
            break;
        case OP_TRUE: PUSH(vm, AS_PTR(new_bool_obj(vm->program, true), Obj)); break;
        case OP_FALSE: PUSH(vm, AS_PTR(new_bool_obj(vm->program, false), Obj)); break;
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
                PUSH(vm, AS_PTR(result, Obj));
            } else {
                IntObj *result = new_int_obj(
                    vm->program,
                    AS_PTR((BIN_LEFT(vm)), IntObj)->number % AS_PTR((BIN_RIGHT(vm)), IntObj)->number
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_PTR(result, Obj));
            }
            break;
        }
        case OP_EXPONENT: {
            if (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT) {
                FloatObj *result = new_float_obj(
                    vm->program, zmx_float_power(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_PTR(result, Obj));
            } else {
                IntObj *result = new_int_obj(
                    vm->program, zmx_int_power(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                );
                DROP_AMOUNT(vm, 2);
                PUSH(vm, AS_PTR(result, Obj));
            }
            break;
        }
        case OP_EQ: {
            BoolObj *result = new_bool_obj(vm->program, equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            DROP_AMOUNT(vm, 2);
            PUSH(vm, AS_PTR(result, Obj));
            break;
        }
        case OP_NOT_EQ: {
            BoolObj *result = new_bool_obj(vm->program, !equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            DROP_AMOUNT(vm, 2);
            PUSH(vm, AS_PTR(result, Obj));
            break;
        }
        case OP_GREATER: BIN_OP_BOOL(vm, >); break;
        case OP_GREATER_EQ: BIN_OP_BOOL(vm, >=); break;
        case OP_LESS: BIN_OP_BOOL(vm, <); break;
        case OP_LESS_EQ: BIN_OP_BOOL(vm, <=); break;
        case OP_MINUS:
            if (PEEK(vm)->type == OBJ_INT) {
                ZmxInt negated = -AS_PTR(POP(vm), IntObj)->number;
                PUSH(vm, AS_PTR(new_int_obj(vm->program, negated), Obj));
            } else if (PEEK(vm)->type == OBJ_FLOAT) {
                ZmxFloat negated = -AS_PTR(POP(vm), FloatObj)->number;
                PUSH(vm, AS_PTR(new_float_obj(vm->program, negated), Obj));
            }
            break;
        case OP_NOT: {
            BoolObj *asBool = as_bool(vm->program, POP(vm));
            asBool->boolean = asBool->boolean ? false : true; // Simply reverse after converting.
            PUSH(vm, AS_PTR(asBool, Obj));
            break;
        }
        case OP_AS: {
            Obj *original = POP(vm);
            switch (READ_NUMBER(vm)) {
            // TODO: implement other types.
            case TYPE_BOOL: PUSH(vm, AS_PTR(as_bool(vm->program, original), Obj)); break;
            case TYPE_STRING: PUSH(vm, AS_PTR(as_string(vm->program, original), Obj)); break;
            default: UNREACHABLE_ERROR(); break;
            }
            break;
        }
        case OP_FINISH_STRING: {
            const u32 amount = READ_NUMBER(vm);

            // Build a string from the deepest till the highest/newest one in the stack.
            StringObj *string = new_string_obj(vm->program, "");
            for (int i = amount - 1; i >= 0; i--) {
                string = concatenate(
                    vm->program, string, AS_PTR(PEEK_DEPTH(vm, i), StringObj)
                );
            }
            DROP_AMOUNT(vm, amount);
            PUSH(vm, AS_PTR(string, Obj));
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
