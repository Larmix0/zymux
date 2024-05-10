#include <stdlib.h>

#include "debug_bytecode.h"
#include "debug_runtime.h"
#include "emitter.h"
#include "vm.h"

#define MIN_STACK_SIZE 256

#define NEXT_INSTRUCTION(vm) (*(vm)->frame->ip++)
#define READ_NUMBER(vm) \
    (read_number((vm)->frame->func, (vm)->frame->ip++ - (vm)->frame->func->bytecode.data))
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
#define PEEK(vm) (*(vm->frame->sp - 1))
#define FREE_STACK(stack) (free((stack)->objects))

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
        case OP_ADD: {
            Obj *right = POP(vm);
            Obj *left = POP(vm);
            IntObj *result = new_int_obj(
                vm->program, AS_PTR(left, IntObj)->integer + AS_PTR(right, IntObj)->integer
            );
            PUSH(vm, AS_PTR(result, Obj));
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
