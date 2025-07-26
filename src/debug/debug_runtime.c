#include <stdio.h>

#include "debug_bytecode.h"
#include "debug_runtime.h"
#include "program.h"
#include "vm.h"

/** Prints the entirety of the passed stack. */
static void print_stack(Obj **stack, const int stackLength) {
    for (int i = 0; i < stackLength; i++) {
        if (i != 0) {
            printf(" | ");
        }
        printf("{");
        print_obj(stack[i], true);
        printf("}");
    }
}

/** Prints for a runtime debug line that starts with TID (thread ID) information. */
static void print_tid(const u32 tid) {
    printf("[TID %" PRIu32 "] - ", tid);
}

/** Prints the current instruction and the stack */
void print_runtime_state(
    ThreadObj *thread, const FuncObj *func, Obj **stack, const int stackLength,
    const u32 bytecodeOffset, InstrSize size
) {
    mutex_lock(&thread->vm->program->printLock);
    print_tid(thread->id);
    if (stackLength == 0) {
        printf("<EMPTY STACK>");
    } else {
        print_stack(stack, stackLength);
    }
    putchar('\n');

    print_tid(thread->id);
    print_instr(func, bytecodeOffset, &size, FORMAT_NO_LINE_OR_PAD);
    mutex_unlock(&thread->vm->program->printLock);
}

/** Prints information about a caught runtime error, its effects, and the error message . */
void print_caught_runtime_error(
    ThreadObj *thread, FuncObj *setFunc, const u32 poppedLocals, const u32 poppedCaptures,
    const u32 poppedOpenCaptures, const char *errorMessage
) {
    mutex_lock(&thread->vm->program->printLock);
    printf("RUNTIME ERROR CAUGHT:\n" INDENT);
    printf("Error message: %s\n" INDENT, errorMessage);
    printf(
        "TID %" PRIu32 " Going back to %s: popping %" PRIu32 " locals, "
        "%" PRIu32 " captures (%" PRIu32 " open).\n",
        thread->id, as_string(&thread->vulnObjs, AS_OBJ(setFunc))->string,
        poppedLocals, poppedCaptures, poppedOpenCaptures
    );
    mutex_unlock(&thread->vm->program->printLock);
}
