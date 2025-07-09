#include <stdio.h>

#include "debug_bytecode.h"
#include "debug_runtime.h"

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

/** Prints the current instruction and the stack */
void print_runtime_state(
    const FuncObj *func, Obj **stack, const int stackLength,
    const u32 bytecodeOffset, InstrSize size
) {
    printf("- ");
    if (stackLength == 0) {
        printf("<EMPTY STACK>");
    } else {
        print_stack(stack, stackLength);
    }
    putchar('\n');
    print_instr(func, bytecodeOffset, &size, FORMAT_NO_LINE_OR_PAD);
}

/** Prints information about a caught runtime error, its effects, and the error message . */
void print_caught_runtime_error(
    VulnerableObjs *vulnObjs, FuncObj *setFunc, const u32 poppedLocals, const u32 poppedCaptures,
    const u32 poppedOpenCaptures, const char *errorMessage
) {
    printf("RUNTIME ERROR CAUGHT:\n" INDENT);
    printf("Error message: %s\n" INDENT, errorMessage);
    printf(
        "Going back to %s: popping %"PRIu32" locals, %"PRIu32" captures (%"PRIu32" open).\n",
        as_string(vulnObjs, AS_OBJ(setFunc))->string, poppedLocals, poppedCaptures,
        poppedOpenCaptures
    );
}
