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
        print_obj(stack[i]);
        printf("}");
    }
}

/** Prints the current instruction and the stack */
void print_runtime_state(
    FuncObj *func, Obj **stack, const int stackLength, const int bytecodeOffset
) {
    print_instr(func, bytecodeOffset, INSTR_NO_LINE_OR_PAD);
    printf("- ");
    if (stackLength == 0) {
        printf("<EMPTY STACK>");
    } else {
        print_stack(stack, stackLength);
    }
    putchar('\n');
}
