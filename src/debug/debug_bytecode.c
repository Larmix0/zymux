#include "stdio.h"

#include "char_buffer.h"
#include "debug_bytecode.h"
#include "emitter.h"

/** Resolves to whether or not the function's bytecode has stored its bytecode's positions. */
#define HAS_POS_INFO(funcObj) (function->positions.data != NULL)

/** 
 * Prints a "bare" instruction.
 * It is an instruction that doesn't have any numbers, consts or anything after it. Just one byte.
 */
static void print_bare_instr(const char *instrName, int *idx) {
    printf("%s", instrName);
    *idx += 1;
}

/** Prints an instruction that has a constant object immediately after. */
static void print_const_instr(
    const char *instrName, FuncObj *function, int *idx
) {
    print_bare_instr(instrName, idx);
    const int constIdx = read_number(function, *idx);
    printf(" %d (", constIdx);
    *idx += 1; // TODO: change this to incrementing depending on instrSize.

    print_obj(function->constPool.data[constIdx]);
    putchar(')');
}

/** Prints a single instruction depending on format, and modifies idx's value accordingly. */
int print_instr(FuncObj *function, int idx, InstrFormat format) {
    switch (format) {
    case INSTR_NORMAL:
        printf("%-10d", function->positions.data[idx].line);
        printf("%11d   ", idx);
        break;
    case INSTR_NO_LINE:
        printf("%11d   ", idx);
        break;
    case INSTR_NO_LINE_OR_PAD:
        printf("%d ", idx);
    }

    switch (function->bytecode.data[idx]) {
    case OP_LOAD_CONST: print_const_instr("LOAD_CONST", function, &idx); break;
    case OP_ADD: print_bare_instr("ADD", &idx); break;
    case OP_SUBTRACT: print_bare_instr("SUBTRACT", &idx); break;
    case OP_MULTIPLY: print_bare_instr("MULTIPLY", &idx); break;
    case OP_DIVIDE: print_bare_instr("DIVIDE", &idx); break;
    case OP_MODULO: print_bare_instr("MODULO", &idx); break;
    case OP_EXPONENT: print_bare_instr("EXPONENT", &idx); break;
    case OP_MINUS: print_bare_instr("MINUS", &idx); break;
    case OP_NOT: print_bare_instr("NOT", &idx); break;
    case OP_POP: print_bare_instr("POP", &idx); break;
    case OP_END: print_bare_instr("END", &idx); break;
    }
    putchar('\n');
    return idx;
}

/** 
 * Prints all the bytecode in the passed function.
 * 
 * Only prints line if source positions array is empty.
 * It could be NULL in the case that the bytecode array is simple empty,
 * but in that the loop won't end up triggering and printing anything anyways.
 */
void print_bytecode(FuncObj *function) {
    // TODO: show the name of the function when printing once string objects are added.
    if (HAS_POS_INFO(function)) {
        printf("Line    | ");
    }
    printf("Instruction | OpCode\n");

    if (HAS_POS_INFO(function)) {
        printf("----------");
    }
    printf("--------------------\n");

    // Loop doesn't increment the index, as print_instr()'s return does that instead.
    for (int idx = 0; idx < function->bytecode.length;) {
        idx = print_instr(function, idx, HAS_POS_INFO(function) ? INSTR_NORMAL : INSTR_NO_LINE);
    }
}
