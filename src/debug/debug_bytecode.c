#include "stdio.h"

#include "char_buffer.h"
#include "debug_bytecode.h"
#include "emitter.h"

/** Resolves to whether or not the func's bytecode has stored its bytecode's positions. */
#define HAS_POS_INFO(funcObj) ((funcObj)->positions.data != NULL)

/** 
 * Prints a "bare" instruction.
 * It is an instruction that doesn't have any numbers, consts or anything after it. Just one byte.
 */
static void print_bare_instr(const char *instrName, int *idx) {
    printf("%s", instrName);
    *idx += 1;
}

/** Prints an instruction that has a number after. Returns the read number as well. */
static int print_number_instr(const char *instrName, FuncObj *func, int *idx) {
    print_bare_instr(instrName, idx);
    const int number = read_number(func, *idx);
    printf(" %d", number);
    *idx += 1; // TODO: Change this to incrementing depending on instrSize.
    return number;
}

/** Prints an instruction that is followed by a data type enum. */
static void print_data_type_instr(const char *instrName, FuncObj *func, int *idx) {
    DataType dataType = print_number_instr(instrName, func, idx);
    printf(" (");
    switch (dataType) {
    case TYPE_INT: printf("int"); break;
    case TYPE_FLOAT: printf("float"); break;
    case TYPE_BOOL: printf("bool"); break;
    case TYPE_STRING: printf("string"); break;
    default: UNREACHABLE_ERROR();
    }
    putchar(')');
}

/** Prints an instruction that has a constant object immediately after. */
static void print_const_instr(
    const char *instrName, FuncObj *func, int *idx
) {
    const int constIdx = print_number_instr(instrName, func, idx);
    printf(" (");
    print_obj(func->constPool.data[constIdx], true);
    putchar(')');
}

/** Prints a single instruction depending on format, and modifies idx's value accordingly. */
int print_instr(FuncObj *func, int idx, InstrFormat format) {
    switch (format) {
    case INSTR_NORMAL:
        printf("%-10d", func->positions.data[idx].line);
        printf("%11d   ", idx);
        break;
    case INSTR_NO_LINE:
        printf("%11d   ", idx);
        break;
    case INSTR_NO_LINE_OR_PAD:
        printf("%d ", idx);
    }

    switch (func->bytecode.data[idx]) {
    case OP_LOAD_CONST: print_const_instr("LOAD_CONST", func, &idx); break;
    case OP_TRUE: print_bare_instr("TRUE", &idx); break;
    case OP_FALSE: print_bare_instr("FALSE", &idx); break;
    case OP_ADD: print_bare_instr("ADD", &idx); break;
    case OP_SUBTRACT: print_bare_instr("SUBTRACT", &idx); break;
    case OP_MULTIPLY: print_bare_instr("MULTIPLY", &idx); break;
    case OP_DIVIDE: print_bare_instr("DIVIDE", &idx); break;
    case OP_MODULO: print_bare_instr("MODULO", &idx); break;
    case OP_EXPONENT: print_bare_instr("EXPONENT", &idx); break;
    case OP_EQ: print_bare_instr("EQUAL", &idx); break;
    case OP_NOT_EQ: print_bare_instr("NOT_EQUAL", &idx); break;
    case OP_GREATER: print_bare_instr("GREATER", &idx); break;
    case OP_GREATER_EQ: print_bare_instr("GREATER_EQUAL", &idx); break;
    case OP_LESS: print_bare_instr("LESS", &idx); break;
    case OP_LESS_EQ: print_bare_instr("LESS_EQUAL", &idx); break;
    case OP_MINUS: print_bare_instr("MINUS", &idx); break;
    case OP_NOT: print_bare_instr("NOT", &idx); break;
    case OP_AS: print_data_type_instr("AS", func, &idx); break;
    case OP_FINISH_STRING: print_number_instr("FINISH_STRING", func, &idx); break;
    case OP_POP: print_bare_instr("POP", &idx); break;
    case OP_END: print_bare_instr("END", &idx); break;
    }
    putchar('\n');
    return idx;
}

/** 
 * Prints all the bytecode in the passed func.
 * 
 * Only prints line if source positions array is empty.
 * It could be NULL in the case that the bytecode array is simple empty,
 * but in that the loop won't end up triggering and printing anything anyways.
 */
void print_bytecode(FuncObj *func) {
    // TODO: show the name of the func when printing once string objects are added.
    if (HAS_POS_INFO(func)) {
        printf("Line    | ");
    }
    printf("Instruction | OpCode\n");

    if (HAS_POS_INFO(func)) {
        printf("----------");
    }
    printf("--------------------\n");

    // Loop doesn't increment the index, as print_instr()'s return does that instead.
    for (int idx = 0; idx < func->bytecode.length;) {
        idx = print_instr(func, idx, HAS_POS_INFO(func) ? INSTR_NORMAL : INSTR_NO_LINE);
    }
}
