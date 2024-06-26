#include <stdio.h>

#include "char_buffer.h"
#include "debug_bytecode.h"

/** Resolves to whether or not the func's bytecode has stored its bytecode's positions. */
#define HAS_POS_INFO(funcObj) ((funcObj)->positions.data != NULL)

/** 
 * Prints a "bare" instruction.
 * It is an instruction that doesn't have any numbers, consts or anything after it. Just one byte.
 */
static void print_bare_instr(const char *instrName, u32 *idx) {
    printf("%s", instrName);
    *idx += 1;
}

/** Prints an instruction that has a number after. Returns the read number as well. */
static u32 print_number_instr(
    const char *instrName, const FuncObj *func, u32 *idx, InstrSize *size
) {
    print_bare_instr(instrName, idx);
    *idx += *size;
    const u32 number = read_number(func, *idx - *size, size);
    printf(" %u", number);
    return number;
}

/** Prints an instruction that is followed by a data type enum. */
static void print_data_type_instr(
    const char *instrName, const FuncObj *func, u32 *idx, InstrSize *size
) {
    DataType dataType = print_number_instr(instrName, func, idx, size);
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
    const char *instrName, const FuncObj *func, u32 *idx, InstrSize *size
) {
    const u32 constIdx = print_number_instr(instrName, func, idx, size);
    printf(" (");
    print_obj(func->constPool.data[constIdx], true);
    putchar(')');
}

/** Prints a single instruction depending on format, and modifies idx's value accordingly. */
u32 print_instr(const FuncObj *func, u32 idx, InstrSize *size, const InstrFormat format) {
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
        break;
    default:
        UNREACHABLE_ERROR();
    }

    switch (func->bytecode.data[idx]) {
    case OP_LOAD_CONST: print_const_instr("LOAD_CONST", func, &idx, size); break;
    case OP_ARG_16: print_bare_instr("ARG_16", &idx); *size = INSTR_TWO_BYTES; break;
    case OP_ARG_32: print_bare_instr("ARG_32", &idx); *size = INSTR_FOUR_BYTES; break;
    case OP_TRUE: print_bare_instr("TRUE", &idx); break;
    case OP_FALSE: print_bare_instr("FALSE", &idx); break;
    case OP_NULL: print_bare_instr("NULL", &idx); break;
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
    case OP_AS: print_data_type_instr("AS", func, &idx, size); break;
    case OP_FINISH_STRING: print_number_instr("FINISH_STRING", func, &idx, size); break;
    case OP_DECLARE_GLOBAL: print_const_instr("DECLARE_GLOBAL", func, &idx, size); break;
    case OP_ASSIGN_GLOBAL: print_const_instr("ASSIGN_GLOBAL", func, &idx, size); break;
    case OP_GET_GLOBAL: print_const_instr("GET_GLOBAL", func, &idx, size); break;
    case OP_ASSIGN_LOCAL: print_number_instr("ASSIGN_LOCAL", func, &idx, size); break;
    case OP_GET_LOCAL: print_number_instr("GET_LOCAL", func, &idx, size); break;
    case OP_POP: print_bare_instr("POP", &idx); break;
    case OP_END: print_bare_instr("END", &idx); break;
    default: UNREACHABLE_ERROR();
    }
    putchar('\n');
    return idx;
}

static void print_separator(const FuncObj *func) {
    if (HAS_POS_INFO(func)) {
        printf("----------");
    }
    printf("--------------------\n");
}

/** 
 * Prints all the bytecode in the passed func.
 * 
 * Only prints line if source positions array is empty.
 * It could be NULL in the case that the bytecode array is simple empty,
 * but in that the loop won't end up triggering and printing anything anyways.
 */
void print_bytecode(const FuncObj *func) {
    print_separator(func);
    print_obj(AS_OBJ(func), true);
    putchar('\n');
    print_separator(func);
    if (HAS_POS_INFO(func)) {
        printf("Line    | ");
    }
    printf("Instruction | OpCode\n");
    print_separator(func);

    InstrSize size = INSTR_ONE_BYTE;
    // Loop doesn't increment the index, as print_instr()'s return does that instead.
    for (u32 idx = 0; idx < func->bytecode.length;) {
        idx = print_instr(func, idx, &size, HAS_POS_INFO(func) ? INSTR_NORMAL : INSTR_NO_LINE);
    }
}
