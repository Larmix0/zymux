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
    const u32 number = read_number(&func->bytecode, *idx - *size, size);
    printf(" %"PRIu32, number);
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
    TOGGLEABLE_DEFAULT_UNREACHABLE();
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
u32 print_instr(const FuncObj *func, u32 idx, InstrSize *size, const BytecodeFormat format) {
    switch (format) {
    case FORMAT_NORMAL:
        printf("%-10d", func->positions.data[idx].line);
        printf("%11d   ", idx);
        break;
    case FORMAT_NO_LINE:
        printf("%11d   ", idx);
        break;
    case FORMAT_NO_LINE_OR_PAD:
        printf("%d ", idx);
        break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }

    const OpCode opcode = func->bytecode.data[idx];
    switch (opcode) {
    case OP_LOAD_CONST: print_const_instr("LOAD_CONST", func, &idx, size); break;
    case OP_ARG_16: print_bare_instr("ARG_16", &idx); *size = INSTR_TWO_BYTES; break;
    case OP_ARG_32: print_bare_instr("ARG_32", &idx); *size = INSTR_FOUR_BYTES; break;
    case OP_TRUE: print_bare_instr("TRUE", &idx); break;
    case OP_FALSE: print_bare_instr("FALSE", &idx); break;
    case OP_NULL: print_bare_instr("NULL", &idx); break;
    case OP_RANGE: print_bare_instr("RANGE", &idx); break;
    case OP_ADD: print_bare_instr("ADD", &idx); break;
    case OP_SUBTRACT: print_bare_instr("SUBTRACT", &idx); break;
    case OP_MULTIPLY: print_bare_instr("MULTIPLY", &idx); break;
    case OP_DIVIDE: print_bare_instr("DIVIDE", &idx); break;
    case OP_MODULO: print_bare_instr("MODULO", &idx); break;
    case OP_EXPONENT: print_bare_instr("EXPONENT", &idx); break;
    case OP_LSHIFT: print_bare_instr("LSHIFT", &idx); break;
    case OP_RSHIFT: print_bare_instr("RSHIFT", &idx); break;
    case OP_BITWISE_OR: print_bare_instr("BITWISE_OR", &idx); break;
    case OP_BITWISE_AND: print_bare_instr("BITWISE_AND", &idx); break;
    case OP_XOR: print_bare_instr("XOR", &idx); break;
    case OP_EQUAL: print_bare_instr("EQUAL", &idx); break;
    case OP_NOT_EQUAL: print_bare_instr("NOT_EQUAL", &idx); break;
    case OP_GREATER: print_bare_instr("GREATER", &idx); break;
    case OP_GREATER_EQ: print_bare_instr("GREATER_EQ", &idx); break;
    case OP_LESS: print_bare_instr("LESS", &idx); break;
    case OP_LESS_EQ: print_bare_instr("LESS_EQ", &idx); break;
    case OP_MINUS: print_bare_instr("MINUS", &idx); break;
    case OP_NOT: print_bare_instr("NOT", &idx); break;
    case OP_TILDE: print_bare_instr("TILDE", &idx); break;
    case OP_IS: print_data_type_instr("IS", func, &idx, size); break;
    case OP_AS: print_data_type_instr("AS", func, &idx, size); break;
    case OP_TERNARY: print_bare_instr("TERNARY", &idx); break;
    case OP_LIST: print_number_instr("LIST", func, &idx, size); break;
    case OP_MAP: print_number_instr("MAP", func, &idx, size); break;
    case OP_DESTRUCTURE: print_number_instr("DESTRUCTURE", func, &idx, size); break;
    case OP_FOR_ASSIGN_VARS: print_number_instr("FOR_ASSIGN_VARS", func, &idx, size); break;
    case OP_MAKE_ITER: print_bare_instr("MAKE_ITER", &idx); break;
    case OP_FOR_ITER_ASSIGN: print_number_instr("FOR_ITER_ASSIGN", func, &idx, size); break;
    case OP_FOR_ITER_LOAD: print_number_instr("FOR_ITER_LOAD", func, &idx, size); break;
    case OP_FINISH_STRING: print_number_instr("FINISH_STRING", func, &idx, size); break;
    case OP_GET_BUILT_IN: print_const_instr("GET_BUILT_IN", func, &idx, size); break;
    case OP_DECLARE_GLOBAL: print_const_instr("DECLARE_GLOBAL", func, &idx, size); break;
    case OP_ASSIGN_GLOBAL: print_const_instr("ASSIGN_GLOBAL", func, &idx, size); break;
    case OP_GET_GLOBAL: print_const_instr("GET_GLOBAL", func, &idx, size); break;
    case OP_ASSIGN_LOCAL: print_number_instr("ASSIGN_LOCAL", func, &idx, size); break;
    case OP_GET_LOCAL: print_number_instr("GET_LOCAL", func, &idx, size); break;
    case OP_CAPTURE: print_bare_instr("CAPTURE", &idx); break;
    case OP_CAPTURE_AT: print_number_instr("CAPTURE_AT", func, &idx, size); break;
    case OP_ASSIGN_CAPTURED: print_number_instr("ASSIGN_CAPTURED", func, &idx, size); break;
    case OP_GET_CAPTURED: print_number_instr("GET_CAPTURED", func, &idx, size); break;
    case OP_SET_PROPERTY: print_const_instr("SET_PROPERTY", func, &idx, size); break;
    case OP_GET_PROPERTY: print_const_instr("GET_PROPERTY", func, &idx, size); break;
    case OP_CALL: print_number_instr("CALL", func, &idx, size); break;
    case OP_ASSIGN_SUBSCR: print_bare_instr("ASSIGN_SUBSCR", &idx); break;
    case OP_GET_SUBSCR: print_bare_instr("GET_SUBSCR", &idx); break;
    case OP_OPTIONALS_FUNC: print_bare_instr("OPTIONALS_FUNC", &idx); break;
    case OP_CLOSURE: print_const_instr("CLOSURE", func, &idx, size); break;
    case OP_CREATE_CLASS: print_const_instr("CREATE_CLASS", func, &idx, size); break;
    case OP_ADD_METHODS: print_number_instr("ADD_METHODS", func, &idx, size); break;
    case OP_ADD_INIT: print_bare_instr("ADD_INIT", &idx); break;
    case OP_JUMP: print_number_instr("JUMP", func, &idx, size); break;
    case OP_JUMP_BACK: print_number_instr("JUMP_BACK", func, &idx, size); break;
    case OP_JUMP_IF: print_number_instr("JUMP_IF", func, &idx, size); break;
    case OP_JUMP_IF_NOT: print_number_instr("JUMP_IF_NOT", func, &idx, size); break;
    case OP_POP_JUMP_IF: print_number_instr("POP_JUMP_IF", func, &idx, size); break;
    case OP_POP_JUMP_IF_NOT: print_number_instr("POP_JUMP_IF_NOT", func, &idx, size); break;
    case OP_POP_JUMP_BACK_IF: print_number_instr("POP_JUMP_BACK_IF", func, &idx, size); break;
    case OP_POP_LOCAL: print_bare_instr("POP_LOCAL", &idx); break;
    case OP_POP_LOCALS: print_number_instr("POP_LOCALS", func, &idx, size); break;
    case OP_POP_CAPTURES: print_number_instr("POP_CAPTURES", func, &idx, size); break;
    case OP_RETURN: print_bare_instr("RETURN", &idx); break;
    case OP_CLOSURE_RETURN: print_number_instr("CLOSURE_RETURN", func, &idx, size); break;
    case OP_COPY_TOP: print_bare_instr("COPY_TOP", &idx); break;
    case OP_START_TRY: print_number_instr("START_TRY", func, &idx, size); break;
    case OP_FINISH_TRY: print_bare_instr("FINISH_TRY", &idx); break;
    case OP_RAISE: print_bare_instr("RAISE", &idx); break;
    case OP_END: print_bare_instr("END", &idx); break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    putchar('\n');
    return idx;
}

/** Prints a line seperator for bytecode output. */
static void print_separator(const FuncObj *func) {
    if (HAS_POS_INFO(func)) {
        printf("----------");
    }
    printf("-----------------------\n");
}

/** 
 * Prints all the bytecode in the passed func.
 * 
 * Only prints line if source positions array is empty.
 * It could be NULL in the case that the bytecode array is simple empty,
 * but in that the loop won't end up triggering and printing anything anyways.
 */
void print_bytecode(const FuncObj *func) {
    printf("-------------------- BYTECODE START --------------------\n");
    print_obj(AS_OBJ(func), true);
    putchar('\n');
    print_separator(func);
    printf("|  ");
    if (HAS_POS_INFO(func)) {
        printf("Line | ");
    }
    printf("Instruction | Opcode  |\n");
    print_separator(func);

    InstrSize size = INSTR_ONE_BYTE;
    // Loop doesn't increment the index, as print_instr()'s return does that instead.
    for (u32 idx = 0; idx < func->bytecode.length;) {
        idx = print_instr(func, idx, &size, HAS_POS_INFO(func) ? FORMAT_NORMAL : FORMAT_NO_LINE);
    }
    printf("-------------------- BYTECODE END --------------------\n");
}
