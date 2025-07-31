#include <stdio.h>

#include "debug_token.h"
#include "char_buffer.h"
#include "program.h"
#include "report_error.h"

/** Prints the passed token to the console. */
static void print_token(const Token token) {
    printf(
        "Token(lexeme='%.*s', length=%d, line=%d, column=%d, type=%s",
        token.pos.length, token.lexeme, token.pos.length,
        token.pos.line, token.pos.column, token_type_string(token.type)
    );
    switch (token.type) {
    case TOKEN_INT_LIT:
        printf(", integer=" ZMX_INT_FMT, token.intVal);
        break;
    case TOKEN_FLOAT_LIT:
        printf(", float=" ZMX_FLOAT_FMT, token.floatVal);
        break;
    case TOKEN_STRING_LIT:
        printf(", string='%s'", token.stringVal.text);
        break;
    default:
        break; // Add nothing to the token's print.
    }
    putchar(')');
}

/** Prints an entire array of tokens. */
void print_tokens(ZmxProgram *program, const TokenArray tokens) {
    MUTEX_LOCK(&program->printLock);
    printf("-------------------- TOKENS START --------------------\n");
    for (u32 i = 0; i < tokens.length; i++) {
        printf(INDENT);
        print_token(tokens.data[i]);
        putchar('\n');
    }
    printf("-------------------- TOKENS END --------------------\n");
    MUTEX_UNLOCK(&program->printLock);
}
