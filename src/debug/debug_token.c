#include <stdio.h>

#include "debug_token.h"
#include "char_buffer.h"
#include "report_error.h"

/** Prints the passed token to the console. */
static void print_token(const Token token) {
    printf(
        "Token(lexeme=\"%.*s\", length=%d, line=%d, column=%d, type=%s",
        token.pos.length, token.lexeme, token.pos.length,
        token.pos.line, token.pos.column, token_type_as_string(token.type)
    );
    switch (token.type) {
        case TOKEN_INT_LIT:
            printf(", integer=" ZMX_INT_FMT, token.intVal);
            break;
        case TOKEN_FLOAT_LIT:
            printf(", float=" ZMX_FLOAT_FMT, token.floatVal);
            break;
        case TOKEN_STRING_LIT:
            printf(", string=\"%s\"", token.stringVal.text);
            break;
        case TOKEN_ERROR:
            printf(", error=\"%s\"", token.errorMessage);
            break;
        default: break;
    }
    putchar(')');
}

/** Prints an entire array of tokens. */
void print_tokens(const TokenArray tokens) {
    printf("Tokens:\n");
    for (int i = 0; i < tokens.length; i++) {
        putchar('\t');
        print_token(tokens.data[i]);
        putchar('\n');
    }
}
