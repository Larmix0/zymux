#include <stdio.h>

#include "debug_token.h"
#include "char_buffer.h"
#include "report_error.h"

/** Prints the passed token to the console. */
static void append_token_representation(CharBuffer *tokenString, const Token token) {
    buffer_append_format(
        tokenString, "Token(lexeme=\"%.*s\", length=%d, line=%d, column=%d, type=%s",
        token.pos.length, token.lexeme, token.pos.length,
        token.pos.line, token.pos.column, type_to_string(token.type)
    );
    switch (token.type) {
        case TOKEN_INT_LIT:
            buffer_append_format(tokenString, ", integer=" ZMX_INT_FMT, token.intVal);
            break;
        case TOKEN_FLOAT_LIT:
            buffer_append_format(tokenString, ", float=" ZMX_FLOAT_FMT, token.floatVal);
            break;
        case TOKEN_STRING_LIT:
            buffer_append_format(tokenString, ", string=\"%s\"", token.stringVal.text);
            break;
        case TOKEN_ERROR:
            buffer_append_format(tokenString, ", error=\"%s\"", token.errorMessage);
            break;
        default: break;
    }
    buffer_append_char(tokenString, ')');
}

/** Prints an entire array of tokens. */
void print_tokens(const TokenArray tokens) {
    printf("Tokens:\n");
    CharBuffer tokenString = create_char_buffer();
    for (int i = 0; i < tokens.length; i++) {
        buffer_append_char(&tokenString, '\t');
        append_token_representation(&tokenString, tokens.data[i]);
        buffer_append_char(&tokenString, '\n');
    }
    printf("%s", tokenString.data);
    free_char_buffer(&tokenString);
}
