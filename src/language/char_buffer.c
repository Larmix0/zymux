#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "char_buffer.h"
#include "data_structures.h"

CharBuffer create_char_buffer() {
    CharBuffer buffer = {.capacity = 1, .length = 1, .text = ZMX_ARRAY_ALLOC(1, char)};
    buffer.text[0] = '\0';
    return buffer;
}

void buffer_append_string(CharBuffer *buffer, const char *string) {
    const int length = strlen(string);

    if (buffer->length + length + 1 > buffer->capacity) {
        buffer->capacity += length * 2;
        buffer->text = ZMX_REALLOC(buffer->text, buffer->capacity, sizeof(char));
    }
    strncat(buffer->text, string, length);
    buffer->length += length;
}

void buffer_append_strings(CharBuffer *buffer, const int amount, ...) {
    va_list args;
    va_start(args, amount);
    for (int i = 0; i < amount; i++) {
        buffer_append_string(buffer, va_arg(args, char *));
    }
    va_end(args);
}

void buffer_append_char(CharBuffer *buffer, const char ch) {
    APPEND_DA(buffer->text, buffer->length, buffer->capacity, '\0', char);
    buffer->text[buffer->length - 2] = ch; // Previous.
}

char buffer_pop(CharBuffer *buffer) {
    // Grab previous before turning it into the new NUL.
    char ch = buffer->text[buffer->length - 2];
    buffer->text[buffer->length - 2] = '\0';
    DROP_DA(buffer->length);
    return ch;
}

void buffer_pop_amount(CharBuffer *buffer, const int amount) {
    for (int i = 0; i < amount; i++) {
        buffer_pop(buffer);
    }
}

void free_char_buffer(CharBuffer *buffer) {
    free(buffer->text);
}
