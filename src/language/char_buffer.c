#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "char_buffer.h"
#include "data_structures.h"

/**
 * Creates and initializes a char buffer with a NUL character prepended.
 * This is to keep the char buffer terminated at all times, even before usage.
 */
CharBuffer create_char_buffer() {
    CharBuffer buffer = {.capacity = 1, .length = 1, .text = ZMX_ARRAY_ALLOC(1, char)};
    buffer.text[0] = '\0';
    return buffer;
}

/** Appends to passed string to the buffer with strncat to ensure it remains NUL terminated. */
void buffer_append_string(CharBuffer *buffer, const char *string) {
    const int length = strlen(string);

    if (buffer->length + length + 1 > buffer->capacity) {
        buffer->capacity += length * 2;
        buffer->text = ZMX_REALLOC(buffer->text, buffer->capacity, sizeof(char));
    }
    strncat(buffer->text, string, length);
    buffer->length += length;
}

/**
 * Appends multiple strings with NUL termination at the end.
 * The amount parameter specifies how many strings to be appended.
 */
void buffer_append_strings(CharBuffer *buffer, const int amount, ...) {
    va_list args;
    va_start(args, amount);
    for (int i = 0; i < amount; i++) {
        buffer_append_string(buffer, va_arg(args, char *));
    }
    va_end(args);
}

/** 
 * Appends the passed character to the buffer.
 * It does so by merely replacing the old NUL with the character and then appending a new NUL.
 */
void buffer_append_char(CharBuffer *buffer, const char ch) {
    buffer->text[buffer->length - 1] = ch;
    APPEND_DA(buffer->text, buffer->length, buffer->capacity, '\0', char);
}

/**
 * Pops the last character of the buffer.
 * Because the buffer is NUL terminated, we actually pop the last character (NUL),
 * and then replace the new last character (the character to be actually popped) with NUL.
 */
char buffer_pop(CharBuffer *buffer) {
    char popped = buffer->text[buffer->length - 2];
    buffer->text[buffer->length - 2] = '\0';
    DROP_DA(buffer->length);
    return popped;
}

/** Pops as many characters as the amount parameter specifies. */
void buffer_pop_amount(CharBuffer *buffer, const int amount) {
    for (int i = 0; i < amount; i++) {
        buffer_pop(buffer);
    }
}

/** Frees all memory the CharBuffer has used. */
void free_char_buffer(CharBuffer *buffer) {
    free(buffer->text);
}
