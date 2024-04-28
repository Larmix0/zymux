#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "char_buffer.h"

/** Creates and initializes a char buffer with a NUL terminator already appended. */
CharBuffer create_char_buffer() {
    CharBuffer buffer = {.capacity = 1, .length = 1, .data = ZMX_ARRAY_ALLOC(1, char)};
    buffer.data[0] = '\0';
    return buffer;
}

/** Appends a string of length with strncat. */
void buffer_append_string_len(CharBuffer *buffer, const char *string, const int length) {
    if (buffer->length + length + 1 > buffer->capacity) {
        buffer->capacity += length * 2;
        buffer->data = ZMX_REALLOC_ARRAY(buffer->data, buffer->capacity, sizeof(char));
    }
    strncat(buffer->data, string, length);
    buffer->length += length;
}

/** Appends the passed string to the buffer. Wrapper around the length version. */
void buffer_append_string(CharBuffer *buffer, const char *string) {
    buffer_append_string_len(buffer, string, strlen(string));
}

/** 
 * Appends multiple strings with NUL termination at the end.
 * The amount parameter specifies how many strings are passed and to be appended.
 */
void buffer_append_strings(CharBuffer *buffer, const int amount, ...) {
    va_list args;
    va_start(args, amount);
    for (int i = 0; i < amount; i++) {
        buffer_append_string(buffer, va_arg(args, char *));
    }
    va_end(args);
}

/** Appends a formatted string into the buffer by using vsnprintf (twice to tell the length). */
void buffer_append_format(CharBuffer *buffer, const char *format, ...) {
    va_list argsForLength, args;
    va_start(args, format);
    va_copy(argsForLength, args);

    const int formatLength = vsnprintf(NULL, 0, format, argsForLength);
    if (buffer->length + formatLength + 1 > buffer->capacity) {
        buffer->capacity += formatLength * 2;
        buffer->data = ZMX_REALLOC_ARRAY(buffer->data, buffer->capacity, sizeof(char));
    }
    // -1 because char buffer's take NUL into account. +1 to length of format is for writing NUL.
    vsnprintf(buffer->data + buffer->length - 1, formatLength + 1, format, args);
    buffer->length += formatLength;

    va_end(args);
    va_end(argsForLength);
}

/** 
 * Appends the passed character to the buffer.
 * 
 * It does so by merely replacing the old NUL with the character and then appending a new NUL.
 */
void buffer_append_char(CharBuffer *buffer, const char ch) {
    if (ch == '\0') {
        return; // Don't append more NULs as char buffer automatically does that.
    }
    buffer->data[buffer->length - 1] = ch;
    APPEND_DA(buffer, '\0');
}

/** 
 * Pops the last character in buffer.
 * 
 * Because the buffer is NUL terminated, we actually pop the last character (NUL),
 * and then replace the new last character (the character to be actually popped) with NUL.
 */
char buffer_pop(CharBuffer *buffer) {
    char popped = buffer->data[buffer->length - 2];
    buffer->data[buffer->length - 2] = '\0';
    DROP_DA(buffer);
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
    free(buffer->data);
}
