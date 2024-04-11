#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "char_buffer.h"
#include "data_structures.h"

/** Creates and initializes a char buffer with a NUL terminator already appended. */
CharBuffer create_char_buffer() {
    CharBuffer buffer = {.capacity = 1, .length = 1, .data = ZMX_ARRAY_ALLOC(1, char)};
    buffer.data[0] = '\0';
    return buffer;
}

/** Appends the passed string to the buffer. Done with strncat to ensure it's NUL terminated. */
void buffer_append_string(CharBuffer *buffer, const char *string) {
    const int length = strlen(string);

    if (buffer->length + length + 1 > buffer->capacity) {
        buffer->capacity += length * 2;
        buffer->data = ZMX_REALLOC_ARRAY(buffer->data, buffer->capacity, sizeof(char));
    }
    strncat(buffer->data, string, length);
    buffer->length += length;
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

/** 
 * Appends the passed character to the buffer.
 * 
 * It does so by merely replacing the old NUL with the character and then appending a new NUL.
 */
void buffer_append_char(CharBuffer *buffer, const char ch) {
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
