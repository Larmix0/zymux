#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "char_buffer.h"

void free_char_buffer(CharBuffer *buffer);

/** Creates and initializes a char buffer with a NUL terminator already appended. */
CharBuffer create_char_buffer() {
    CharBuffer buffer = {.capacity = 0, .length = 0, .text = ARRAY_ALLOC(1, char)};
    buffer.text[0] = '\0';
    return buffer;
}

/** Appends a string of length with strncat. */
void buffer_append_string_len(CharBuffer *buffer, const char *string, const size_t length) {
    if (*string == '\0') {
        return; // Don't append more NULs.
    }
    if (buffer->length + length + 1 > buffer->capacity) {
        buffer->capacity += length * 2 + 1;
        buffer->text = ARRAY_REALLOC(buffer->text, buffer->capacity, sizeof(char));
    }
    strncat(buffer->text, string, length);
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
    if (*format == '\0') {
        return; // Don't append more NULs.
    }

    va_list argsForLength, args;
    va_start(args, format);
    va_copy(argsForLength, args);

    const int formatLength = vsnprintf(NULL, 0, format, argsForLength);
    if (buffer->length + formatLength + 1 > buffer->capacity) {
        buffer->capacity += formatLength * 2 + 1;
        buffer->text = ARRAY_REALLOC(buffer->text, buffer->capacity, sizeof(char));
    }
    // +1 to length of format is for writing NUL.
    vsnprintf(buffer->text + buffer->length, formatLength + 1, format, args);
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
        return; // Don't append more NULs.
    }

    buffer->text[buffer->length++] = ch; // Replace the NUL with the character.
    if (buffer->length + 1 > buffer->capacity) { // +1 for NUL.
        INCREASE_CAPACITY(buffer->capacity);
        buffer->text = REALLOC(buffer->text, buffer->capacity * sizeof(char));
    }
    // Now append the automatic NUL since the character took its place.
    buffer->text[buffer->length] = '\0';
}

/** 
 * Pops the last character in buffer.
 * 
 * Because the buffer is NUL terminated, we actually pop the last character (NUL),
 * and then replace the new last character (the character to be actually popped) with NUL.
 */
char buffer_pop(CharBuffer *buffer) {
    ASSERT(buffer->length > 0, "Buffer is already considered empty.");
    char popped = buffer->text[buffer->length - 1];
    buffer->text[buffer->length - 1] = '\0';
    buffer->length--;
    return popped;
}

/** Pops as many characters as the amount parameter specifies. */
void buffer_pop_amount(CharBuffer *buffer, const int amount) {
    for (int i = 0; i < amount; i++) {
        buffer_pop(buffer);
    }
}

/** "Combines" 2 passed buffers, by appending the right one to the left and freeing the right. */
void buffer_combine(CharBuffer *left, CharBuffer *right) {
    buffer_append_string(left, right->text);
    free_char_buffer(right);
}

/** Frees all memory the CharBuffer has used. */
void free_char_buffer(CharBuffer *buffer) {
    free(buffer->text);
}
