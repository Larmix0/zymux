#ifndef CHAR_BUFFER_H
#define CHAR_BUFFER_H

#include <stdarg.h>

#include "data_structures.h"

/**
 * An array of characters that is always NUL terminated (the NUL counts towards length).
 * 
 * The buffer's length always takes into account NUL.
 */
DECLARE_DA_STRUCT(CharBuffer, char);

/** Returns an initialized CharBuffer. */
CharBuffer create_char_buffer();

/** Appends the length amount of characters in strings to buffer. */
void buffer_append_string_len(CharBuffer *buffer, const char *string, const int length);

/** Appends the passed string to the buffer. */
void buffer_append_string(CharBuffer *buffer, const char *string);

/** Appends the passed character to the buffer. */
void buffer_append_char(CharBuffer *buffer, const char ch);

/** Appends a variable number of strings to the buffer. The number is amount parameter. */
void buffer_append_strings(CharBuffer *buffer, const int amount, ...);

/** Appends a formatted string. */
void buffer_append_format(CharBuffer *buffer, const char *format, ...);

/** Pops a single character off of the buffer. */
char buffer_pop(CharBuffer *buffer);

/** Pops "amount" characters off of the buffer. */
void buffer_pop_amount(CharBuffer *buffer, const int amount);

/** Frees all the memory the buffer used. */
void free_char_buffer(CharBuffer *buffer);

#endif
