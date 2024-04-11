#ifndef CHAR_BUFFER_H
#define CHAR_BUFFER_H

#include "data_structures.h"

/**
 * An array of characters that is always NUL terminated (the NUL counts towards length).
 * 
 * The buffer's length always takes into account NUL.
 */
DECLARE_DA_STRUCT(CharBuffer, char);

/** Returns an initialized CharBuffer. */
CharBuffer create_char_buffer();

/** Appends the passed string to the buffer. */
void buffer_append_string(CharBuffer *buffer, const char *string);

/** Appends the passed character to the buffer. */
void buffer_append_char(CharBuffer *buffer, const char ch);

/** Appends a variable number of strings to the buffer. The number is amount parameter. */
void buffer_append_strings(CharBuffer *buffer, const int amount, ...);

/** Pops a single character off of the buffer. */
char buffer_pop(CharBuffer *buffer);

/** Pops "amount" characters off of the buffer. */
void buffer_pop_amount(CharBuffer *buffer, const int amount);

/** Frees all the memory the buffer used. */
void free_char_buffer(CharBuffer *buffer);

#endif
