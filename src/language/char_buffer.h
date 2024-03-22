#ifndef CHAR_BUFFER_H
#define CHAR_BUFFER_H

typedef struct {
    int capacity;
    int length;
    char *text;
} CharBuffer;

CharBuffer create_char_buffer();

void buffer_append_string(CharBuffer *buffer, const char *string);
void buffer_append_char(CharBuffer *buffer, const char ch);
void buffer_append_strings(CharBuffer *buffer, const int amount, ...);

char buffer_pop(CharBuffer *buffer);
void buffer_pop_amount(CharBuffer *buffer, const int amount);

void free_char_buffer(CharBuffer *buffer);

#endif
