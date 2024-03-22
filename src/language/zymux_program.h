#ifndef ZYMUX_PROGRAM_H
#define ZYMUX_PROGRAM_H

#include <stdbool.h>

#include "constants.h"

#define ZMX_ALLOC(type) (zmx_alloc(sizeof(type), __FILE__, __func__, __LINE__))
#define ZMX_ARRAY_ALLOC(length, type) \
    (zmx_alloc((length) * sizeof(type), __FILE__, __func__, __LINE__))
#define ZMX_REALLOC(oldPointer, amount, type) \
    (zmx_realloc((oldPointer), (amount) * sizeof(type), __FILE__, __func__, __LINE__))

typedef struct {
    bool hasErrored;
    bool showErrors;
    char *currentFile;
} ZymuxProgram;

void *zmx_alloc(const size_t size, const char *file, const char *func, const int line);
void *zmx_realloc(
    void *oldPointer, const size_t newSize, const char *file, const char *func, const int line
);
i64 zmx_power(const int base, const int exponent);

ZymuxProgram create_zymux_program(char *file, bool showErrors);
void free_zymux_program(ZymuxProgram *program);

#endif
