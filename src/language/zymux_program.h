#ifndef ZYMUX_PROGRAM_H
#define ZYMUX_PROGRAM_H

#include <stdbool.h>

#include "constants.h"

/** To be used whenever Zymux allocates a piece of memory. */
#define ZMX_ALLOC(type) (zmx_alloc(sizeof(type), __FILE__, __func__, __LINE__))

/** To be used whenever Zymux allocates an array of a certain type. */
#define ZMX_ARRAY_ALLOC(length, type) \
    (zmx_alloc((length) * sizeof(type), __FILE__, __func__, __LINE__))

/** To be used whenever Zymux needs to reallocate any block of memory. */
#define ZMX_REALLOC(oldPointer, amount, type) \
    (zmx_realloc((oldPointer), (amount) * sizeof(type), __FILE__, __func__, __LINE__))

/** Stores the state of Zymux throughout the entire execution of the program. */
typedef struct {
    bool hasErrored;
    bool showErrors;
    char *currentFile;
} ZymuxProgram;

/** 
 * Allocates size memory and uses file, func, and line for errors.
 * This is here for the macros, so use those macros instead.
 */
void *zmx_alloc(const size_t size, const char *file, const char *func, const int line);

/** 
 * Reallocates oldPointer with size and uses file, func, and line for errors.
 * This is here for the macros, so use those macros instead.
 */
void *zmx_realloc(
    void *oldPointer, const size_t newSize, const char *file, const char *func, const int line
);

/** Returns the number result of base to the power of exponent. */
i64 zmx_power(const int base, const int exponent);

/** Returns an initialized zymux program with the parameters. */
ZymuxProgram create_zymux_program(char *file, bool showErrors);

/** Frees all the memory a ZymuxProgram has used. */
void free_zymux_program(ZymuxProgram *program);

#endif
