#include <stdlib.h>
#include <string.h>

#include "errors.h"
#include "zymux_program.h"

ZymuxProgram create_zymux_program(char *file, bool showErrors) {
    ZymuxProgram program = {.hasErrored = false, .currentFile = file, .showErrors = showErrors};
    return program;
}

void free_zymux_program(ZymuxProgram *program) {
    // TODO: implement freeing when the program holds heap-allocated things.
}

void *zmx_alloc(const size_t size, const char *file, const char *func, const int line) {
    void *pointer = malloc(size);
    if (pointer == NULL) {
        MEMORY_ERROR_ARGS(file, func, line, "Zymux failed to allocate memory.");
    }
    return pointer;
}

void *zmx_realloc(
    void *oldPointer, const size_t newSize, const char *file, const char *func, const int line
) {
    void *newPointer = realloc(oldPointer, newSize);
    if (newPointer == NULL) {
        MEMORY_ERROR_ARGS(file, func, line, "Zymux failed to reallocate memory.");
    }
    return newPointer;
}

i64 zmx_power(const int base, const int exponent) {
    i64 result = 1;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}
