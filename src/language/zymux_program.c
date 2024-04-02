#include <stdlib.h>
#include <string.h>

#include "errors.h"
#include "zymux_program.h"

/** Returns an initialized ZymuxProgram with the file and whether or not to show errors. */
ZymuxProgram create_zymux_program(char *file, bool showErrors) {
    ZymuxProgram program = {.hasErrored = false, .currentFile = file, .showErrors = showErrors};
    return program;
}

/** Frees all the memory the passed ZymuxProgram owns. */
void free_zymux_program(ZymuxProgram *program) {
    // TODO: implement freeing when the program holds heap-allocated things.
}

/** 
 * Allocates a block of memory size.
 * 
 * The file, func, and line are manually passed from the caller so that in the case of a 
 * memory error, we point the user to the line of Zymux that called the allocator instead.
 */
void *zmx_alloc(const size_t size, const char *file, const char *func, const int line) {
    void *pointer = malloc(size);
    if (pointer == NULL) {
        MEMORY_ERROR_ARGS(file, func, line, "Zymux failed to allocate memory.");
    }
    return pointer;
}

/** 
 * Reallocated oldPointer and returns a new block from newSize.
 * 
 * The file, func, and line are manually passed from the caller so that in the case of a 
 * memory error, we point the user to the line of Zymux that called the allocator instead.
 */
void *zmx_realloc(
    void *oldPointer, const size_t newSize, const char *file, const char *func, const int line
) {
    void *newPointer = realloc(oldPointer, newSize);
    if (newPointer == NULL) {
        MEMORY_ERROR_ARGS(file, func, line, "Zymux failed to reallocate memory.");
    }
    return newPointer;
}

/** 
 * Returns the base to the power of exponent.
 * 
 * Could've linked with the C math library, but I decided to reimplement it since it's just 5 lines.
 * Simply implements the power by multiplying the base once for every exponent.
 */
i64 zmx_power(const int base, const int exponent) {
    i64 result = 1;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}
