#include <stdlib.h>
#include <string.h>

#include "node.h"
#include "program.h"

/** Returns an initialized Zymux program with the file and whether or not to show errors. */
ZmxProgram create_zmx_program(char *file, bool showErrors) {
    ZmxProgram program = {
        .hasErrored = false, .showErrors = showErrors, .currentFile = file, .allNodes = NULL
    };
    return program;
}

/** Frees all the memory the passed program owns. */
void free_zmx_program(ZmxProgram *program) {
    // Doesn't free nodes or tokens,
    // as this should be manually done after compiling and before executing the VM.
}

/** 
 * Allocates a block of memory using size.
 * 
 * The file, func, and line are manually passed from the caller so that in the case of a 
 * memory error, we point the user to the line of Zymux that called the allocator instead.
 */
void *zmx_alloc(const size_t size, const SourceInfo info) {
    void *pointer = malloc(size);
    if (pointer == NULL) {
        MEMORY_ERROR_ARGS(info, "Zymux failed to allocate memory.");
    }
    return pointer;
}

/** 
 * Reallocates oldPointer and returns a new block from newSize.
 * 
 * The file, func, and line are manually passed from the caller so that in the case of a 
 * memory error, we point the user to the line of Zymux that called the allocator instead.
 */
void *zmx_realloc(void *oldPointer, const size_t newSize, const SourceInfo info) {
    void *newPointer = realloc(oldPointer, newSize);
    if (newPointer == NULL) {
        MEMORY_ERROR_ARGS(info, "Zymux failed to reallocate memory.");
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
