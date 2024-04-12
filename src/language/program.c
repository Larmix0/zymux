#include <stdlib.h>
#include <string.h>

#include "parser.h"
#include "program.h"
#include "report_error.h"

/** Returns an initialized Zymux program with the file and whether or not to show errors. */
ZmxProgram create_zmx_program(char *file, bool showErrors) {
    ZmxProgram program = {
        .hasErrored = false, .showErrors = showErrors, .currentFile = file, .allNodes = NULL
    };
    return program;
}

/** Frees all nodes that were allocated and placed inside the passed program. */
void free_all_nodes(ZmxProgram *program) {
    if (program->allNodes == NULL) {
        return;
    }

    AstNode *current = program->allNodes;
    AstNode *next = current->next;
    while (current != NULL) {
        free(current);
        current = next;
        if (next != NULL) {
            next = next->next;
        }
    }
}

/** Frees all the memory the passed program owns. */
void free_zmx_program(ZmxProgram *program) {
    // Doesn't free nodes, as this should be manually done after compiling and before executing VM.
}

/** 
 * Allocates a block of memory using size.
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
 * Reallocates oldPointer and returns a new block from newSize.
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
