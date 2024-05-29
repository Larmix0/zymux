#include <stdlib.h>
#include <string.h>

#include "node.h"
#include "object.h"
#include "program.h"

/** Returns an initialized Zymux program with the file and whether or not to show errors. */
ZmxProgram create_zmx_program(char *file, const bool showErrors) {
    ZmxProgram program = {
        .hasErrored = false, .showErrors = showErrors,
        .allNodes = NULL, .allObjs = NULL, .mainFile = NULL, .currentFile = NULL
    };
    program.mainFile = new_string_obj(&program, file);
    program.currentFile = new_string_obj(&program, file);
    return program;
}

/** 
 * Frees all the memory the passed program owns which is freed after the VM's execution.
 * 
 * This means it doesn't free nodes or tokens, as they should be manually freed
 * after compiling and before executing the VM for runtime memory efficiency.
 */
void free_zmx_program(ZmxProgram *program) {
    free_all_objs(program);
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
