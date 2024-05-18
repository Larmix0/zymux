#include <stdlib.h>
#include <string.h>

#include "node.h"
#include "object.h"
#include "program.h"

/** Returns an initialized Zymux program with the file and whether or not to show errors. */
ZmxProgram create_zmx_program(char *file, const bool showErrors) {
    ZmxProgram program = {
        .hasErrored = false, .showErrors = showErrors, .currentFile = file,
        .allNodes = NULL, .allObjs = NULL
    };
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

/** 
 * Returns the base to the power of exponent.
 * 
 * Could've linked with the C math library, but I decided to reimplement it since it's just 5 lines.
 * Simply implements the power by multiplying the base once for every exponent.
 */
ZmxInt zmx_int_power(const ZmxInt base, const ZmxInt exponent) {
    ZmxInt result = 1;
    for (ZmxInt i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}

/** Returns the base to the power of exponent (float). */
ZmxFloat zmx_float_power(const ZmxFloat base, const ZmxFloat exponent) {
    // TODO: Remove this function as a whole, since actual float exponent is much more complicated..
    ZmxFloat result = 1;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}

/** 
 * Returns the float result of dividend modulo divisor (both floats).
 * 
 * This function was created because the modulo operator in C by default only supports integers.
 */
ZmxFloat zmx_float_modulo(ZmxFloat dividend, const ZmxFloat divisor) {
    while (dividend >= divisor) {
        dividend -= divisor;
    }
    return dividend;
}
