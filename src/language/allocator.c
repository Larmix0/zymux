#include <stdlib.h>

#include "allocator.h"

/** 
 * Allocates a block of memory using size.
 * 
 * The file, func, and line are manually passed from the caller so that in the case of a 
 * memory error, we point the user to the line of Zymux that called the allocator instead.
 */
void *zmx_alloc(const size_t size, const SourceInfo info) {
    void *pointer = malloc(size);
    if (pointer == NULL) {
        MEMORY_ERROR_INFO(info, "Zymux failed to zmx_alloc memory.");
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
        MEMORY_ERROR_INFO(info, "Zymux failed to reallocate memory.");
    }
    return newPointer;
}
