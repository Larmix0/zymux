#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include "report_error.h"

/** To be used for generally allocating a number of bytes. */
#define ALLOC(amount) (zmx_alloc(amount, SOURCE_INFO))

/** To be used when allocating memory for a specific type. */
#define TYPE_ALLOC(type) (ALLOC(sizeof(type)))

/** To be used when allocating an array of a certain type. */
#define ARRAY_ALLOC(length, type) (ALLOC((length) * sizeof(type)))

/** To be used when reallocating a block of memory of a specific length in bytes.  */
#define REALLOC(oldPointer, amount) \
    (zmx_realloc(oldPointer, amount, SOURCE_INFO))

/** To be used when reallocating an array. */
#define ARRAY_REALLOC(oldPointer, amount, type) \
    (REALLOC(oldPointer, (amount) * sizeof(type)))

/** 
 * Allocates size memory and uses file, func, and line for errors.
 * This is here for the macros, so use those macros instead.
 */
void *zmx_alloc(const size_t size, const SourceInfo info);

/** 
 * Reallocates oldPointer with size and uses file, func, and line for errors.
 * This is here for the macros, so use those macros instead.
 */
void *zmx_realloc(void *oldPointer, const size_t newSize, const SourceInfo info);

#endif
