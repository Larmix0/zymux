#ifndef DYNAMIC_ARRAY_H
#define DYNAMIC_ARRAY_H

#include <stdlib.h>

#include "allocator.h"
#include "constants.h"

/** Declares the struct of a dynamic array typedef'd with name and an array/pointer of type. */
#define DECLARE_DA_STRUCT(name, type) typedef struct {u32 length; u32 capacity; type *data;} name

/** Creates a dynamic array. */
#define CREATE_DA() {.length = 0, .capacity = 0, .data = NULL}

/** Initializes/resets an array that's already created. */
#define INIT_DA(da) \
    do { \
        (da)->length = 0; \
        (da)->capacity = 0; \
        (da)->data = NULL; \
    } while (false)

/**
 * Appends item to the passed dynamic array.
 * 
 * Also, we can see when allocating that we dereference the array's data which may be NULL
 * if the size of the array is 0, which it is in the first append.
 * This is actually fine as sizeof() is a compile time construct for figuring out the size,
 * so we aren't actually dereferencing NULL.
 * 
 * However, I believe if it's a variable length array (VLA) then it'll have to evaluate the element,
 * therefore dereferencing NULL. This is not an issue though, as our dynamic array isn't a VLA.
 */
#define APPEND_DA(da, item) \
    do { \
        if ((da)->capacity < (da)->length + 1) { \
            INCREASE_CAPACITY((da)->capacity); \
            (da)->data = REALLOC((da)->data, (da)->capacity * sizeof(*(da)->data)); \
        } \
        (da)->data[(da)->length] = (item); \
        (da)->length++; \
    } while (false)

/** Pops and returns the last element from the passed dynamic array. */
#define POP_DA(da) ((da)->data[--(da)->length])

/** Simply drops the last element of the passed dynamic array without returning it. */
#define DROP_DA(da) ((da)->length--)

/** Removes "amount" of elements from the top of the array by subtracting its length.  */
#define DROP_AMOUNT_DA(da, amount) ((da)->length -= amount)

/**
 * Frees the memory of the data in the dynamic array.
 * Does not free the elements themselves (like an array of heap allocated structs).
 */
#define FREE_DA(da) (free((da)->data))

/** An array of just C integers. */
DECLARE_DA_STRUCT(IntArray, int);

/** An array of unsigned 32 bit integers. */
DECLARE_DA_STRUCT(U32Array, u32);

#endif
