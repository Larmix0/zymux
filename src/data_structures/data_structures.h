#ifndef DATA_STRUCTURES_H
#define DATA_STRUCTURES_H

#include "stdlib.h"

#include "program.h"

/** Declares the struct of a dynamic array typedef'd with name and an array/pointer of type. */
#define DECLARE_DA_STRUCT(name, type) typedef struct {int length; int capacity; type *data;} name

/** Creates a dynamic array. */
#define CREATE_DA() {.length = 0, .capacity = 0, .data = NULL}

/** Initializes/resets an array that's already created. */
#define INIT_DA(array) \
    do { \
        (array)->length = 0; \
        (array)->capacity = 0; \
        (array)->data = NULL; \
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
 * therefore dereferencing NULL. This is not an issue though as our dynamic array isn't a VLA.
 */
#define APPEND_DA(array, item) \
    do { \
        if ((array)->capacity < (array)->length + 1) { \
            (array)->capacity = (array)->capacity < 16 ? 16 : (array)->capacity * 2; \
            (array)->data = ZMX_REALLOC( \
                (array)->data, (array)->capacity * sizeof(*(array)->data) \
            ); \
        } \
        (array)->data[(array)->length++] = (item); \
    } while (false)

/** Pops and returns the last element from the passed dynamic array. */
#define POP_DA(array) ((array)->data[((array)->length--) - 1])

/** Simply drops the last element of the passed dynamic array without returning it. */
#define DROP_DA(array) ((array)->length--)

/**
 * Frees the memory of the data in the dynamic array.
 * Does not free the elements themselves (like an array of heap allocated structs).
 */
#define FREE_DA(array) (free((array)->data))

#endif
