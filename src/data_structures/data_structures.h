#ifndef DATA_STRUCTURES_H
#define DATA_STRUCTURES_H

#include "zymux_program.h"

/** Generic append macro for dynamic arrays where all relevant members of it get passed. */
#define APPEND_DA(elements, length, capacity, item, type) \
    do { \
        if (capacity < length + 1) { \
            capacity = capacity < 16 ? 16 : capacity * 2; \
            elements = ZMX_REALLOC(elements, capacity, type); \
        } \
        elements[length++] = item; \
    } while (false)

/** Generic pop macro for dynamic arrays where the pointer of elements and length are passed. */
#define POP_DA(elements, length) (elements[length--])

/** Generic drop macro for dynamic arrays where the length is passed to be decremented. */
#define DROP_DA(length) (length--)

#endif
