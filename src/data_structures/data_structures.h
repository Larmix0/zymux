#ifndef DATA_STRUCTURES_H
#define DATA_STRUCTURES_H

#include "zymux_program.h"

#define APPEND_DA(elements, length, capacity, item, type) \
    do { \
        if (capacity < length + 1) { \
            capacity = capacity < 16 ? 16 : capacity * 2; \
            elements = ZMX_REALLOC(elements, capacity, type); \
        } \
        elements[length++] = item; \
    } while (false)

#define POP_DA(elements, length) (elements[length--])
#define DROP_DA(length) (length--)

#endif
