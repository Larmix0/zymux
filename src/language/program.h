#ifndef PROGRAM_H
#define PROGRAM_H

#include <stdbool.h>

#include "constants.h"
#include "report_error.h"

/** To be used for generally allocating a number of bytes. */
#define ZMX_ALLOC(amount) (zmx_alloc((amount), SOURCE_INFO))

/** To be used when allocating memory for a specific type. */
#define ZMX_TYPE_ALLOC(type) (ZMX_ALLOC(sizeof(type)))

/** To be used when allocating an array of a certain type. */
#define ZMX_ARRAY_ALLOC(length, type) (ZMX_ALLOC((length) * sizeof(type)))

/** To be used when reallocating a block of memory of a specific length in bytes.  */
#define ZMX_REALLOC(oldPointer, amount) \
    (zmx_realloc((oldPointer), (amount), SOURCE_INFO))

/** To be used when reallocating an array. */
#define ZMX_REALLOC_ARRAY(oldPointer, amount, type) \
    (ZMX_REALLOC((oldPointer), (amount) * sizeof(type)))

typedef struct AstNode AstNode;
typedef struct Obj Obj;

/** Stores the state of Zymux throughout the entire execution of the program. */
typedef struct ZmxProgram {
    bool hasErrored;
    bool showErrors;
    char *currentFile;
    AstNode *allNodes;
    Obj *allObjs;
} ZmxProgram;

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

/** Returns the number result of base to the power of exponent. */
i64 zmx_power(const int base, const int exponent);

/** Returns an initialized zymux program with the parameters. */
ZmxProgram create_zmx_program(char *file, bool showErrors);

/** Frees generally everything held by program. */
void free_zmx_program(ZmxProgram *program);

#endif
