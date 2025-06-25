#ifndef GC_H
#define GC_H

#include <stdbool.h>

#include "constants.h"
#include "dynamic_array.h"
#include "object.h"

/** Multiplier for how many more bytes need to be allocated before another collection. */
#define COLLECTION_GROWTH 2

/** Puts the specifically passed object in the protection pool. */
#define GC_PROTECT(gc, obj) APPEND_DA(&(gc)->protected, obj)

/** Drops the topmost objects in the explicit protection pool. */
#define GC_DROP_PROTECTED(gc) DROP_DA(&(gc)->protected)

/** Drops multiple objects in the protection pool. */
#define GC_DROP_PROTECTED_AMOUNT(gc, amount) DROP_AMOUNT_DA(&(gc)->protected, amount)

/** Increments the number which freezes GCing newly created objects from the GC if > 0. */
#define GC_FREEZE(gc) ((gc)->frozenLayers++)

/** decrement the counter protecting new objects and clear the frozen pool if safe to do so. */
#define GC_END_FREEZE(gc) \
    do { \
        (gc)->frozenLayers--; \
        if ((gc)->frozenLayers <= 0) { \
            FREE_DA(&(gc)->frozen); \
            INIT_DA(&(gc)->frozen); \
        } \
    } while (false)

typedef struct Compiler Compiler;
typedef struct Vm Vm;
typedef struct ZmxProgram ZmxProgram;

/** 
 * Holds all of the structs/objects relevant to marking in the garbage collector.
 * 
 * If anything in the garbage collector is set to NULL, then it'll assume that the things it holds
 * aren't currently allocated, and therefore don't need to be kept track of (the GC will ignore it).
 */
typedef struct Gc {
    Compiler *compiler; /** The current compiler to mark its objects as reachable. */
    Vm *vm; /** The current VM to mark its objects as reachable. */

    /** How many "layers" of frozen objs are on the GC. Protects all new objects if this is > 1. */
    int frozenLayers;

    /** Objects temporarily frozen from collection regardless of reachability. */
    ObjArray frozen;

    /** Special objects explicitly and manually protected, and they have to be manually popped. */
    ObjArray protected;

    u64 allocated; /** Amount of bytes allocated so far. */
    u64 nextCollection; /** Amount of bytes that need to be allocated before collecting again. */
} Gc;

/** Creates an empty garbage collector. */
Gc create_gc();

/** Frees the memory the garbage collector has allocated. */
void free_gc(Gc *gc);

/** Goes through the garbage collection phase by seeing reachable objects in the program's GC. */
void gc_collect(ZmxProgram *program);

#endif
