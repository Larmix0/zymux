#ifndef GC_H
#define GC_H

#include <stdbool.h>

#include "constants.h"
#include "dynamic_array.h"
#include "object.h"

/** How many more bytes allocated before collection multiplier after each collection. */
#define COLLECTION_GROWTH 2

/** Puts the specifically passed object in the protection pool. */
#define GC_PROTECT_OBJ(gc, obj) APPEND_DA(&(gc)->protected, obj)

/** Increments the number which allows protecting newly created objects from the GC if > 0. */
#define GC_PUSH_PROTECTION(gc) ((gc)->protectionLayers++)

/** Decrements the number which allows protecting newly created objects from the GC if > 0. */
#define GC_POP_PROTECTION(gc) ((gc)->protectionLayers--)

/** Clears the pool of protected objects if there aren't any current protection layers. */
#define GC_CLEAR_PROTECTED(gc) \
    do { \
        if ((gc)->protectionLayers <= 1) { \
            FREE_DA(&(gc)->protected); \
            INIT_DA(&(gc)->protected); \
        } \
    } while (false)

/** Tries to clear the protected pool, and decrements the counter protecting new objects. */
#define GC_POP_AND_CLEAR_PROTECTED(gc) \
    do { \
        if ((gc)->protectionLayers == 1) { \
            GC_POP_PROTECTION(gc); \
        } \
        GC_CLEAR_PROTECTED(gc); \
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
    ObjArray protected; /** Objects temporarily protected from collection before being reachable. */

    /** How many "layers" of protection are on the GC. Protects all new objects if this is > 1. */
    int protectionLayers;

    u64 allocated; /** Amount of bytes allocated so far. */
    u64 nextCollection; /** Amount of bytes that need to be allocated before collecting again. */
} Gc;

/** Creates a garbage collector. Pass NULL to any argument that hasn't been created. */
Gc create_gc(Compiler *compiler, Vm *vm);

/** Creates a garbage collector whose values are immediately set to the default NULL/0. */
Gc create_empty_gc();

/** Frees the memory the garbage collector has allocated. */
void free_gc(Gc *gc);

/** Goes through the garbage collection phase by seeing reachable objects in the program's GC. */
void gc_collect(ZmxProgram *program);

#endif
