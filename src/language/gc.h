#ifndef GC_H
#define GC_H

#include <stdbool.h>

#include "constants.h"
#include "object.h"

/** How many more bytes allocated before collection multiplier after each collection. */
#define COLLECTION_GROWTH 2

/** Starts protecting newly created objects from the GC. */
#define GC_SET_PROTECTION(gc) ((gc)->protectNewObjs = true)

/** Stops protecting newely created objects, but doesn't clear the protection pool itself. */
#define GC_STOP_PROTECTION(gc) ((gc)->protectNewObjs = false)

/** 
 *
 * Clears the pool of protected objects and stops protecting new objects.
 * 
 * This is done when the protected objects have been placed at the place that would stop them
 * from being collected, and can therefore now be collected if they leave that place and become
 * unreachable.
 */
#define GC_RESET_PROTECTION(gc) \
    do { \
        FREE_DA(&(gc)->protected); \
        INIT_DA(&(gc)->protected); \
        (gc)->protectNewObjs = false; \
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
    bool protectNewObjs; /** Whether new objects created should automatically be protected. */
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
