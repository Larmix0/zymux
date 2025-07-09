#ifndef GC_H
#define GC_H

#include <stdbool.h>

#include "constants.h"
#include "dynamic_array.h"
#include "object.h"

/** Multiplier for how many more bytes need to be allocated before another collection. */
#define COLLECTION_GROWTH 2

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
    VulnerableObjs startupVulnObjs; /** For vulnerables before runtime execution. */
    Vm *vm; /** The single VM in the program to mark its objects as reachable. */

    u64 allocated; /** Amount of bytes allocated so far. */
    u64 nextCollection; /** Amount of bytes that need to be allocated before collecting again. */

    /** 
     * The mutex used for critical GC operations that must be in sync (like collection).
     * 
     * It's an allocated pointer to a mutex so that the GC can be safely passed around as a value,
     * with the only thing being copied the address of the mutex, not its information.
     */
    Mutex *lock;
} Gc;

/** Creates an empty garbage collector. */
Gc create_gc(ZmxProgram *program);

/** Frees the memory the garbage collector has allocated. */
void free_gc(Gc *gc);

/** Goes through the garbage collection phase by seeing reachable objects in the program's GC. */
void gc_collect(ZmxProgram *program);

#endif
