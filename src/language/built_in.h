#ifndef BUILT_IN
#define BUILT_IN

#include "constants.h"

typedef struct Obj Obj;
typedef struct Vm Vm;
typedef struct ZmxProgram ZmxProgram;

/** 
 * A signature for native functions.
 * 
 * Return some object or NULL.
 * Even if the function doesn't want to return anything, it should
 * return a null object. However, it will return a NULL pointer if an error occurred.
 */
typedef Obj *(*NativeFunc)(Vm *vm, Obj **args);

/** Loads all native objects into the program. */
void load_built_ins(ZmxProgram *program);

#endif
