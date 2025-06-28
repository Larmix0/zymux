#ifndef BUILT_IN
#define BUILT_IN

#include "constants.h"
#include "hash_table.h"

typedef struct Obj Obj;
typedef struct Vm Vm;
typedef struct ZmxProgram ZmxProgram;
typedef struct ClassObj ClassObj;

/** 
 * A signature for native functions.
 * 
 * Return some object or NULL.
 * Functions that don't have a return value will return a language null object,
 * while a straight up error causes the function to return a C NULL.
 */
typedef Obj *(*NativeFunc)(Vm *vm, Obj *callee, Obj **args);

/** A struct which holds all the built-ins of a program. */
typedef struct {
    Table funcs;
    ClassObj *fileClass;
} BuiltIns;

/** Loads all built-ins into the built-in struct inside the program. */
void load_built_ins(ZmxProgram *program);

/** Returns a built-ins struct which is empty (nothing loaded yet). */
BuiltIns empty_built_ins();

/** Frees the memory allocated and owned by built-ins (excludeing objs - they're program owned). */
void free_built_ins(BuiltIns *builtIn);

#endif
