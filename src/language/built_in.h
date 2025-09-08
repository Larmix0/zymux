#ifndef BUILT_IN
#define BUILT_IN

#include "constants.h"
#include "hash_table.h"

typedef struct Obj Obj;
typedef struct ThreadObj ThreadObj;
typedef struct Vm Vm;
typedef struct ZmxProgram ZmxProgram;
typedef struct ClassObj ClassObj;

/** 
 * A signature for native functions.
 * 
 * Returns an object or NULL.
 * Functions that don't have a return value will return a language null object,
 * while a runtime error causes the function to return a C NULL.
 */
typedef Obj *(*NativeFunc)(ThreadObj *thread, Obj *callee, Obj **args);

/** A struct which holds all the built-ins of a program. */
typedef struct {
    Table funcs; /** Name (string) keys, native function values. */
    Table modules; /** Name (string) keys, native module values. */
    ClassObj *intClass;
    ClassObj *floatClass;
    ClassObj *listClass;
    ClassObj *mapClass;
    ClassObj *fileClass;
    ClassObj *threadClass;
    ClassObj *lockClass;
} BuiltIns;

/** Loads all built-ins into the built-in struct in the program inside the passed vulnerabes. */
void load_built_ins(VulnerableObjs *vulnObjs);

/** Returns a built-ins struct which is empty (nothing loaded yet). */
BuiltIns empty_built_ins();

/** Frees the memory allocated and owned by built-ins (excluding objs - they're program owned). */
void free_built_ins(BuiltIns *builtIn);

#endif
