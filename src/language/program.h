#ifndef PROGRAM_H
#define PROGRAM_H

#include <stdbool.h>

#include "gc.h"
#include "hash_table.h"

typedef struct Node Node;

typedef struct Obj Obj;
typedef struct StringObj StringObj;
typedef struct BoolObj BoolObj;
typedef struct NullObj NullObj;

/** Stores the state of Zymux throughout the entire execution of the program. */
typedef struct ZmxProgram {
    bool hasErrored; /** Whether the program has had any kind of errors. */
    bool showErrors; /** Whether or not error messages should be displayed and printed. */
    
    Node *allNodes; /** All nodes in a linked list to make freeing them easier */
    Obj *allObjs; /** All objects in a linked list to make freeing them easier (and for the GC). */
    
    StringObj *currentFile; /** The name of the file currently being worked on. */
    Table builtIn; /** The built-in functionality as name key and native object value pairs. */
    
    Table internedStrings; /** Hash set of all strings that are interned. */
    NullObj *internedNull; /** An interned null. */
    BoolObj *internedTrue; /** An interned boolean of true. */
    BoolObj *internedFalse; /** An interned boolean of false. */

    Gc gc; /** The garbage collector for the program's objects. */
} ZmxProgram;

/** Returns an initialized zymux program with the parameters. */
ZmxProgram create_zmx_program(char *file, const bool showErrors);

/** Frees generally everything held by program. */
void free_zmx_program(ZmxProgram *program);

#endif
