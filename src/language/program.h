#ifndef PROGRAM_H
#define PROGRAM_H

#include <stdbool.h>

#include "cli_handler.h"
#include "gc.h"
#include "hash_table.h"
#include "thread.h"
#include "token.h"

typedef struct Node Node;

typedef struct Obj Obj;
typedef struct StringObj StringObj;
typedef struct BoolObj BoolObj;
typedef struct NullObj NullObj;

/** An array of strings, which is just an array of pointers to string locations. */
DECLARE_DA_STRUCT(StringArray, char *);

/** Stores the state of Zymux throughout the entire execution of the program. */
typedef struct ZmxProgram {
    bool hasErrored; /** Whether the program has had any kind of errors. */
    bool showErrors; /** Whether or not error messages should be displayed and printed. */
    bool isRuntime; /** Whether or not runtime execution has started. */
    CliHandler *cli; /** The command line's information (such as arguments and exit codes). */

    StringArray allTokenStrings; /** All strings allocated for tokens in an array. */
    Node *allNodes; /** All nodes in a linked list to make freeing them easier */
    Obj *allObjs; /** All objects in a linked list to make freeing them easier (and for the GC). */
    
    Table internedStrings; /** Hash set of all strings that are interned. */
    NullObj *internedNull; /** An interned null. */
    BoolObj *internedTrue; /** An interned boolean of true. */
    BoolObj *internedFalse; /** An interned boolean of false. */

    StringObj *currentFile; /** The name of the file currently being worked on. */
    BuiltIns builtIn; /** All the built-ins of the language. */
    Gc gc; /** The garbage collector for the program's objects. */

    Mutex printLock; /** The lock for safe, synchronized printing. */
} ZmxProgram;

/** Returns an initialized, allocated zymux program with the parameters. */
ZmxProgram *new_zmx_program(char *file, CliHandler *cli, const bool showErrors);

/** Frees generally everything held by program. */
void free_zmx_program(ZmxProgram *program);

#endif
