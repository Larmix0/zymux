#ifndef PROGRAM_H
#define PROGRAM_H

#include <stdbool.h>

#include "hash_table.h"

typedef struct Node Node;
typedef struct Obj Obj;
typedef struct StringObj StringObj;
typedef struct BoolObj BoolObj;
typedef struct NullObj NullObj;

/** Stores the state of Zymux throughout the entire execution of the program. */
typedef struct ZmxProgram {
    bool hasErrored;
    bool showErrors;
    
    Node *allNodes;
    Obj *allObjs;

    StringObj *mainFile;
    StringObj *currentFile;

    Table internedStrings;
    NullObj *internedNull;
    BoolObj *internedTrue;
    BoolObj *internedFalse;
} ZmxProgram;

/** Returns an initialized zymux program with the parameters. */
ZmxProgram create_zmx_program(char *file, const bool showErrors);

/** Frees generally everything held by program. */
void free_zmx_program(ZmxProgram *program);

#endif
