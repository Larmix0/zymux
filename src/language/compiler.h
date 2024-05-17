#ifndef COMPILER_H
#define COMPILER_H

#include "node.h"
#include "object.h"

/** The compiler for a Zymux program. */
typedef struct Compiler {
    ZmxProgram *program; /** Zymux program to hold the program's information. */
    NodeArray ast; /** The whole AST that is being compiled. */
    FuncObj *func; /** The "function" whose bytecode is currently being added while compiling. */
    bool isDebugging; /** Whether or not we should store the positions of each bytecode. */
} Compiler;

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, NodeArray ast, bool isDebugging);

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler);

/** Compiles all the bytecode of the passed. */
bool compile(Compiler *compiler);

#endif
