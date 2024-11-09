#ifndef COMPILER_H
#define COMPILER_H

#include "dynamic_array.h"
#include "emitter.h"
#include "node.h"
#include "object.h"

/** The compiler for a Zymux program. */
typedef struct Compiler {
    bool trackPositions; /** Whether or not we should store the positions of each bytecode. */
    ZmxProgram *program; /** Zymux program to hold the program's information. */
    NodeArray ast; /** The whole AST that is being compiled. */

    JumpArray jumps; /** An array of jumps which may change during patchings. */
    U32Array continues; /** Bytecode indices where unpatched continues are. */
    U32Array breaks; /** Bytecode indices where unpatched breaks are. */

    /** The "function" whose bytecode is being emitted. Toplevel is also implicitly a function. */
    FuncObj *func; 
} Compiler;

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast, bool trackPositions);

/** 
 * Compiles bytecode from the AST inside compiler into its func.
 * 
 * Returns whether or not we've errored.
 */
bool compile(Compiler *compiler);

/** 
 * Returns a compiled compiler. Either it's compiled or everything is set to 0/NULL if errored.
 * 
 * trackPositions is whether we should store positions for bytecodes or not.
 */
Compiler compile_source(ZmxProgram *program, char *source, const bool trackPositions);

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler);

#endif
