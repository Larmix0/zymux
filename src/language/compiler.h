#ifndef COMPILER_H
#define COMPILER_H

#include "dynamic_array.h"
#include "emitter.h"
#include "node.h"
#include "object.h"

typedef struct Resolver Resolver;

/** The compiler for a Zymux program. */
typedef struct Compiler {
    ZmxProgram *program; /** Zymux program to hold the program's information. */
    NodeArray ast; /** The whole AST that is being compiled. */
    bool isMain; /** Whether we're compiling the first executed file or an imported module. */

    JumpArray jumps; /** An array of jumps which may change during patchings. */
    U32Array continues; /** Bytecode indices where unpatched continues are. */
    U32Array breaks; /** Bytecode indices where unpatched breaks are. */

    /** The "function" whose bytecode is being emitted. Toplevel is also implicitly a function. */
    FuncObj *func; 
} Compiler;

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast, const bool isMain);

/** 
 * Compiles bytecode from the AST inside compiler into its func.
 * 
 * Returns whether or not we've errored.
 */
bool compile(Compiler *compiler);

/** Returns a compiled function or NULL if it errored. */
FuncObj *compile_file_source(ZmxProgram *program, char *source, const bool isMain);

/** 
 * Returns a compiled function (NULL if failed) for a REPL source line.
 *
 * This is made for REPL as it takes the one resolver and doesn't free it in case of an error.
 */
FuncObj *compile_repl_source(ZmxProgram *program, char *source, Resolver *resolver);

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler);

#endif
