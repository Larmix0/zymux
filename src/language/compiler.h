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

    /** The vulnerable objects of the compiler (this is expected to be shared with a root). */
    VulnerableObjs *vulnObjs;
} Compiler;

/** 
 * Returns a compiler with the passed AST.
 * 
 * The function inside the returned compiler automatically gets protected in the special protection
 * pool of the "vulnerables" passed.
 * It has to be manually unprotected later when the functionn is put
 * inside a safe runtime root, or is no longer needed.
 */
Compiler create_compiler(VulnerableObjs *vulnObjs, const NodeArray ast, const bool isMain);

/** Compiles bytecode from the AST inside compiler into its func. */
void compile(Compiler *compiler);

/** Returns a compiled function protected inside the passed vulnerables, or NULL if it errored. */
FuncObj *compile_file_source(VulnerableObjs *vulnObjs, char *source, const bool isMain);

/** 
 * Returns a compiled function protected inside vulnerables (NULL if failed) for a REPL line.
 *
 * This is made for REPL as it takes the one resolver and doesn't free it in case of an error.
 */
FuncObj *compile_repl_source(VulnerableObjs *vulnObjs, char *source, Resolver *resolver);

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler);

#endif
