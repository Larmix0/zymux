#ifndef COMPILER_H
#define COMPILER_H

#include "emitter.h"
#include "node.h"
#include "object.h"

/** Represents a variable's information at compilation time. */
typedef struct {
    bool isPrivate; /** Whether it's private or publicly accessible from the outside. */
    bool isConst; /** Whether it's changeable. */
    Token name; /** Name of the variable. */
    u32 scope; /** How deep is the variable's scope relative to the closure it's on. */
} Variable;

/** An array of variables that a specific closure has (which is 1 or more scopes inside a func). */
DECLARE_DA_STRUCT(ClosedVariables, Variable);

/** The array of variable's inside a function, which may include multiple scopes. */
DECLARE_DA_STRUCT(ClosedVariablesArray, ClosedVariables);

/** An array of unsigned 32 bit integers to hold indices of arrays in them. */
DECLARE_DA_STRUCT(U32Array, u32);

/** The compiler for a Zymux program. */
typedef struct Compiler {
    bool trackPositions; /** Whether or not we should store the positions of each bytecode. */
    ZmxProgram *program; /** Zymux program to hold the program's information. */
    NodeArray ast; /** The whole AST that is being compiled. */
    FuncObj *func; /** The "function" whose bytecode is currently being added while compiling. */

    ClosedVariables globals; /** Covers just the top-level global variables scope. */
    ClosedVariablesArray locals; /** All non-global variables as an array of func closures. */

    JumpArray jumps; /** An array of jumps which may change during patchings. */
    U32Array continues; /** Bytecode indices where new continues jump to if inside a loop. */
    U32Array breaks; /** Bytecode indices where new breaks jump to if inside a loop. */
    U32Array loopScopes; /** An array of scopes which are considered loop scopes. */

    u32 scopeDepth; /** How deep the current scope is. */
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
