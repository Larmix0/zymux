#ifndef COMPILER_H
#define COMPILER_H

#include "node.h"
#include "object.h"

/** Represents a variable's information at compilation time. */
typedef struct {
    bool isPrivate; /** Whether it's private or publicly accessible from the outside. */
    bool isConst; /** Whether it's changeable. */
    Token name; /** Name of the variable. */
    u32 scope; /** How deep is the variable's scope. */
} Variable;

/** An array of variables at a given scope. */
DECLARE_DA_STRUCT(ClosedVariables, Variable);

/** The array of scopes (each function has its scope of variables). */
DECLARE_DA_STRUCT(ClosedVariablesArray, ClosedVariables);

/** The compiler for a Zymux program. */
typedef struct Compiler {
    bool isDebugging; /** Whether or not we should store the positions of each bytecode. */
    ZmxProgram *program; /** Zymux program to hold the program's information. */
    NodeArray ast; /** The whole AST that is being compiled. */
    FuncObj *func; /** The "function" whose bytecode is currently being added while compiling. */

    ClosedVariables globals; /** The array of global variables. */
    ClosedVariablesArray locals; /** All variables ordered as an array of func scope closures. */
    u32 scopeDepth; /** How deep the current scope is. */
} Compiler;

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast, bool isDebugging);

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler);

/** Compiles all the bytecode of the passed. */
bool compile(Compiler *compiler);

/** 
 * Returns a compiler whose main func is fully compiled.
 * 
 * debugByteCodePos controls whether or not we keep track of the positions of each bytecode,
 * and also whether or not we should print the lexed tokens, parsed AST, and compiled bytecode.
 */
FuncObj *compile_source(ZmxProgram *program, char *source, const bool debugBytecode);

#endif
