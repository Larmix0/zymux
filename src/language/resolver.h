#ifndef RESOLVER_H
#define RESOLVER_H

#include "dynamic_array.h"
#include "node.h"

#define UNRESOLVED_NUMBER (-1) /** Default for a number to be resolved later. */
#define UNRESOLVED_NAME_SCOPE (NAME_UNRESOLVED) /** Default scope for unresolved names. */

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

/**
 * Holds resolution information of a given AST as its resolving its semantics.
 * 
 * The resolver is used for 2 purposes. One is to detect errors regarding the resolution
 * of different kinds of names and statements like a variable not existing, returning from
 * the global scope, and so on.
 * 
 * The other purpose is to put information inside the nodes about how they're supposed to be
 * compiled which wasn't set by the parser since it doesn't handle keeping track of semantics.
 * An example would be adding info about whether a variable assignment is local or global.
 */
typedef struct {
    ZmxProgram *program; /** Zymux program to hold the program's information. */
    NodeArray ast; /** The whole AST that is being resolved. */

    ClosedVariables globals; /** Covers just the top-level, user-defined global variables scope. */
    ClosedVariablesArray closures; /** All non-global variables as an array of variable closures. */

    U32Array loopScopes; /** An array of scopes which are considered loop scopes. */
    u32 scopeDepth; /** How deep the current scope is. */
} Resolver;

/** Returns a starting resolve for the passed AST. */
Resolver create_resolver(ZmxProgram *program, NodeArray ast);

/** Frees all the owned and allocated memory of the resolver. */
void free_resolver(Resolver *resolver);

/** 
 * Resolves all semantics of the AST inside the passed resolver.
 * 
 * Returns whether or not a semantic error occurred.
 */
bool resolve(Resolver *resolver);

#endif
