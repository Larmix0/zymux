#ifndef RESOLVER_H
#define RESOLVER_H

#include "dynamic_array.h"
#include "node.h"

#define UNRESOLVED_NUMBER (-1) /** Default for a number to be resolved later. */
#define UNRESOLVED_NAME_SCOPE (VAR_UNRESOLVED) /** Default scope for unresolved names. */

/** An array for variable resolution addresses */
DECLARE_DA_STRUCT(VarResolutionArray, VarResolution *);

/** Represents a variable and its resolution information at compilation time. */
typedef struct {
    bool isPrivate; /** Whether it's private or publicly accessible from the outside. */
    bool isConst; /** Whether it's changeable. */
    u32 scope; /** How deep is the variable's scope relative to the closure it's on. */
    Token name; /** Name of the variable. */

    /** The current variable's resolution, which all referenced resolutions should be set to. */
    VarResolution currentResolution;

    /** 
     * An array to the addresses of all resolutions to a specific variable.
     * This is useful so that if we want to modify some resolution info of a variable,
     * we can walk this array of addresses and modify all resolutions stored inside nodes
     * that have resolutions relating to the given variable.
     * 
     * For example, if we have a declaration node for a variable, and 2 var get nodes,
     * then all their 3 resolution's addresses will be stored here, so that if the variable's
     * scope/index gets modified during the resolution phase, we modify all 3 nodes'
     * resolution info.
     */
    VarResolutionArray resolutions;
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

    ClosedVariables globals; /** An array of global only variables. */
    ClosedVariablesArray locals; /** All variables excluding globals as an array of closures. */
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
