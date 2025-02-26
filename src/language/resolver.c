#include "resolver.h"

/** The outermost closure currently being resolved. */
#define CURRENT_CLOSURE(resolver) (&LAST_ITEM_DA(&(resolver)->locals))

/** The outermost closure's local variables. */
#define CURRENT_LOCALS(resolver) (&CURRENT_CLOSURE(resolver)->vars)

/** The outermost closure's captured variables. */
#define CURRENT_CAPTURED(resolver) (&CURRENT_CLOSURE(resolver)->captured)

static void capture_local(Resolver *resolver, Variable *captured, const u32 closureIdx);

/** Holds some type of scope that is usually created by brace blocks.*/
typedef enum {
    SCOPE_NORMAL, /** A non-special block scope, which is also encapsulated by all other types. */
    SCOPE_LOOP, /** Indicates that the scope was created by some loop. */
    SCOPE_FUNC /** A scope made for any kind of function (normal, method, initializer, etc.). */
} ScopeType;

static void resolve_node(Resolver *resolver, Node *node);
static void resolve_node_array(Resolver *resolver, NodeArray *nodes);

/** Returns an initialized resolution-time closure state that is empty. */
static ClosedVariables empty_closure() {
    ClosedVariables closure = {
        .isClosure = false,
        .vars = CREATE_DA(), .captured = CREATE_DA(), .localCaptures = CREATE_DA()
    };
    return closure;
}

/** Returns a starting resolver for the passed AST. */
Resolver create_resolver(ZmxProgram *program, NodeArray ast) {
    Resolver resolver = {
        .program = program, .ast = ast, .globals = empty_closure(), .locals = CREATE_DA(),
        .scopeDepth = 0, .loopScopes = CREATE_DA()
    };
    // Add the permanent closure for locals not covered by functions (like vars in global loops).
    APPEND_DA(&resolver.locals, empty_closure());
    return resolver;
}

/** Returns a variable, which has its name in a token along some extra resolution information. */
static Variable create_variable(
    const bool isPrivate, const bool isConst, const bool isLocalCapture,
    const Token name, const u32 scope, const VarResolution resolution
) {
    Variable var = {
        .isPrivate = isPrivate, .isConst = isConst, .isLocalCapture = isLocalCapture,
        .name = name, .scope = scope, .resolution = resolution, .resolutions = CREATE_DA()
    };
    return var;
}

/** Frees all the variables and their contents in an array of resolution-phase variables. */
static void free_variables(VariableArray *variables) {
    for (u32 i = 0; i < variables->length; i++) {
        FREE_DA(&variables->data[i].resolutions);
    }
    FREE_DA(variables);
}

/** Frees all the allocated memory of a resolution-time closure. */
static void free_closure(ClosedVariables *closure) {
    free_variables(&closure->vars);
    free_variables(&closure->captured);
    FREE_DA(&closure->localCaptures);
}

/** Frees all the owned and allocated memory of the resolver. */
void free_resolver(Resolver *resolver) {
    free_closure(&resolver->globals);
    for (u32 i = 0; i < resolver->locals.length; i++) {
        free_closure(&resolver->locals.data[i]);
    }
    FREE_DA(&resolver->locals);
    FREE_DA(&resolver->loopScopes);
}

/** Returns a created resolution for some variable. */
static VarResolution create_var_resolution(const i64 index, const VarScope scopeType) {
    VarResolution resolution = {.index = index, .scope = scopeType};
    return resolution;
}

/** Reports a resolution error on a specific position. */
static void resolution_error(
    Resolver *resolver, const SourcePosition pos, const char *format, ...
) {
    va_list args;
    va_start(args, format);
    zmx_user_error(resolver->program, pos, "Resolution error", format, &args);
    va_end(args);
}

/** Returns the amount of captured variables which were locally declared in the passed closure. */
static u32 get_local_captures(ClosedVariables *closure) {
    u32 total = 0;
    for (u32 i = 0; i < closure->localCaptures.length; i++) {
        total += closure->localCaptures.data[i];
    }
    return total;
}

/** Returns a variable inside the locals of the passed closure. Returns NULL if it's not found. */
static Variable *lookup_local(ClosedVariables *closure, const Token name) {
    for (i64 i = (i64)closure->vars.length - 1; i >= 0; i--) {
        if (equal_token(closure->vars.data[i].name, name)) {
            return &closure->vars.data[i];
        }
    }
    return NULL;
}

/** Returns a variable inside the captures of the passed closure. Returns NULL if it's not found. */
static Variable *lookup_captured(ClosedVariables *closure, const Token name) {
    for (i64 i = (i64)closure->captured.length - 1; i >= 0; i--) {
        if (equal_token(closure->captured.data[i].name, name)) {
            return &closure->captured.data[i];
        }
    }
    return NULL;
}

/** 
 * Returns a variable's address in the passed closure whose name is the same as the passed token.
 * 
 * Returns NULL by default if the variable doesn't exist in the closure (captured or not).
 * Searches through the locals of the closure first, then searches through the captured variables.
 * 
 * The search is done in reverse, reading the most recently appended variable first, which is
 * the order the language's semantics read with.
 */
static Variable *find_variable(ClosedVariables *closure, const Token name) {
    Variable *result = lookup_local(closure, name);
    if (result) {
        return result;
    }

    result = lookup_captured(closure, name);
    return result == NULL ? NULL : result;
}

/** 
 * Returns the variable in the passed variable array searching only in the top scope of it.
 * Returns NULL if the variable doesn't exist within the topmost scope of that array.
 */
static Variable *get_top_scope_var(VariableArray variables, Token name, const u32 scope) {
    for (i64 i = (i64)variables.length - 1; i >= 0; i--) {
        if (variables.data[i].scope != scope) {
            return NULL;
        }
        if (equal_token(variables.data[i].name, name)) {
            return &variables.data[i];
        }
    }
    return NULL;
}

/** Returns a copy of the variable that has its own allocated memory. */
static Variable copy_variable(const Variable original) {
    Variable copied = create_variable(
        original.isPrivate, original.isConst, original.isLocalCapture,
        original.name, original.scope, original.resolution
    );
    for (u32 i = 0; i < original.resolutions.length; i++) {
        APPEND_DA(&copied.resolutions, original.resolutions.data[i]);
    }
    return copied;
}

/**
 * Appends a new closure to the resolver that inherits the captured of the closure above it.
 * 
 * If one of the moved captured variables has a local, non-captured version inside that closure,
 * then capture and use that instead of the already captured one.
 * This is because the already captured one is likely more outerscope and supposed to be shadowed.
 */
static void append_new_closure(Resolver *resolver) {
    ClosedVariables *prevClosure = CURRENT_CLOSURE(resolver);
    const u32 prevClosureIdx = resolver->locals.length - 1;

    APPEND_DA(&resolver->locals, empty_closure());
    if (prevClosure->captured.length > 0) {
        CURRENT_CLOSURE(resolver)->isClosure = true; // Already becomes a closure just in case.
    }
    for (u32 i = 0; i < prevClosure->captured.length; i++) {
        Variable current = prevClosure->captured.data[i];

        // TODO: Use captured lookup here?
        for (i64 j = (i64)prevClosure->vars.length - 1; j >= 0; j--) {
            if (equal_token(prevClosure->vars.data[j].name, prevClosure->captured.data[i].name)) {
                capture_local(resolver, &prevClosure->vars.data[j], prevClosureIdx);
                current = LAST_ITEM_DA(&prevClosure->captured);
                APPEND_DA(CURRENT_CAPTURED(resolver), current);
                return;
            }
        }
        // Didn't find an uncaptured local, so copy the capture we had.
        APPEND_DA(CURRENT_CAPTURED(resolver), copy_variable(current));
    }
}

/** Adds a scope that's one layer deeper (usually from entering a new block scope). */
static void push_scope(Resolver *resolver, const ScopeType type) {
    resolver->scopeDepth++;
    APPEND_DA(&CURRENT_CLOSURE(resolver)->localCaptures, 0); // Start new captures scope with 0.

    if (type == SCOPE_LOOP) {
        APPEND_DA(&resolver->loopScopes, resolver->scopeDepth);
    } else if (type == SCOPE_FUNC) {
        append_new_closure(resolver);
    }
}

/** 
 * Pops all variables from a variable array which are in the topmost scope.
 * 
 * Returns the amount popped.
 */
static u32 pop_variable_array_scope(Resolver *resolver, VariableArray *variables) {
    u32 popped = 0;
    while (variables->length > 0 && LAST_ITEM_DA(variables).scope == resolver->scopeDepth) {
        FREE_DA(&LAST_ITEM_DA(variables).resolutions);
        DROP_DA(variables);
        popped++;
    }
    return popped;
}

/** Removes 1 type of scope from the locals (including that scope's captured locals). */
static void finish_scope(Resolver *resolver, const ScopeType type) {
    resolver->scopeDepth--;
    DROP_DA(&CURRENT_CLOSURE(resolver)->localCaptures);

    if (type == SCOPE_LOOP) {
        DROP_DA(&resolver->loopScopes);
    } else if (type == SCOPE_FUNC) {
        ASSERT(resolver->locals.length > 1, "Attempted to pop the permanent top-level closure.");
        free_closure(CURRENT_CLOSURE(resolver));
        DROP_DA(&resolver->locals);
    }
}

/** Collapses the outermost scope (both its captured and non-captured locals). */
static void pop_scope(Resolver *resolver, ScopeType type) {
    pop_variable_array_scope(resolver, CURRENT_LOCALS(resolver));
    pop_variable_array_scope(resolver, CURRENT_CAPTURED(resolver));
    finish_scope(resolver, type);
}

/** Collapses a scope and assign the amount of variables popped to the passed block's resolution. */
static void pop_scope_block(Resolver *resolver, BlockNode *block, ScopeType type) {
    block->localsAmount = pop_variable_array_scope(resolver, CURRENT_LOCALS(resolver));
    block->capturedAmount = pop_variable_array_scope(resolver, CURRENT_CAPTURED(resolver));
    finish_scope(resolver, type);
}

/** Resolves the expressions within a string node. Which can include exprs in interpolation. */
static void resolve_string(Resolver *resolver, StringNode *node) {
    for (u32 i = 0; i < node->exprs.length; i++) {
        resolve_node(resolver, node->exprs.data[i]);
    }
}

/** Resolves a unary node (the right hand side having unary being applied to it). */
static void resolve_unary(Resolver *resolver, const UnaryNode *node) {
    resolve_node(resolver, node->rhs);
}

/** Resolves a binary node (the two operand nodes). */
static void resolve_binary(Resolver *resolver, const BinaryNode *node) {
    resolve_node(resolver, node->lhs);
    resolve_node(resolver, node->rhs);
}

/** Resolves the expression which is wrapped in the parentheses node. */
static void resolve_parentheses(Resolver *resolver, const ParenthesesNode *node) {
    resolve_node(resolver, node->expr);
}

/** Resolves a range's start, end, and step nodes in that order. */
static void resolve_range(Resolver *resolver, RangeNode *node) {
    resolve_node(resolver, node->start);
    resolve_node(resolver, node->end);
    resolve_node(resolver, node->step);
}

/** Resolves a call expression's callee and arguments. */
static void resolve_call(Resolver *resolver, CallNode *node) {
    resolve_node(resolver, node->callee);
    resolve_node_array(resolver, &node->args);
}

/** Resolves the expression wrapped in an expression-statement. */
static void resolve_expr_stmt(Resolver *resolver, const ExprStmtNode *node) {
    resolve_node(resolver, node->expr);
}

/** Resolves a block's array of statements without pushing/popping a scope on top of it. */
static void block(Resolver *resolver, BlockNode *node) {
    resolve_node_array(resolver, &node->stmts);
}

/** Resolves a block's array of statements under the depth of a new passed scope. */
static void scoped_block(Resolver *resolver, BlockNode *node, const ScopeType type) {
    push_scope(resolver, type);
    block(resolver, node);
    pop_scope_block(resolver, node, type);
}

/** 
 * Declares a local variable, errors if it's already declared or is const
 * Simply puts a flag on the node which indicates that it's a local declaration.
 */
static void declare_local(Resolver *resolver, VarDeclNode *node) {
    const Token name = node->name;
    if (get_top_scope_var(*CURRENT_LOCALS(resolver), name, resolver->scopeDepth) != NULL) {
        resolution_error(
            resolver, name.pos, "Can't redeclare local name '%.*s'.", name.pos.length, name.lexeme
        );
    }
    Variable *captured = lookup_captured(CURRENT_CLOSURE(resolver), node->name);
    if (captured && captured->scope == resolver->scopeDepth && captured->isLocalCapture) {
        resolution_error(
            resolver, name.pos, "Can't redeclare local name '%.*s'.", name.pos.length, name.lexeme
        );
    }

    node->resolution.index = CURRENT_LOCALS(resolver)->length;
    node->resolution.scope = VAR_LOCAL;

    Variable declared = create_variable(
        false, node->isConst, false, name, resolver->scopeDepth, node->resolution
    );
    APPEND_DA(&declared.resolutions, &node->resolution);
    APPEND_DA(CURRENT_LOCALS(resolver), declared);
}

/** 
 * Declares a global variable, errors if already declared or is const.
 * Does so by appending the declared variable to the globals closure after setting its resolution.
 */
static void declare_global(Resolver *resolver, VarDeclNode *node) {
    const Token name = node->name;
    if (find_variable(&resolver->globals, name) != NULL) {
        resolution_error(
            resolver, name.pos, "Can't redeclare global name '%.*s'.",
            name.pos.length, name.lexeme
        );
    }

    node->resolution.index = CURRENT_LOCALS(resolver)->length;
    node->resolution.scope = VAR_GLOBAL;

    Variable declared = create_variable(
        false, node->isConst, false, name, resolver->scopeDepth, node->resolution
    );
    APPEND_DA(&declared.resolutions, &node->resolution);
    APPEND_DA(&resolver->globals.vars, declared);
}

/**
 * Resolves a new variable's declaration.
 * 
 * We use "name" when erroring out instead of "variable" because variable declaration nodes
 * can include other, non-variable names like declaring the names of functions.
 */
static void resolve_var_decl(Resolver *resolver, VarDeclNode *node) {
    resolve_node(resolver, node->value);
    
    const Token name = node->name;
    Obj *nameAsObj = AS_OBJ(new_string_obj(resolver->program, name.lexeme, name.pos.length));
    if (table_get(&resolver->program->builtIn, nameAsObj) != NULL) {
        resolution_error(
            resolver, name.pos, "Can't redeclare built-in name '%.*s'.",
            name.pos.length, name.lexeme
        );
    } else if (resolver->scopeDepth == 0) {
        declare_global(resolver, node);
    } else {
        declare_local(resolver, node);
    }
}


/** An error which occured due to an attempt to assign a constant variable. */
static void const_assign_error(Resolver *resolver, const Node *node, const Token name) {
    resolution_error(
        resolver, get_node_pos(AS_NODE(node)),
        "Can't assign to const variable '%.*s'.", name.pos.length, name.lexeme
    );
}

/** 
 * Adds a variable reference information (a resolution) to the variable.
 * 
 * This is done by setting the resolution to be the general one on the passed variable,
 * then appending the passed resolution itself to the variable's array of resolutions.
 * This makes it so that if the variable's resolution information changes, we can just go through
 * the resolution array and set all resolutions to the appropriate one.
 */
static void add_variable_reference(Variable *variable, VarResolution *resolution) {
    *resolution = variable->resolution;
    APPEND_DA(&variable->resolutions, resolution);
}

/**
 * Captures the local variable toCapture by putting it in all appropriate closure contexts.
 * 
 * Appends a version of toCapture to every closure between the one where the variable was declared
 * and the closure that's trying to access it (including those 2 closures themselves).
 * 
 * The difference between the appended versions of those captured variables in the closures
 * is just putting the correct resolution info for them relative to the closure they're in.
 */
static void capture_local(Resolver *resolver, Variable *toCapture, const u32 closureIdx) {
    U32Array *localCaptures = &resolver->locals.data[closureIdx].localCaptures;
    LAST_ITEM_DA(localCaptures)++;

    for (u32 i = closureIdx; i < resolver->locals.length; i++) {
        // The closure of the captured itself gets appended a truthy local capture.
        const bool isLocalCapture = i == closureIdx ? true : false;
        Variable closureCapture = create_variable(
            toCapture->isPrivate, toCapture->isConst, isLocalCapture,
            toCapture->name, toCapture->scope,
            create_var_resolution(resolver->locals.data[i].captured.length, VAR_CAPTURED)
        );
        APPEND_DA(&resolver->locals.data[i].captured, closureCapture);
        resolver->locals.data[i].isClosure = true;
    }
    toCapture->resolutions.data[0]->scope = VAR_CAPTURED; // Set declaration to a captured var one.
}

/** 
 * Searches through and tries to capture a variable or find one that was already captured.
 * Returns that variable, or NULL if it didn't find anything.
 * 
 * The way the search works is we're given closureIdx, then we search through the variables
 * of the closure at that index and see if we find a variable that has the passed name parameter.
 * If we don't find it, then the function recursively calls itself again, looking for the variable
 * one layer higher, which ensures we capture/get the closest variable.
 * 
 * If one is found, then we either simply return it if what we found was already captured, but if
 * it was a local variable, then we capture that variable first and return the captured version
 * instead.
 */
static Variable *search_captures(Resolver *resolver, const Token name, const i64 closureIdx) {
    ClosedVariables *closure = &resolver->locals.data[closureIdx];
    Variable *variable = find_variable(closure, name);
    if (variable == NULL) {
        if (closureIdx <= 0) {
            return NULL;
        }
        return search_captures(resolver, name, closureIdx - 1);
    }

    if (variable->resolution.scope != VAR_CAPTURED) {
        // Capture, then return that captured variable instead of the local we got from our search.
        capture_local(resolver, variable, closureIdx);
        return &LAST_ITEM_DA(CURRENT_CAPTURED(resolver));
    } else {
        return variable;
    }
}

/**
 * Tries to assign a variable which is findable within the current function/closure if one exists.
 * 
 * Returns whether or not a variable was found in the current closure and assigned to the node.
 * If one is found it also adds a reference to it in the node's resolution.
 */
static bool try_assign_local(Resolver *resolver, VarAssignNode *node) {
    Variable *local = find_variable(CURRENT_CLOSURE(resolver), node->name);
    if (local == NULL) {
        return false;
    }

    if (local->isConst) {
        const_assign_error(resolver, AS_NODE(node), node->name);
    }
    add_variable_reference(local, &node->resolution);
    return true;
}

/**
 * Tries to assign to a variable that isn't within the current function nor is global.
 * 
 * Returns whether or not we found a captured variable/captured a local one with the same name
 * and assigned that to the node.
 */
static bool try_assign_captured(Resolver *resolver, VarAssignNode *node) {
    if (resolver->locals.length < 2) {
        // Doesn't have enough functions to even capture variables.
        return false;
    }
    Variable *local = search_captures(resolver, node->name, resolver->locals.length - 2);
    if (local == NULL) {
        return false;
    }

    if (local->isConst) {
        const_assign_error(resolver, AS_NODE(node), node->name);
    }
    add_variable_reference(local, &node->resolution);
    return true;
}

/**
 * Tries to assign a global variable if one exists.
 * 
 * Returns whether or not a global was found and assigned to the node.
 * We give the assign node the resolution information
 * of the declared global variable, then put the node's own resolution address on the array
 * of resolutions so it can be modified later if needed.
 */
static bool try_assign_global(Resolver *resolver, VarAssignNode *node) {
    Variable *global = find_variable(&resolver->globals, node->name);
    if (global == NULL) {
        return false;
    }

    if (global->isConst) {
        const_assign_error(resolver, AS_NODE(node), node->name);
    }
    add_variable_reference(global, &node->resolution);
    return true;
}

/** 
 * Resolves a variable assignment expression.
 * 
 * Allows global, local and captured variable assignments. Errors if the assigned variable
 * doesn't exist or is a built-in.
 */
static void resolve_var_assign(Resolver *resolver, VarAssignNode *node) {
    resolve_node(resolver, node->value);
    const Token name = node->name;
    if (resolver->scopeDepth > 0) {
        if (try_assign_local(resolver, node) || try_assign_captured(resolver, node)) {
            return;
        }
    }
    if (try_assign_global(resolver, node)) {
        return;
    }

    // Guaranteed error at this point. We just try to put a more informative error message.
    Obj *nameAsObj = AS_OBJ(new_string_obj(resolver->program, name.lexeme, name.pos.length));
    if (table_get(&resolver->program->builtIn, nameAsObj) != NULL) {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node)), "Can't assign to built-in name '%.*s'.",
            name.pos.length, name.lexeme
        );
    } else {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node)), "Assigned variable '%.*s' doesn't exist.",
            name.pos.length, name.lexeme
        );
    }
}

/** 
 * Tries to find and resolve a variable which is findable within the current function.
 * Returns whether or not it found one and resolved it.
 */
static bool try_get_local(Resolver *resolver, VarGetNode *node) {
    Variable *local = find_variable(CURRENT_CLOSURE(resolver), node->name);
    if (local == NULL) {
        return false;
    }

    add_variable_reference(local, &node->resolution);
    return true;
}

/** 
 * Tries to find a variable that isn't within the current function nor is global.
 * Returns whether or not it found one and resolved it.
 */
static bool try_get_captured(Resolver *resolver, VarGetNode *node) {
    if (resolver->locals.length < 2) {
        // Doesn't have enough functions to even capture variables.
        return false;
    }
    Variable *local = search_captures(resolver, node->name, resolver->locals.length - 2);
    if (local == NULL) {
        return false;
    }

    add_variable_reference(local, &node->resolution);
    return true;
}

/** 
 * Tries to resolve a variable in the global scope if there is one.
 * 
 * Adds a reference to the global if one is found, which is a function that sets a node's resolution
 * and stores it inside the given variable in case it needs to be modified later.
 * Returns true if that happened, otherwise false if the global isn't found to begin with.
 */
static bool try_get_global(Resolver *resolver, VarGetNode *node) {
    Variable *global = find_variable(&resolver->globals, node->name);
    if (global == NULL) {
        return false;
    }

    add_variable_reference(global, &node->resolution);
    return true;
}

/** 
 * Tries to resolve a variable get if it's referencing a built-in name.
 * 
 * Returns whether or not the resolution was successful and the name in the node was a built-in.
 * We look in the table of built-ins and see if it's there. If it is, we manually set resolution
 * information about the node to be that of a built-in variable.
 */
static bool try_get_built_in(Resolver *resolver, Obj *nameAsObj, VarGetNode *node) {
    if (table_get(&resolver->program->builtIn, nameAsObj) == NULL) {
        return false;
    }
    
    node->resolution.scope = VAR_BUILT_IN;
    return true;
}

/** 
 * Resolves a name that is used to get the value of a variable.
 * 
 * Tries to get locals, then captured variables, then globals, and finally built-ins.
 * Errors if the variable isn't present in any of them.
 */
static void resolve_var_get(Resolver *resolver, VarGetNode *node) {
    const Token name = node->name;
    if (resolver->scopeDepth > 0) {
        if (try_get_local(resolver, node) || try_get_captured(resolver, node)) {
            return;
        }
    }
    Obj *nameAsObj = AS_OBJ(new_string_obj(resolver->program, name.lexeme, name.pos.length));
    if (try_get_global(resolver, node) || try_get_built_in(resolver, nameAsObj, node)) {
        return;
    }

    resolution_error(
        resolver, get_node_pos(AS_NODE(node)),
        "Variable '%.*s' doesn't exist.", name.pos.length, name.lexeme
    );
}

/** Resolves the condition and branches of an if-else statement. */
static void resolve_if_else(Resolver *resolver, IfElseNode *node) {
    resolve_node(resolver, node->condition);
    resolve_node(resolver, AS_NODE(node->ifBranch));
    resolve_node(resolver, node->elseBranch);
}

/** Resolves a while loop's condition and its body statements inside a loop scope. */
static void resolve_while(Resolver *resolver, WhileNode *node) {
    resolve_node(resolver, node->condition);
    scoped_block(resolver, node->body, SCOPE_LOOP);
}

/** Resolves a do-while loop, which is like a while, but its condition's resolved after the body. */
static void resolve_do_while(Resolver *resolver, DoWhileNode *node) {
    scoped_block(resolver, node->body, SCOPE_LOOP);
    resolve_node(resolver, node->condition);
}

/** 
 * Resolves a for loop statement.
 * 
 * For loops declare 2 scopes: a first, non-loop scope which it puts its loop variable and iterator
 * on, then the 2nd deeper scope, which is a loop scope that encapsulates
 * the loop's inner statements.
 * 
 * The reason for having the loop var + iterator scope be a different one from the loop itself is so
 * that jump statements (like break or continue) don't accidentally pop the iterator and loop 
 * variable.
 * For break, it already jumps to the place where the loop variable
 * and iterator pop instructions are, so we don't want to end up popping them multiple times.
 */
static void resolve_for(Resolver *resolver, ForNode *node) {
    push_scope(resolver, SCOPE_NORMAL);

    const u32 loopVarSpot = CURRENT_LOCALS(resolver)->length;
    resolve_var_decl(resolver, node->loopVar);
    APPEND_DA(
        CURRENT_LOCALS(resolver),
        create_variable(
            false, false, false, create_token("<iter>", 0), resolver->scopeDepth,
            create_var_resolution(loopVarSpot + 1, VAR_LOCAL)
        )
    );
    resolve_node(resolver, node->iterable);
    scoped_block(resolver, node->body, SCOPE_LOOP);

    pop_scope(resolver, SCOPE_NORMAL);
}

/** 
 * Returns the amount of variables declared in the deepest loop relative to an array of variables.
 * 
 * The variables included are the ones in the passed variable array that have a scope equal to
 * or deeper to the scope of the innermost loop. In other words, it includes the variables
 * that were created inside the loop at some point.
 */
static u32 get_loop_vars_amount(Resolver *resolver, VariableArray *variables) {
    ASSERT(
        resolver->loopScopes.length > 0, "Tried to count loop variables, but not inside a loop."
    );
    if (variables->length == 0) {
        return 0; // No variables created at any scope.
    }

    u32 amount = 0;
    const u32 deepestLoop = LAST_ITEM_DA(&resolver->loopScopes);
    for (i64 i = variables->length - 1; i >= 0; i--) {
        if (variables->data[i].scope < deepestLoop) {
            break; // Below loop, done.
        }
        amount++;
    }
    return amount;
}

/** 
 * Resolves a loop control statement (like break or continue).
 * 
 * Puts the amount of pops that'll be emitted from skipping the loop iteration inside the node,
 * which is the number of variables declared on the loop before encountering the loop control
 * statement.
 * Includes both normal locals declared on the loop, and declared locals that were captured.
 */
static void resolve_loop_control(Resolver *resolver, LoopControlNode *node) {
    if (resolver->loopScopes.length == 0) {
        const char *stringRepr = node->keyword == TOKEN_BREAK_KW ? "break" : "continue";
        resolution_error(resolver, node->pos, "Can only use '%s' inside a loop.", stringRepr);
        return;
    }

    node->localsAmount = get_loop_vars_amount(resolver, CURRENT_LOCALS(resolver));
    node->capturedAmount = get_loop_vars_amount(resolver, CURRENT_CAPTURED(resolver));
}

/**
 * Resolves a function, which includes its parameters and its body inside a function-type scope.
 * 
 * Manually appends the function's name a second time to the recently created locals of itself
 * in order to allow recursion by accessinig index 0 inside BP at runtime
 * (as BP starts on the callee).
 */
static void resolve_func(Resolver *resolver, FuncNode *node) {
    resolve_var_decl(resolver, node->nameDecl);
    push_scope(resolver, SCOPE_FUNC);
    Variable declared = create_variable(
        false, node->nameDecl->isConst, false,
        node->nameDecl->name, resolver->scopeDepth, node->nameDecl->resolution
    );
    APPEND_DA(CURRENT_LOCALS(resolver), declared);

    for (u32 i = 0; i < node->params.length; i++) {
        // TODO: make this also check for other stuff potentially when optional params are added.
        ASSERT(node->params.data[i]->type == AST_LITERAL, "Parameter is not a variable literal.");

        // +1 on the variable resolution created because the 0th spot is occupied by the func name.
        const Token varName = AS_PTR(LiteralNode, node->params.data[i])->value;
        const Variable parameter = create_variable(
            false, false, false,
            varName, resolver->scopeDepth, create_var_resolution(i + 1, VAR_LOCAL)
        );
        APPEND_DA(CURRENT_LOCALS(resolver), parameter);
    }
    block(resolver, node->body);
    node->isClosure = CURRENT_CLOSURE(resolver)->isClosure;
    pop_scope_block(resolver, node->body, SCOPE_FUNC);
}

/** Resolves a return by resolving the returned value and erroring if we're not inside a func. */
static void resolve_return(Resolver *resolver, ReturnNode *node) {
    if (resolver->locals.length < 2) {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node)), "Can't return outside a function scope."
        );
    }
    resolve_node(resolver, node->returnValue);
    node->capturedPops = get_local_captures(CURRENT_CLOSURE(resolver));
}

/** Resolves the passed nodes and anything inside it that requires resolution. */
static void resolve_node(Resolver *resolver, Node *node) {
    if (node == NULL) {
        return; // Nothing in the node.
    }

    switch (node->type) {
    case AST_STRING: resolve_string(resolver, AS_PTR(StringNode, node)); break;
    case AST_UNARY: resolve_unary(resolver, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: resolve_binary(resolver, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: resolve_parentheses(resolver, AS_PTR(ParenthesesNode, node)); break;
    case AST_RANGE: resolve_range(resolver, AS_PTR(RangeNode, node)); break;
    case AST_CALL: resolve_call(resolver, AS_PTR(CallNode, node)); break;
    case AST_EXPR_STMT: resolve_expr_stmt(resolver, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: scoped_block(resolver, AS_PTR(BlockNode, node), SCOPE_NORMAL); break;
    case AST_VAR_DECL: resolve_var_decl(resolver, AS_PTR(VarDeclNode, node)); break;
    case AST_VAR_ASSIGN: resolve_var_assign(resolver, AS_PTR(VarAssignNode, node)); break;
    case AST_VAR_GET: resolve_var_get(resolver, AS_PTR(VarGetNode, node)); break;
    case AST_IF_ELSE: resolve_if_else(resolver, AS_PTR(IfElseNode, node)); break;
    case AST_WHILE: resolve_while(resolver, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: resolve_do_while(resolver, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: resolve_for(resolver, AS_PTR(ForNode, node)); break;
    case AST_LOOP_CONTROL: resolve_loop_control(resolver, AS_PTR(LoopControlNode, node)); break;
    case AST_FUNC: resolve_func(resolver, AS_PTR(FuncNode, node)); break;
    case AST_RETURN: resolve_return(resolver, AS_PTR(ReturnNode, node)); break;

    case AST_LITERAL:
    case AST_KEYWORD:
    case AST_EOF:
    case AST_ERROR:
        break; // Nothing to resolve.
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** Resolves an array of nodes. */
static void resolve_node_array(Resolver *resolver, NodeArray *nodes) {
    for (u32 i = 0; i < nodes->length; i++) {
        resolve_node(resolver, nodes->data[i]);
    }
}

/** 
 * Resolves all semantics of the AST inside the passed resolver.
 * 
 * Returns whether or not the semantic analysis was successful and didn't raise any errors.
 */
bool resolve(Resolver *resolver) {
    resolve_node_array(resolver, &resolver->ast);

    return !resolver->program->hasErrored;
}
