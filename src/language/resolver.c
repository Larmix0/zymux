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
        .program = program, .ast = ast,
        .globals = empty_closure(), .locals = CREATE_DA(),
        .loopScopes = CREATE_DA(), .scopeDepth = 0,
        .currentFunc = FUNC_NONE, .currentCls = CLS_NONE
    };
    // Add the permanent closure for locals not covered by functions (like vars in global loops).
    APPEND_DA(&resolver.locals, empty_closure());
    return resolver;
}

/** Returns a variable, which has its name in a token along some extra resolution information. */
static Variable create_variable(
    const bool isPrivate, const bool isConst, const bool isLocalCapture,
    const Token name, const u32 scope, const u32 index, const u32 closureIdx,
    const VarResolution resolution, FuncNode *paramFunc
) {
    Variable var = {
        .isPrivate = isPrivate, .isConst = isConst, .isLocalCapture = isLocalCapture,
        .name = name, .scope = scope, .index = index, .closureIdx = closureIdx,
        .resolution = resolution, .resolutions = CREATE_DA(), .paramFunc = paramFunc
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
static VarResolution create_var_resolution(const i64 index, const VarType scopeType) {
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

/** 
 * Returns a variable in the variable array which matches name. Returns NULL if it's not found.
 * 
 * The search is done in reverse, reading the most recently appended variable first, which is
 * the order the language's semantics read with.
 */
static Variable *find_variable_array_var(VariableArray *variables, const Token name) {
    for (i64 i = (i64)variables->length - 1; i >= 0; i--) {
        if (equal_token(variables->data[i].name, name)) {
            return &variables->data[i];
        }
    }
    return NULL;
}

/** 
 * Returns a variable in the passed closure whose name is the same as the passed token or NULL.
 * 
 * Searches through the locals of the closure first, then searches through the captured variables.
 * Returns NULL by default if the variable doesn't exist in the closure at all (local or captured).
 */
static Variable *find_closure_var(ClosedVariables *closure, const Token name) {
    Variable *result = find_variable_array_var(&closure->vars, name);
    return result ? result : find_variable_array_var(&closure->captured, name);
}

/** 
 * Returns the variable in the passed variable array searching only in the top scope of it.
 * Returns NULL if the variable doesn't exist within the topmost scope of that array.
 */
static Variable *find_top_scope_var(VariableArray variables, const Token name, const u32 scope) {
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
        original.name, original.scope, original.index, original.closureIdx,
        original.resolution, original.paramFunc
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
        for (i64 j = (i64)prevClosure->vars.length - 1; j >= 0; j--) {
            if (equal_token(prevClosure->vars.data[j].name, prevClosure->captured.data[i].name)) {
                capture_local(resolver, &prevClosure->vars.data[j], prevClosureIdx);
                APPEND_DA(CURRENT_CAPTURED(resolver), LAST_ITEM_DA(&prevClosure->captured));
                return;
            }
        }
        // Didn't find an uncaptured local, so copy the capture we had.
        APPEND_DA(CURRENT_CAPTURED(resolver), copy_variable(prevClosure->captured.data[i]));
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
static u32 pop_top_scope(Resolver *resolver, VariableArray *variables) {
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
static void pop_scope(Resolver *resolver, const ScopeType type) {
    pop_top_scope(resolver, CURRENT_LOCALS(resolver));
    pop_top_scope(resolver, CURRENT_CAPTURED(resolver));
    finish_scope(resolver, type);
}

/** Collapses a scope and assign the amount of variables popped to the passed block's resolution. */
static void pop_block_scope(Resolver *resolver, BlockNode *block, const ScopeType type) {
    block->localsAmount = pop_top_scope(resolver, CURRENT_LOCALS(resolver));
    block->capturedAmount = pop_top_scope(resolver, CURRENT_CAPTURED(resolver));
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

/** Resolves the condition, and the two expressions of a ternary expression. */
static void resolve_ternary(Resolver *resolver, TernaryNode *node) {
    resolve_node(resolver, node->condition);
    resolve_node(resolver, node->trueExpr);
    resolve_node(resolver, node->falseExpr);
}

/** Resolve a subscript set's subscript, subscripted, and value nodes */
static void resolve_assign_subscr(Resolver *resolver, AssignSubscrNode *node) {
    resolve_node(resolver, node->callee);
    resolve_node(resolver, node->subscript);
    resolve_node(resolver, node->value);
}

/** Resolves a subscript get's subscript expression and the thing being subscripted. */
static void resolve_get_subscr(Resolver *resolver, GetSubscrNode *node) {
    resolve_node(resolver, node->callee);
    resolve_node(resolver, node->subscript);
}

/** Resolves all of a list's items. */
static void resolve_list(Resolver *resolver, ListNode *node) {
    resolve_node_array(resolver, &node->items);
}

/** Resolves all keys and values of a map. */
static void resolve_map(Resolver *resolver, MapNode *node) {
    resolve_node_array(resolver, &node->keys);
    resolve_node_array(resolver, &node->values);
}

/** Resolves a call expression's callee, positional arguments, and keyword arguments. */
static void resolve_call(Resolver *resolver, CallNode *node) {
    resolve_node(resolver, node->callee);
    resolve_node_array(resolver, &node->positionalArgs);
    resolve_node(resolver, AS_NODE(node->keywordArgs));
}

/** Resolves the expression wrapped in an expression-statement. */
static void resolve_expr_stmt(Resolver *resolver, const ExprStmtNode *node) {
    resolve_node(resolver, node->expr);
}

/** Resolves a block's array of statements without pushing/popping a scope on top of it. */
static void unscoped_block(Resolver *resolver, BlockNode *node) {
    resolve_node_array(resolver, &node->stmts);
}

/** Resolves a block's array of statements under the depth of a new passed scope, then pops it. */
static void block(Resolver *resolver, BlockNode *node, const ScopeType type) {
    push_scope(resolver, type);
    unscoped_block(resolver, node);
    pop_block_scope(resolver, node, type);
}

/** 
 * Declares a local variable, errors if it's already declared or is const
 * Simply puts a flag on the node which indicates that it's a local declaration.
 */
static void declare_local(Resolver *resolver, DeclareVarNode *node, FuncNode *paramFunc) {
    const Token name = node->name;
    if (find_top_scope_var(*CURRENT_LOCALS(resolver), name, resolver->scopeDepth)) {
        resolution_error(
            resolver, name.pos, "Can't redeclare local name '%.*s'.", name.pos.length, name.lexeme
        );
    }
    Variable *captured = find_variable_array_var(&CURRENT_CLOSURE(resolver)->captured, node->name);
    if (captured && captured->scope == resolver->scopeDepth && captured->isLocalCapture) {
        resolution_error(
            resolver, name.pos, "Can't redeclare local name '%.*s'.", name.pos.length, name.lexeme
        );
    }

    node->resolution.index = CURRENT_LOCALS(resolver)->length;
    node->resolution.scope = VAR_LOCAL;

    Variable declared = create_variable(
        false, node->isConst, false, name, resolver->scopeDepth,
        CURRENT_LOCALS(resolver)->length, resolver->locals.length - 1, node->resolution, paramFunc
    );
    APPEND_DA(&declared.resolutions, &node->resolution);
    APPEND_DA(CURRENT_LOCALS(resolver), declared);
}

/** 
 * Declares a global variable, errors if already declared or is const.
 * Does so by appending the declared variable to the globals closure after setting its resolution.
 */
static void declare_global(Resolver *resolver, DeclareVarNode *node, FuncNode *paramFunc) {
    const Token name = node->name;
    if (find_closure_var(&resolver->globals, name)) {
        resolution_error(
            resolver, name.pos, "Can't redeclare global name '%.*s'.", name.pos.length, name.lexeme
        );
    }

    node->resolution.index = CURRENT_LOCALS(resolver)->length;
    node->resolution.scope = VAR_GLOBAL;

    Variable declared = create_variable(
        false, node->isConst, false, name, resolver->scopeDepth,
        resolver->globals.vars.length, 0, node->resolution, paramFunc
    );
    APPEND_DA(&declared.resolutions, &node->resolution);
    APPEND_DA(&resolver->globals.vars, declared);
}

/**
 * Resolves a new variable's declaration.
 * 
 * We use "name" when erroring out instead of "variable" because variable declaration nodes
 * can include other, non-variable names like declaring the names of functions.
 * 
 * paramFunc is expected to be the function that a parameter belongs to if the variable declared
 * is a parameter of the function, otherwise NULL should be supplied.
 */
static void resolve_declare_var(Resolver *resolver, DeclareVarNode *node, FuncNode *paramFunc) {
    resolve_node(resolver, node->value);
    
    const Token name = node->name;
    Obj *nameAsObj = AS_OBJ(new_string_obj(resolver->program, name.lexeme, name.pos.length));
    if (table_get(&resolver->program->builtIn, nameAsObj)) {
        resolution_error(
            resolver, name.pos, "Can't redeclare built-in name '%.*s'.",
            name.pos.length, name.lexeme
        );
    } else if (resolver->scopeDepth == 0) {
        declare_global(resolver, node, paramFunc);
    } else {
        declare_local(resolver, node, paramFunc);
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
 * Marks a parameter of a function as captured.
 * 
 * Doing that is by adding an index of that variable (relative to BP at runtime)
 * to its original function, and that index goes inside the stack at the slot of the parameter
 * that should be captured.
 */
static void mark_param_captured(ClosedVariables *closure, Variable *parameter) {
    for (i64 i = (i64)closure->vars.length - 1; i >= 0; i--) {
        if (equal_token(closure->vars.data[i].name, parameter->name)) {
            APPEND_DA(&parameter->paramFunc->capturedParams, i);
            return;
        }
    }
    UNREACHABLE_ERROR(); // The parameter we're capturing should always be findable.
}

/** Fixes a captures resolutions/references to a corrected index. */
static void fix_capture_resolution(Variable *toFix, const u32 correctIdx) {
    for (u32 i = 0; i < toFix->resolutions.length; i++) {
        toFix->resolutions.data[i]->index = correctIdx;
    }
    toFix->resolution.index = correctIdx;
}

/** 
 * Appends a capture to a captures array and fixes some indices if needed.
 * 
 * It'll automatically start by assuming the inserted variable is at the end of the list,
 * and if there are any captured variables already in the array with a local index that's
 * after the inserted variable, we subtract the index because it means the variable we're inserting
 * comes before that found variable with a higher index.
 * 
 * We also increment that higher index variable's resolution indices by 1 as it now has another
 * capture behind it at runtime.
 */
static void append_capture(VariableArray *captures, Variable *capturedVar) {
    u32 captureIdx = captures->length;
    for (u32 i = 0; i < captures->length; i++) {
        Variable *current = &captures->data[i];
        if (
            (current->closureIdx == capturedVar->closureIdx && current->index > capturedVar->index)
            || capturedVar->closureIdx < current->closureIdx
        ) {
            captureIdx--; // One less variable behind it.
            fix_capture_resolution(current, current->resolution.index + 1);
        }
    }
    fix_capture_resolution(capturedVar, captureIdx);
    APPEND_DA(captures, *capturedVar);
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
    ClosedVariables *closure = &resolver->locals.data[closureIdx];
    U32Array *localCaptures = &closure->localCaptures;
    LAST_ITEM_DA(localCaptures)++;

    for (u32 i = closureIdx; i < resolver->locals.length; i++) {
        // The closure of the captured itself gets appended a truthy local capture.
        const bool isLocalCapture = i == closureIdx ? true : false;
        Variable closureCapture = create_variable(
            toCapture->isPrivate, toCapture->isConst, isLocalCapture,
            toCapture->name, toCapture->scope, toCapture->index, toCapture->closureIdx,
            create_var_resolution(resolver->locals.data[i].captured.length, VAR_CAPTURED),
            NULL
        );
        append_capture(&resolver->locals.data[i].captured, &closureCapture);
        resolver->locals.data[i].isClosure = true;
    }
    toCapture->resolutions.data[0]->scope = VAR_CAPTURED;
    if (toCapture->paramFunc) {
        mark_param_captured(closure, toCapture);
    }
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
    Variable *variable = find_closure_var(closure, name);
    if (!variable) {
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
static bool try_assign_local(Resolver *resolver, AssignVarNode *node) {
    Variable *local = find_closure_var(CURRENT_CLOSURE(resolver), node->name);
    if (!local) {
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
static bool try_assign_captured(Resolver *resolver, AssignVarNode *node) {
    if (resolver->locals.length < 2) {
        // Doesn't have enough functions to even capture variables.
        return false;
    }
    Variable *local = search_captures(resolver, node->name, resolver->locals.length - 2);
    if (!local) {
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
static bool try_assign_global(Resolver *resolver, AssignVarNode *node) {
    Variable *global = find_closure_var(&resolver->globals, node->name);
    if (!global) {
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
static void resolve_assign_var(Resolver *resolver, AssignVarNode *node) {
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
    if (table_get(&resolver->program->builtIn, nameAsObj)) {
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
static bool try_get_local(Resolver *resolver, GetVarNode *node) {
    Variable *local = find_closure_var(CURRENT_CLOSURE(resolver), node->name);
    if (!local) {
        return false;
    }

    add_variable_reference(local, &node->resolution);
    return true;
}

/** 
 * Tries to find a variable that isn't within the current function nor is global.
 * Returns whether or not it found one and resolved it.
 */
static bool try_get_captured(Resolver *resolver, GetVarNode *node) {
    if (resolver->locals.length < 2) {
        // Doesn't have enough functions to even capture variables.
        return false;
    }
    Variable *local = search_captures(resolver, node->name, resolver->locals.length - 2);
    if (!local) {
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
static bool try_get_global(Resolver *resolver, GetVarNode *node) {
    Variable *global = find_closure_var(&resolver->globals, node->name);
    if (!global) {
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
static bool try_get_built_in(Resolver *resolver, Obj *nameAsObj, GetVarNode *node) {
    if (table_get(&resolver->program->builtIn, nameAsObj)) {
        node->resolution.scope = VAR_BUILT_IN;
        return true;
    }
    return false;
}

/** 
 * Resolves a name that is used to get the value of a variable.
 * 
 * Tries to get locals, then captured variables, then globals, and finally built-ins.
 * Errors if the variable isn't present in any of them.
 */
static void resolve_get_var(Resolver *resolver, GetVarNode *node) {
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

/** Resolves the property being set and the value of the assignment. */
static void resolve_set_property(Resolver *resolver, SetPropertyNode *node) {
    resolve_node(resolver, AS_NODE(node->get));
    resolve_node(resolver, node->value);
}

/** Resolves accessing an object to get/lookup some property out of it. */
static void resolve_get_property(Resolver *resolver, GetPropertyNode *node) {
    resolve_node(resolver, node->originalObj);
}

/** Resolves accessing a property inside a "super" keyword. */
static void resolve_super(Resolver *resolver, GetSuperNode *node) {
    if (resolver->currentCls != CLS_SUBCLASS) {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node)), "Can only use 'super' inside a subclass."
        );
        return;
    }
    // Resolves the instance variable, which'll be used to find the superclass later.
    resolve_get_var(resolver, node->instanceGet);
}

/** Resolves all variables of a multi-variable declaration and the value they're destructuring. */
static void resolve_multi_declare(Resolver *resolver, MultiDeclareNode *node) {
    resolve_node_array(resolver, &node->declarations);
    resolve_node(resolver, node->value);
}

/** Resolves all variables of a multi-assignment expression and the value they're destructuring. */
static void resolve_multi_assign(Resolver *resolver, MultiAssignNode *node) {
    resolve_node_array(resolver, &node->assignments);
    resolve_node(resolver, node->value);
}

/** Resolves the condition and branches of an if-else statement. */
static void resolve_if_else(Resolver *resolver, IfElseNode *node) {
    resolve_node(resolver, node->condition);
    resolve_node(resolver, AS_NODE(node->ifBlock));
    resolve_node(resolver, node->elseBlock);
}

/** Resolves a try-catch's blocks. */
static void resolve_try_catch(Resolver *resolver, TryCatchNode *node) {
    resolve_node(resolver, AS_NODE(node->tryBlock));

    push_scope(resolver, SCOPE_NORMAL);
    if (node->catchVar) {
        resolve_declare_var(resolver, node->catchVar, NULL);
    }
    unscoped_block(resolver, node->catchBlock);
    pop_block_scope(resolver, node->catchBlock, SCOPE_NORMAL);
}

/** Resolves a raise statement's message. */
static void resolve_raise(Resolver *resolver, RaiseNode *node) {
    resolve_node(resolver, node->message);
}

/** Resolves all of a case node's label values and its block. */
static void resolve_case(Resolver *resolver, CaseNode *node) {
    for (u32 i = 0; i < node->labelVals.length; i++) {
        resolve_node(resolver, node->labelVals.data[i]);
    }
    resolve_node(resolver, AS_NODE(node->block));
}

/** 
 * Resolve the matched expression, label expressions, their blocks, and default block if used.
 * 
 * Appends the matched expression implicitly as if it were a variable, because we leave it
 * on the stack at runtime for performance reasons, so we need to make sure it gets popped
 * just like any other variables in all cases (like breaking out of a loop).
 */
static void resolve_match(Resolver *resolver, MatchNode *node) {
    push_scope(resolver, SCOPE_NORMAL);
    resolve_node(resolver, node->matchedExpr);

    const u32 matchIdx = CURRENT_LOCALS(resolver)->length;
    APPEND_DA(
        CURRENT_LOCALS(resolver),
        create_variable(
            false, false, false, create_token("<matched expr>", 0), resolver->scopeDepth,
            matchIdx, resolver->locals.length - 1, create_var_resolution(matchIdx, VAR_LOCAL), NULL
        )
    );
    resolve_node_array(resolver, &node->cases);
    if (node->defaultBlock) {
        resolve_node(resolver, AS_NODE(node->defaultBlock));
    }
    pop_scope(resolver, SCOPE_NORMAL);
}

/** Resolves a while loop's condition and its body statements inside a loop scope. */
static void resolve_while(Resolver *resolver, WhileNode *node) {
    resolve_node(resolver, node->condition);
    block(resolver, node->body, SCOPE_LOOP);
}

/** Resolves a do-while loop, which is like a while, but its condition's resolved after the body. */
static void resolve_do_while(Resolver *resolver, DoWhileNode *node) {
    block(resolver, node->body, SCOPE_LOOP);
    resolve_node(resolver, node->condition);
}

/** 
 * Resolves a for loop statement.
 * 
 * For loops declare 2 scopes: a first, non-loop scope which it puts its loop variable(s)
 * and iterator on, then the 2nd deeper scope, which is a loop scope that encapsulates
 * the loop's inner statements.
 * 
 * The reason for having the loop variable(s) + iterator scope be a different one from
 * the loop itself is so that jump statements (like break or continue) don't accidentally
 * pop the iterator and loop variable(s).
 * For break, it already jumps to the place where the loop variable(s)
 * and iterator pop instructions are, so we don't want to end up popping them multiple times.
 */
static void resolve_for(Resolver *resolver, ForNode *node) {
    push_scope(resolver, SCOPE_NORMAL);
    resolve_node(resolver, node->loopVar);

    const u32 loopVarSpot = CURRENT_LOCALS(resolver)->length;
    APPEND_DA(
        CURRENT_LOCALS(resolver),
        create_variable(
            false, false, false, create_token("<iter>", 0), resolver->scopeDepth,
            loopVarSpot, resolver->locals.length - 1,
            create_var_resolution(loopVarSpot, VAR_LOCAL), NULL
        )
    );
    resolve_node(resolver, node->iterable);
    block(resolver, node->body, SCOPE_LOOP);

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
    for (i64 i = (i64)variables->length - 1; i >= 0; i--) {
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

/** Resolves an enum, which only has its name declaration (as the members are just ints as text). */
static void resolve_enum(Resolver *resolver, const EnumNode *node) {
    resolve_node(resolver, AS_NODE(node->nameDecl));
}

/** 
 * Resolves a return by resolving the returned value.
 * 
 * Errors if the return occurred outside a function, or if we attempted to return an actual
 * value inside an initializer.
 * Also, for initializers they automatically always return the instance.
 */
static void resolve_return(Resolver *resolver, ReturnNode *node) {
    if (resolver->currentFunc == FUNC_NONE) {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node)), "Can't return outside a function scope."
        );
    }
    if (resolver->currentFunc == FUNC_INIT) {
        if (node->value) {
            resolution_error(
                resolver, get_node_pos(AS_NODE(node)), "Can't return a value inside an initializer."
            );
        }
        // Return the instance in initializers.
        node->defaultVal = new_get_var_node(resolver->program, create_token("this", TOKEN_THIS_KW));
    } else {
        node->defaultVal = NULL_NODE(resolver->program);
    }

    resolve_node(resolver, node->value); // Won't do anything if it's NULL.
    resolve_node(resolver, node->defaultVal);
    node->capturedPops = get_local_captures(CURRENT_CLOSURE(resolver));
}

/** Handles the resolutions of a function's mandatory and optional parameteres, and body. */
static void finish_func_resolution(Resolver *resolver, FuncNode *node) {
    for (u32 i = 0; i < node->mandatoryParams.length; i++) {
        resolve_declare_var(resolver, AS_PTR(DeclareVarNode, node->mandatoryParams.data[i]), node);
    }
    for (u32 i = 0; i < node->optionalParams.length; i++) {
        resolve_declare_var(resolver, AS_PTR(DeclareVarNode, node->optionalParams.data[i]), node);
    }
    unscoped_block(resolver, node->body);
    resolve_return(resolver, node->defaultReturn); // Automatically exit the function.
}

/**
 * Resolves a function, which includes its parameters and its body inside a function-type scope.
 * 
 * Manually appends the function's name a second time to the recently created locals of itself
 * in order to allow recursion by accessinig index 0 inside BP at runtime
 * (as BP starts on the callee).
 * 
 * If the function is some class's method, then the keyword name "this" is added instead of
 * the function itself a second time, so that the instance can be accessed instead.
 */
static void resolve_func(Resolver *resolver, FuncNode *node, const FuncType type) {
    const FuncType previous = resolver->currentFunc;
    resolver->currentFunc = type;

    resolve_declare_var(resolver, node->nameDecl, NULL);
    push_scope(resolver, SCOPE_FUNC);

    const Token usedName = type == FUNC_INIT || type == FUNC_METHOD ?
        create_token("this", TOKEN_THIS_KW) : node->nameDecl->name;
    Variable declared = create_variable(
        false, node->nameDecl->isConst, false, usedName, resolver->scopeDepth,
        CURRENT_LOCALS(resolver)->length, resolver->locals.length - 1,
        node->nameDecl->resolution, NULL
    );
    APPEND_DA(CURRENT_LOCALS(resolver), declared);
    finish_func_resolution(resolver, node);
    node->isClosure = CURRENT_CLOSURE(resolver)->isClosure;

    pop_block_scope(resolver, node->body, SCOPE_FUNC);
    resolver->currentFunc = previous;
}

/** Checks if a class's inheritance name is itself. Errors out if so. */
static void check_inherits_self(Resolver *resolver, ClassNode *node) {
    const Token clsName = node->nameDecl->name;
    const Token superclassName = node->superclass->name;
    if (equal_token(clsName, superclassName)) {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node->superclass)), "Class can't inherit itself."
        );
    }
}

/** 
 * Resolves a class's variable declaration, initializer (if there's one), and all methods.
 * 
 * First pushes the class scope so that everything inside it is automatically treated
 * as a local/capture (except the name of the class itself).
 * Then, for each method pushes another temporary scope which ensures the method name is deleted
 * after its compilation as its no longer needed, and would only interfere with the resolution of
 * the other methods.
 */
static void resolve_class(Resolver *resolver, ClassNode *node) {
    resolve_declare_var(resolver, node->nameDecl, NULL);

    const ClsType previous = resolver->currentCls;
    resolver->currentCls = CLS_CLASS;
    if (node->superclass) {
        resolver->currentCls = CLS_SUBCLASS;
        check_inherits_self(resolver, node);
        resolve_get_var(resolver, node->superclass);
    }
    
    push_scope(resolver, SCOPE_NORMAL);
    if (node->init) {
        push_scope(resolver, SCOPE_NORMAL);
        resolve_func(resolver, node->init, FUNC_INIT);
        pop_scope(resolver, SCOPE_NORMAL);
    }
    // TODO: doesn't this prevent catching an error where 2 methods have the same name?
    for (u32 i = 0; i < node->methods.length; i++) {
        push_scope(resolver, SCOPE_NORMAL);
        resolve_func(resolver, AS_PTR(FuncNode, node->methods.data[i]), FUNC_METHOD);
        pop_scope(resolver, SCOPE_NORMAL);
    }
    for (u32 i = 0; i < node->abstractMethods.length; i++) {
        push_scope(resolver, SCOPE_NORMAL);
        resolve_declare_var(resolver, AS_PTR(DeclareVarNode, node->abstractMethods.data[i]), NULL);
        pop_scope(resolver, SCOPE_NORMAL);
    }
    pop_scope(resolver, SCOPE_NORMAL);
    resolver->currentCls = previous;
}

/** Resolves the passed nodes and anything inside it that requires resolution. */
static void resolve_node(Resolver *resolver, Node *node) {
    if (node == NULL) {
        return;
    }

    switch (node->type) {
    case AST_STRING: resolve_string(resolver, AS_PTR(StringNode, node)); break;
    case AST_UNARY: resolve_unary(resolver, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: resolve_binary(resolver, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: resolve_parentheses(resolver, AS_PTR(ParenthesesNode, node)); break;
    case AST_RANGE: resolve_range(resolver, AS_PTR(RangeNode, node)); break;
    case AST_ASSIGN_SUBSCR: resolve_assign_subscr(resolver, AS_PTR(AssignSubscrNode, node)); break;
    case AST_GET_SUBSCR: resolve_get_subscr(resolver, AS_PTR(GetSubscrNode, node)); break;
    case AST_TERNARY: resolve_ternary(resolver, AS_PTR(TernaryNode, node)); break;
    case AST_LIST: resolve_list(resolver, AS_PTR(ListNode, node)); break;
    case AST_MAP: resolve_map(resolver, AS_PTR(MapNode, node)); break;
    case AST_CALL: resolve_call(resolver, AS_PTR(CallNode, node)); break;
    case AST_EXPR_STMT: resolve_expr_stmt(resolver, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: block(resolver, AS_PTR(BlockNode, node), SCOPE_NORMAL); break;
    case AST_DECLARE_VAR: resolve_declare_var(resolver, AS_PTR(DeclareVarNode, node), NULL); break;
    case AST_ASSIGN_VAR: resolve_assign_var(resolver, AS_PTR(AssignVarNode, node)); break;
    case AST_GET_VAR: resolve_get_var(resolver, AS_PTR(GetVarNode, node)); break;
    case AST_SET_PROPERTY: resolve_set_property(resolver, AS_PTR(SetPropertyNode, node)); break;
    case AST_GET_PROPERTY: resolve_get_property(resolver, AS_PTR(GetPropertyNode, node)); break;
    case AST_GET_SUPER: resolve_super(resolver, AS_PTR(GetSuperNode, node)); break;
    case AST_MULTI_DECLARE: resolve_multi_declare(resolver, AS_PTR(MultiDeclareNode, node)); break;
    case AST_MULTI_ASSIGN: resolve_multi_assign(resolver, AS_PTR(MultiAssignNode, node)); break;
    case AST_IF_ELSE: resolve_if_else(resolver, AS_PTR(IfElseNode, node)); break;
    case AST_TRY_CATCH: resolve_try_catch(resolver, AS_PTR(TryCatchNode, node)); break;
    case AST_RAISE: resolve_raise(resolver, AS_PTR(RaiseNode, node)); break;
    case AST_CASE: resolve_case(resolver, AS_PTR(CaseNode, node)); break;
    case AST_MATCH: resolve_match(resolver, AS_PTR(MatchNode, node)); break;
    case AST_WHILE: resolve_while(resolver, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: resolve_do_while(resolver, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: resolve_for(resolver, AS_PTR(ForNode, node)); break;
    case AST_LOOP_CONTROL: resolve_loop_control(resolver, AS_PTR(LoopControlNode, node)); break;
    case AST_ENUM: resolve_enum(resolver, AS_PTR(EnumNode, node)); break;
    case AST_RETURN: resolve_return(resolver, AS_PTR(ReturnNode, node)); break;
    case AST_FUNC: resolve_func(resolver, AS_PTR(FuncNode, node), FUNC_FUNCTION); break;
    case AST_CLASS: resolve_class(resolver, AS_PTR(ClassNode, node)); break;

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
