#include "resolver.h"

/** The outermost closure of local variables. */
#define CURRENT_LOCALS(resolver) (&(resolver)->locals.data[(resolver)->locals.length - 1])

/** A boolean of whether or not we have created any variables in the current locals closure. */
#define HAS_VARIABLES(resolver) (CURRENT_LOCALS(resolver)->length > 0)

/** Holds some type of scope that is usually created by brace blocks.*/
typedef enum {
    SCOPE_NORMAL, /** A non-special block scope, which is also encapsulated by all other types. */
    SCOPE_LOOP, /** Indicates that the scope was created by some loop. */
    SCOPE_FUNC /** A scope made for any kind of function (normal, method, initializer, etc.). */
} ScopeType;

static void resolve_node(Resolver *resolver, Node *node);
static void resolve_node_array(Resolver *resolver, NodeArray *nodes);

/** Returns a starting resolver for the passed AST. */
Resolver create_resolver(ZmxProgram *program, NodeArray ast) {
    Resolver resolver = {
        .program = program, .ast = ast, .globals = CREATE_DA(), .locals = CREATE_DA(),
        .scopeDepth = 0, .loopScopes = CREATE_DA()
    };
    // Add the permanent closure for locals not covered by functions (like vars in global loops).
    APPEND_DA(&resolver.locals, (ClosedVariables)CREATE_DA());
    return resolver;
}

/** Returns a variable, which has its name in a token along some extra resolution information. */
static Variable create_variable(
    const bool isPrivate, const bool isConst, const Token name, const u32 scope,
    const VarResolution resolution
) {
    Variable var = {
        .isPrivate = isPrivate, .isConst = isConst, .name = name, .scope = scope,
        .currentResolution = resolution, .resolutions = CREATE_DA()
    };
    return var;
}

/** Frees all the variables and their contents in an array of resolution-phase variables. */
void free_variables(ClosedVariables *variables) {
    for (u32 i = 0; i < variables->length; i++) {
        FREE_DA(&variables->data[i].resolutions);
    }
    FREE_DA(variables);
}

/** Frees all the owned and allocated memory of the resolver. */
void free_resolver(Resolver *resolver) {
    free_variables(&resolver->globals);
    for (u32 i = 0; i < resolver->locals.length; i++) {
        free_variables(&resolver->locals.data[i]);
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
void resolution_error(Resolver *resolver, const SourcePosition pos, const char *format, ...) {
    va_list args;
    va_start(args, format);
    zmx_user_error(resolver->program, pos, "Resolution error", format, &args);
    va_end(args);
}

/** 
 * Returns a variable's address in the passed closure whose name is the same as the passed token.
 * Returns NULL by default if the variable doesn't exist in the closure.
 */
static Variable *get_closure_var(ClosedVariables *variables, Token name) {
    for (i64 i = (i64)variables->length - 1; i >= 0; i--) {
        if (equal_token(variables->data[i].name, name)) {
            return &variables->data[i];
        }
    }
    return NULL;
}

/** 
 * Returns the variable in the passed closure searching only in the top scope of a closure.
 * Returns NULL if the variable doesn't exist within the top scope of the passed closure.
 */
static Variable *get_top_scope_var(ClosedVariables variables, Token name, const u32 scope) {
    for (i64 i = (i64)variables.length - 1; i >= 0; i--) {
        if (variables.data[i].scope != scope) {
            break; // Fall into not found.
        }
        if (equal_token(variables.data[i].name, name)) {
            return &variables.data[i];
        }
    }
    return NULL;
}

/** Adds a scope that's one layer deeper (usually from entering a new block scope). */
static void push_scope(Resolver *resolver, const ScopeType type) {
    resolver->scopeDepth++;
    if (type == SCOPE_LOOP) {
        APPEND_DA(&resolver->loopScopes, resolver->scopeDepth);
    } else if (type == SCOPE_FUNC) {
        APPEND_DA(&resolver->locals, (ClosedVariables)CREATE_DA());
    }
}

/** Collapses the outermost scope and returns how many variables were popped. */
static u32 pop_scope(Resolver *resolver, ScopeType type) {
    ClosedVariables *locals = CURRENT_LOCALS(resolver);
    u32 poppedAmount = 0;
    while (
        locals->length > 0 && locals->data[locals->length - 1].scope == resolver->scopeDepth
    ) {
        FREE_DA(&locals->data[locals->length - 1].resolutions);
        DROP_DA(locals);
        poppedAmount++;
    }

    resolver->scopeDepth--;
    if (type == SCOPE_LOOP) {
        DROP_DA(&resolver->loopScopes);
    } else if (type == SCOPE_FUNC) {
        ASSERT(resolver->locals.length > 1, "Attempted to pop the permanent top-level closure.");
        free_variables(locals);
        DROP_DA(&resolver->locals);
    }
    return poppedAmount;
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
    node->varsAmount = pop_scope(resolver, type);
}

/** 
 * Declares a local variable, errors if it's already declared or is const
 * Simply puts a flag on the node which indicates that it's a local declaration.
 */
static void declare_local(Resolver *resolver, VarDeclNode *node) {
    const Token name = node->name;
    if (get_top_scope_var(*CURRENT_LOCALS(resolver), name, resolver->scopeDepth) != NULL) {
        resolution_error(
            resolver, name.pos, "Can't redeclare local name '%.*s'.",
            name.pos.length, name.lexeme
        );
    }
    node->resolution.index = CURRENT_LOCALS(resolver)->length;
    node->resolution.scope = VAR_LOCAL;

    Variable declared = create_variable(
        false, node->isConst, name, resolver->scopeDepth, node->resolution
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
    if (get_closure_var(&resolver->globals, name) != NULL) {
        resolution_error(
            resolver, name.pos, "Can't redeclare global name '%.*s'.",
            name.pos.length, name.lexeme
        );
    }

    node->resolution.index = CURRENT_LOCALS(resolver)->length;
    node->resolution.scope = VAR_GLOBAL;

    Variable declared = create_variable(
        false, node->isConst, name, resolver->scopeDepth, node->resolution
    );
    APPEND_DA(&declared.resolutions, &node->resolution);
    APPEND_DA(&resolver->globals, declared);
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
    *resolution = variable->currentResolution;
    APPEND_DA(&variable->resolutions, resolution);
}

/**
 * Attempts to assign a local if one exists.
 * 
 * Returns whether or not a local was found and assigned to the node.
 * When resolving assignments, the node gets its resolution from the found declared variable,
 * then it (the node resolution) is put inside the variable's array of resolutions to reference when
 * needing to modify the resolutions of all references to a variable.
 */
static bool try_assign_local(Resolver *resolver, VarAssignNode *node) {
    Variable *local = get_closure_var(CURRENT_LOCALS(resolver), node->name);
    if (local == NULL) {
        return false;
    }

    if (local->isConst) {
        const_assign_error(resolver, AS_NODE(node), node->name);
    }
    add_variable_reference(local, &node->resolution);
    return true;
}

/** TODO: add assigning closures and their docs. Don't forget const_assign_error() if it's const. */
static bool try_assign_nonlocal(Resolver *resolver, const VarAssignNode *node) {
    UNUSED_PARAMETER(resolver);
    UNUSED_PARAMETER(node);
    return false;
}

/**
 * Attempts to assign a global variable if one exists.
 * 
 * Returns whether or not a global was found and assigned to the node.
 * We give the assign node the resolution information
 * of the declared global variable, then put the node's own resolution address on the array
 * of resolutions so it can be modified later if needed.
 */
static bool try_assign_global(Resolver *resolver, VarAssignNode *node) {
    Variable *global = get_closure_var(&resolver->globals, node->name);
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
 * Allows global, local and closure variable assignments. Errors if the assigned variable
 * doesn't exist or is a built-in.
 * TODO: add more explanation for multi-assignments and closures when they're added.
 */
static void resolve_var_assign(Resolver *resolver, VarAssignNode *node) {
    resolve_node(resolver, node->value);
    const Token name = node->name;

    if (resolver->scopeDepth > 0) {
        if (try_assign_local(resolver, node) || try_assign_nonlocal(resolver, node)) {
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
 * Attempts to find and resolve a variable that is local to the current function's closure.
 * 
 * Returns whether or not it found one and resolved it.
 * Resolving is done by adding the information of resolution on the node,
 * then adding a reference to the node resolution in the found variable
 * so that it can modify it later if needed.
 */
static bool try_get_local(Resolver *resolver, VarGetNode *node) {
    Variable *local = get_closure_var(CURRENT_LOCALS(resolver), node->name);
    if (local == NULL) {
        return false;
    }

    add_variable_reference(local, &node->resolution);
    return true;
}

/** TODO: add getting variable outside current closure here. */
static bool try_get_nonlocal(Resolver *resolver, VarGetNode *node) {
    UNUSED_PARAMETER(resolver);
    UNUSED_PARAMETER(node);
    return false;
}

/** 
 * Tries to resolve a variable in the global scope if there is one.
 * 
 * Adds a reference to the global if one is found, which is a function that sets a node's resolution
 * and stores it inside the given variable in case it needs to be modified later.
 * Returns true if that happened, otherwise false if the global isn't found to begin with.
 */
static bool try_get_global(Resolver *resolver, VarGetNode *node) {
    Variable *global = get_closure_var(&resolver->globals, node->name);
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
 * Attempts to get locals, then upvalues, then globals, and finally built-ins.
 * Errors if the variable isn't present in any of them.
 */
static void resolve_var_get(Resolver *resolver, VarGetNode *node) {
    const Token name = node->name;
    if (resolver->scopeDepth > 0) {
        if (try_get_local(resolver, node) || try_get_nonlocal(resolver, node)) {
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
    APPEND_DA(
        CURRENT_LOCALS(resolver),
        create_variable(
            false, false, node->loopVar,
            resolver->scopeDepth, create_var_resolution(loopVarSpot, VAR_LOCAL)
        )
    );
    APPEND_DA(
        CURRENT_LOCALS(resolver),
        create_variable(
            false, false, create_token("<iter>", 0), resolver->scopeDepth,
            create_var_resolution(loopVarSpot + 1, VAR_LOCAL)
        )
    );
    resolve_node(resolver, node->iterable);
    scoped_block(resolver, node->body, SCOPE_LOOP);

    pop_scope(resolver, SCOPE_NORMAL);
}

/** Returns the amount of variables inside the outermost loop. */
static u32 loop_vars_amount(Resolver *resolver) {
    ASSERT(resolver->loopScopes.length > 0, "Tried to count loop variables, but not inside one.");
    if (!HAS_VARIABLES(resolver)) {
        return 0;
    }

    u32 loopVarsAmount = 0;
    const u32 deepestLoop = resolver->loopScopes.data[resolver->loopScopes.length - 1];
    for (i64 i = CURRENT_LOCALS(resolver)->length - 1; i >= 0; i--) {
        if (CURRENT_LOCALS(resolver)->data[i].scope < deepestLoop) {
            break; // Below loop, done.
        }
        loopVarsAmount++;
    }
    return loopVarsAmount;
}

/** 
 * Resolves a loop control statement (like break or continue).
 * 
 * Puts the amount of pops that'll be emitted from skipping the loop iteration inside the node,
 * which is the number of variables declared on the loop before encountering the loop control
 * statement.
 */
static void resolve_loop_control(Resolver *resolver, LoopControlNode *node) {
    if (resolver->loopScopes.length == 0) {
        const char *stringRepr = node->keyword == TOKEN_BREAK_KW ? "break" : "continue";
        resolution_error(resolver, node->pos, "Can only use '%s' inside a loop.", stringRepr);
        return;
    }

    node->loopVarsAmount = loop_vars_amount(resolver);
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
        false, node->nameDecl->isConst, node->nameDecl->name, resolver->scopeDepth,
        node->nameDecl->resolution
    );
    APPEND_DA(CURRENT_LOCALS(resolver), declared);

    for (u32 i = 0; i < node->params.length; i++) {
        // TODO: make this also check for other stuff potentially when optional params are added.
        ASSERT(node->params.data[i]->type == AST_LITERAL, "Parameter is not a variable literal.");

        // +1 on the variable resolution created because the 0th spot is occupied by the func name.
        const Token varName = AS_PTR(LiteralNode, node->params.data[i])->value;
        const Variable parameter = create_variable(
            false, false, varName, resolver->scopeDepth, create_var_resolution(i + 1, VAR_LOCAL)
        );
        APPEND_DA(CURRENT_LOCALS(resolver), parameter);
    }
    block(resolver, node->body);
    
    node->body->varsAmount = pop_scope(resolver, SCOPE_FUNC);
}

/** Resolves a return by resolving the returned value and erroring if we're not inside a func. */
static void resolve_return(Resolver *resolver, ReturnNode *node) {
    if (resolver->locals.length < 2) {
        resolution_error(
            resolver, get_node_pos(AS_NODE(node)), "Can't return outside a function scope."
        );
    }
    resolve_node(resolver, node->returnValue);
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
