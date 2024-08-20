#include "resolver.h"

/** Returns the outermost closure of scopes. */
#define CURRENT_CLOSURE(resolver) (&(resolver)->closures.data[(resolver)->closures.length - 1])

/** A boolean of whether or not we have created any locals in the current closure scope. */
#define HAS_LOCALS(resolver) (CURRENT_CLOSURE(resolver)->length > 0)

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
        .program = program, .ast = ast, .globals = CREATE_DA(), .closures = CREATE_DA(),
        .scopeDepth = 0, .loopScopes = CREATE_DA()
    };
    // Add the default, permanent closure for locals not covered by functions (like global loops).
    APPEND_DA(&resolver.closures, (ClosedVariables)CREATE_DA());
    return resolver;
}

/** Frees all the owned and allocated memory of the resolver. */
void free_resolver(Resolver *resolver) {
    for (u32 i = 0; i < resolver->closures.length; i++) {
        FREE_DA(&resolver->closures.data[i]);
    }
    FREE_DA(&resolver->closures);
    FREE_DA(&resolver->globals);
    FREE_DA(&resolver->loopScopes);
}

/** Reports a resolution error on a specific position. */
void resolution_error(Resolver *resolver, const SourcePosition pos, const char *format, ...) {
    va_list args;
    va_start(args, format);
    zmx_user_error(resolver->program, pos, "Resolution error", format, &args);
    va_end(args);
}

/** Returns a variable, which has its name in a token along some extra resolution information. */
static Variable create_variable(
    const bool isPrivate, const bool isConst, const Token name, const u32 scope
) {
    Variable var = {.isPrivate = isPrivate, .isConst = isConst, .name = name, .scope = scope};
    return var;
}

/** Returns the index of the name in the array of variables. Returns -1 if not present. */
static i64 get_closed_var_index(ClosedVariables variables, Token name) {
    for (i64 i = (i64)variables.length - 1; i >= 0; i--) {
        if (equal_token(variables.data[i].name, name)) {
            return i;
        }
    }
    return -1;
}

/** 
 * Returns the index of the passed variable name searching only in the top scope of a closure.
 * 
 * Returns -1 if the variable's not present at the highest scope of the closure, even if it's on
 * other scopes.
 */
static i64 get_top_scope_var_index(ClosedVariables variables, Token name, const u32 scope) {
    for (i64 i = (i64)variables.length - 1; i >= 0; i--) {
        if (variables.data[i].scope != scope) {
            break; // Fall into not found.
        }
        if (equal_token(variables.data[i].name, name)) {
            return i;
        }
    }
    return -1;
}

/** Adds a scope that's one layer deeper (usually from entering a new block scope). */
static void push_scope(Resolver *resolver, const ScopeType type) {
    resolver->scopeDepth++;
    if (type == SCOPE_LOOP) {
        APPEND_DA(&resolver->loopScopes, resolver->scopeDepth);
    } else if (type == SCOPE_FUNC) {
        APPEND_DA(&resolver->closures, (ClosedVariables)CREATE_DA());
    }
}

/** Collapses the outermost scope and returns how many variables were popped. */
static u32 pop_scope(Resolver *resolver, ScopeType type) {
    ClosedVariables *closure = CURRENT_CLOSURE(resolver);
    u32 poppedAmount = 0;
    while (
        closure->length > 0 && closure->data[closure->length - 1].scope == resolver->scopeDepth
    ) {
        DROP_DA(closure);
        poppedAmount++;
    }

    resolver->scopeDepth--;
    if (type == SCOPE_LOOP) {
        DROP_DA(&resolver->loopScopes);
    } else if (type == SCOPE_FUNC) {
        FREE_DA(closure);
        DROP_DA(&resolver->closures);
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
static void declare_local(Resolver *resolver, VarDeclNode *node, const Variable declared) {
    const Token name = declared.name;
    if (get_top_scope_var_index(*CURRENT_CLOSURE(resolver), name, resolver->scopeDepth) != -1) {
        resolution_error(
            resolver, name.pos, "Can't redeclare local name '%.*s'.",
            name.pos.length, name.lexeme
        );
    }
    APPEND_DA(CURRENT_CLOSURE(resolver), declared);
    node->varScope = NAME_LOCAL;
}

/** 
 * Declares a global variable, errors if already declared or is const.
 * A flag that indicates the variable is global on the node is enough for the compiler.
 */
static void declare_global(Resolver *resolver, VarDeclNode *node, const Variable declared) {
    const Token name = declared.name;
    if (get_closed_var_index(resolver->globals, name) != -1) {
        resolution_error(
            resolver, name.pos, "Can't redeclare global name '%.*s'.",
            name.pos.length, name.lexeme
        );
    }
    APPEND_DA(&(resolver)->globals, declared);
    node->varScope = NAME_GLOBAL;
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
    const Variable declared = create_variable(false, node->isConst, name, resolver->scopeDepth);
    Obj *nameAsObj = AS_OBJ(string_obj_from_len(resolver->program, name.lexeme, name.pos.length));
    if (table_get(&resolver->program->builtIn, nameAsObj) != NULL) {
        resolution_error(
            resolver, name.pos, "Can't redeclare built-in name '%.*s'.",
            name.pos.length, name.lexeme
        );
    } else if (resolver->scopeDepth == 0) {
        declare_global(resolver, node, declared);
    } else {
        declare_local(resolver, node, declared);
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
 * Attempts to assign a local if one exists.
 * 
 * Returns whether or not a local was found and assigned to the node.
 * Assiging is done by changing the variable scope of the node as well as putting an index
 * that denotes where on the stack the assignment should occur.
 */
static bool try_assign_local(Resolver *resolver, VarAssignNode *node) {
    const i64 localIdx = get_closed_var_index(*CURRENT_CLOSURE(resolver), node->name);
    if (localIdx == -1) {
        return false;
    }

    ASSERT(localIdx >= 0, "Expected positive local index, got %" PRId64 " instead.", localIdx);
    if (CURRENT_CLOSURE(resolver)->data[localIdx].isConst) {
        const_assign_error(resolver, AS_NODE(node), node->name);
    }
    node->varScope = NAME_LOCAL;
    node->assignIndex = localIdx;
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
 * When assigning globals, we only change the node's variable scope, but not the index since
 * the VM will just use a table for them instead of indexing some array.
 */
static bool try_assign_global(Resolver *resolver, VarAssignNode *node) {
    i64 globalIdx = get_closed_var_index(resolver->globals, node->name);
    if (globalIdx == -1) {
        return false;
    }

    ASSERT(globalIdx >= 0, "Expected positive global index, got %" PRId64 " instead.", globalIdx);
    if (resolver->globals.data[globalIdx].isConst) {
        const_assign_error(resolver, AS_NODE(node), node->name);
    }
    node->varScope = NAME_GLOBAL;
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
    Obj *nameAsObj = AS_OBJ(string_obj_from_len(resolver->program, name.lexeme, name.pos.length));
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
 * Resolving locals includes setting the node's scope as local, and appending the index where
 * it's expected to be on the stack, so that the compiler can simply put that number for the VM.
 */
static bool try_get_local(Resolver *resolver, VarGetNode *node) {
    const i64 localIdx = get_closed_var_index(*CURRENT_CLOSURE(resolver), node->name);
    if (localIdx == -1) {
        return false;   
    }
    node->varScope = NAME_LOCAL;
    node->getIndex = localIdx;
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
 * Returns whether or not it found one and resolved it, or if there wasn't any matching globals.
 * Just like assignments, we use hash tables in the at runtime for globals, so we simply set the
 * variable get node's scope to a global and letting the compiler do the rest.
 */
static bool try_get_global(Resolver *resolver, VarGetNode *node) {
    if (get_closed_var_index(resolver->globals, node->name) == -1) {
        return false;
    }
    node->varScope = NAME_GLOBAL;
    return true;
}

/** 
 * Tries to resolve a variable get if it's referencing a built-in name.
 * 
 * Returns whether or not the resolution was successful and the name in the node was a built-in.
 * If it's built-in, we simply just set the variable scope to denote that, then the compiler/VM
 * will find it in the table of built-ins.
 */
static bool try_get_built_in(Resolver *resolver, Obj *nameAsObj, VarGetNode *node) {
    if (table_get(&resolver->program->builtIn, nameAsObj) == NULL) {
        return false;
    }
    node->varScope = NAME_BUILT_IN;
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
    Obj *nameAsObj = AS_OBJ(string_obj_from_len(resolver->program, name.lexeme, name.pos.length));
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

    APPEND_DA(
        CURRENT_CLOSURE(resolver),
        create_variable(false, false, node->loopVar, resolver->scopeDepth)
    );
    APPEND_DA(
        CURRENT_CLOSURE(resolver),
        create_variable(false, false, create_token("<iter>", 0), resolver->scopeDepth)
    );
    resolve_node(resolver, node->iterable);
    scoped_block(resolver, node->body, SCOPE_LOOP);

    pop_scope(resolver, SCOPE_NORMAL);
}

/** Returns the amount of variables inside the outermost loop. */
static u32 loop_vars_amount(Resolver *resolver) {
    ASSERT(resolver->loopScopes.length > 0, "Tried to count loop variables, but not inside one.");
    if (!HAS_LOCALS(resolver)) {
        return 0;
    }

    u32 loopVarsAmount = 0;
    const u32 deepestLoop = resolver->loopScopes.data[resolver->loopScopes.length - 1];
    for (i64 i = CURRENT_CLOSURE(resolver)->length - 1; i >= 0; i--) {
        if (CURRENT_CLOSURE(resolver)->data[i].scope < deepestLoop) {
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
 * Manually appends the function's name again to the recently created locals of itself in order
 * to allow recursion by accessinig index 0 inside BP at runtime (as BP starts on the callee).
 */
static void resolve_func(Resolver *resolver, FuncNode *node) {
    resolve_var_decl(resolver, node->nameDecl);

    push_scope(resolver, SCOPE_FUNC);
    const Variable funcName = create_variable(
        false, node->nameDecl->isConst, node->nameDecl->name, resolver->scopeDepth
    );
    APPEND_DA(CURRENT_CLOSURE(resolver), funcName);
    for (u32 i = 0; i < node->params.length; i++) {
        // TODO: make this also check for other stuff potentially when optional params are added.
        ASSERT(node->params.data[i]->type == AST_LITERAL, "Parameter is not a variable literal.");

        const Token varName = AS_PTR(LiteralNode, node->params.data[i])->value;
        const Variable parameter = create_variable(false, false, varName, resolver->scopeDepth);
        APPEND_DA(CURRENT_CLOSURE(resolver), parameter);
    }
    block(resolver, node->body);
    node->body->varsAmount = pop_scope(resolver, SCOPE_FUNC);
}

/** Resolves a return by resolving the returned value and erroring if we're not inside a func. */
static void resolve_return(Resolver *resolver, ReturnNode *node) {
    if (resolver->closures.length < 2) {
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
