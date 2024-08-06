#include <stdarg.h>

#include "compiler.h"
#include "emitter.h"
#include "lexer.h"
#include "parser.h"

#if DEBUG_BYTECODE
    #include "debug_bytecode.h"
#endif

/** Returns the outermost closure of scopes. */
#define CURRENT_LOCALS(compiler) (&(compiler)->locals.data[(compiler)->locals.length - 1])

/** Resolves into a boolean of whether or not we have created any locals. */
#define HAS_LOCALS(compiler) (CURRENT_LOCALS(compiler)->length > 0)

/** The position of the previous opcode. Defaults to (0, 0, 0) if there aren't any. */
#define PREVIOUS_OPCODE_POS(compiler) \
    (compiler)->func->positions.length == 0 \
        ? create_src_pos(0, 0, 0) \
        : (compiler)->func->positions.data[(compiler)->func->positions.length - 1]

/** Holds some type of scope that is usually created by {} blocks.*/
typedef enum {
    SCOPE_NORMAL, /** A non-special block scope, which is also encapsulated by all other types. */
    SCOPE_LOOP /** Indicates that the scope was created by some loop. */
} ScopeType;

static void compile_node(Compiler *compiler, const Node *node);

/** Reports a compilation error on a specific node's main token. */
static void compiler_error(Compiler *compiler, const Node *erroredNode, const char *format, ...) {
    va_list args;
    va_start(args, format);
    zmx_user_error(compiler->program, get_node_pos(erroredNode), "Compiler error", format, &args);
    va_end(args);
}

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast, bool trackPositions) {
    program->gc.protectNewObjs = true;
    Compiler compiler = {
        .trackPositions = trackPositions, .program = program, .ast = ast,
        .globals = CREATE_DA(), .locals = CREATE_DA(), .jumps = CREATE_DA(),
        .func = new_func_obj(program, new_string_obj(program, "<main>"), -1),
        .breaks = CREATE_DA(), .continues = CREATE_DA(), .loopScopes = CREATE_DA(), .scopeDepth = 0
    };
    // Add the default, permanent closure for locals not covered by functions (like global ifs).
    APPEND_DA(&compiler.locals, (ClosedVariables)CREATE_DA());
    program->gc.protectNewObjs = false;
    return compiler;
}

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler) {
    for (u32 i = 0; i < compiler->locals.length; i++) {
        FREE_DA(&compiler->locals.data[i]);
    }
    FREE_DA(&compiler->locals);
    FREE_DA(&compiler->globals);
    FREE_DA(&compiler->jumps);
    FREE_DA(&compiler->continues);
    FREE_DA(&compiler->breaks);
    FREE_DA(&compiler->loopScopes);
}

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
 * Returns the index of the passed variable name searching only in the top scope of the closure.
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

/** Compiles a literal value which is by adding its obj form in the constants pool. */
static void compile_literal(Compiler *compiler, const LiteralNode *node) {
    Token value = node->value;
    Obj *literalAsObj;
    switch (value.type) {
    case TOKEN_INT_LIT:
        literalAsObj = AS_OBJ(new_int_obj(compiler->program, value.intVal));
        break;
    case TOKEN_FLOAT_LIT:
        literalAsObj = AS_OBJ(new_float_obj(compiler->program, value.floatVal));
        break;
    case TOKEN_STRING_LIT:
        literalAsObj = AS_OBJ(new_string_obj(compiler->program, value.stringVal.text));
        break;
    default:
        UNREACHABLE_ERROR();
    }
    emit_const(compiler, OP_LOAD_CONST, literalAsObj, node->value.pos);
}

/** 
 * Compiles the node which holds all information of a string (including interpolation).
 * 
 * It alternates between emitting a string const and compiling an interpolated expression.
 * When it's an interpolated expression, an instruction to convert the expression's result to
 * a string is also emitted.
 * 
 * At the end, we emit an instruction to build a "finished" string from all the string literals
 * and converted expressions that are on the top of the stack.
 */
static void compile_string(Compiler *compiler, const StringNode *node) {
    // Figure out whether the first node is a string or interpolation.
    Node *first = node->exprs.data[0];
    bool nextIsString = first->type == AST_LITERAL
            && AS_PTR(LiteralNode, first)->value.type == TOKEN_STRING_LIT;

    for (u32 i = 0; i < node->exprs.length; i++) {
        compile_node(compiler, node->exprs.data[i]);
        if (!nextIsString) {
            // Converts interpolated expression into a string.
            emit_number(compiler, OP_AS, TYPE_STRING, get_node_pos(node->exprs.data[i]));
        }
        nextIsString = !nextIsString;
    }
    // Only finish (concatenate) if there's more than one string as optimization.
    if (node->exprs.length > 1) {
        emit_number(compiler, OP_FINISH_STRING, node->exprs.length, get_node_pos(AS_NODE(node)));
    }
}

/** 
 * Emits the pop instructions for all top loop scope's variables.
 * 
 * Only emits their pop instructions, it doesn't stop keeping track of those variables
 * in the compiler.
 */
static void emit_loop_pops(Compiler *compiler) {
    ASSERT(compiler->loopScopes.length > 0, "Can't pop loop locals outside of loop.");
    if (!HAS_LOCALS(compiler)) {
        return; // No locals to pop.
    }

    const u32 deepestLoop = compiler->loopScopes.data[compiler->loopScopes.length - 1];
    u32 poppedAmount = 0;
    for (i64 i = CURRENT_LOCALS(compiler)->length - 1; i >= 0; i--) {
        if (CURRENT_LOCALS(compiler)->data[i].scope < deepestLoop) {
            break; // Below loop, done.
        }
        poppedAmount++;
    }
    emit_pops(compiler, poppedAmount, PREVIOUS_OPCODE_POS(compiler));
}

/** 
 * Handles and compiles a loop control statement (like continue or break).
 * 
 * It initially emits the loop control jumps as pure zeroes in the bytecode,
 * so that they can be resolved later.
 */
static void loop_control(
    Compiler *compiler, const KeywordNode *node, U32Array *loopControlArray, const char *stringRepr
) {
    if (compiler->loopScopes.length == 0) {
        compiler_error(compiler, AS_NODE(node), "Can only use \"%s\" inside a loop.", stringRepr);
    }
    emit_loop_pops(compiler);
    u32 controlSpot = emit_unpatched_jump(compiler, 0, node->pos);
    APPEND_DA(loopControlArray, controlSpot);
}

/** Compiles a keyword node, which is one that holds a bare keyword and it's position. */
static void compile_keyword(Compiler *compiler, const KeywordNode *node) {
    switch (node->keyword) {
    case TOKEN_TRUE_KW: emit_instr(compiler, OP_TRUE, node->pos); break;
    case TOKEN_FALSE_KW: emit_instr(compiler, OP_FALSE, node->pos); break;
    case TOKEN_NULL_KW: emit_instr(compiler, OP_NULL, node->pos); break;
    case TOKEN_CONTINUE_KW: loop_control(compiler, node, &compiler->continues, "continue"); break;
    case TOKEN_BREAK_KW: loop_control(compiler, node, &compiler->breaks, "break"); break;
    default: UNREACHABLE_ERROR();
    }
}

/** Compiles a unary node, which is a normal one but with a specific operation applied to it. */
static void compile_unary(Compiler *compiler, const UnaryNode *node) {
    compile_node(compiler, node->rhs);

    OpCode unaryOp;
    switch (node->operation.type) {
    case TOKEN_MINUS: unaryOp = OP_MINUS; break;
    case TOKEN_BANG: unaryOp = OP_NOT; break;
    default: UNREACHABLE_ERROR();
    }
    emit_instr(compiler, unaryOp, node->operation.pos);
}

/** 
 * Compiles a node with a value to its left and right and an operation between.
 * 
 * In compilation however, we compile the left side, right side and then the operation.
 * This is to make sure both operands are accessible and loaded to the stack when applying
 * their operations.
 */
static void compile_binary(Compiler *compiler, const BinaryNode *node) {
    compile_node(compiler, node->lhs);
    compile_node(compiler, node->rhs);

    OpCode binaryOp;
    switch (node->operation.type) {
    case TOKEN_PLUS: binaryOp = OP_ADD; break;
    case TOKEN_MINUS: binaryOp = OP_SUBTRACT; break;
    case TOKEN_STAR: binaryOp = OP_MULTIPLY; break;
    case TOKEN_SLASH: binaryOp = OP_DIVIDE; break;
    case TOKEN_MODULO: binaryOp = OP_MODULO; break;
    case TOKEN_EXPO: binaryOp = OP_EXPONENT; break;
    case TOKEN_EQ_EQ: binaryOp = OP_EQ; break;
    case TOKEN_BANG_EQ: binaryOp = OP_NOT_EQ; break;
    case TOKEN_GREATER: binaryOp = OP_GREATER; break;
    case TOKEN_GREATER_EQ: binaryOp = OP_GREATER_EQ; break;
    case TOKEN_LESS: binaryOp = OP_LESS; break;
    case TOKEN_LESS_EQ: binaryOp = OP_LESS_EQ; break;
    default: UNREACHABLE_ERROR();
    }
    emit_instr(compiler, binaryOp, node->operation.pos);
}

/** Compiles the stored parenthesized expression. */
static void compile_parentheses(Compiler *compiler, const ParenthesesNode *node) {
    compile_node(compiler, node->expr);
}

/** 
 * Compiles a call to a callable object.
 * 
 * First, we compile the callee (the object being called), then compile the arguments left to right.
 * After that, we emit a call operation that has a number proceeding it, which represents how many
 * argument objects are gonna be loaded on the stack.
 * 
 * Note that the callee is also reliably found one spot deeper in the stack than the first argument.
 */
static void compile_call(Compiler *compiler, const CallNode *node) {
    compile_node(compiler, node->callee);
    for (u32 i = 0; i < node->args.length; i++) {
        compile_node(compiler, node->args.data[i]);
    }
    emit_number(compiler, OP_CALL, node->args.length, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles an expression statement.
 * It's just an expression node that pops the resulting value afterwards.
 */
static void compile_expr_stmt(Compiler *compiler, const ExprStmtNode *node) {
    compile_node(compiler, node->expr);
    emit_pops(compiler, 1, get_node_pos(node->expr));
}

/** Adds a scope that's one layer deeper (usually from entering a new block scope). */
static void push_scope(Compiler *compiler, const ScopeType type) {
    compiler->scopeDepth++;
    if (type == SCOPE_LOOP) {
        APPEND_DA(&compiler->loopScopes, compiler->scopeDepth);
    }
}

/** Collapses the outermost scope, its variables, and emits their respective pop instructions. */
static void pop_scope(Compiler *compiler, ScopeType type) {
    ClosedVariables *currentClosure = CURRENT_LOCALS(compiler);
    u32 poppedAmount = 0;
    while (currentClosure->length > 0 
            && currentClosure->data[currentClosure->length - 1].scope == compiler->scopeDepth) {
        DROP_DA(currentClosure);
        poppedAmount++;
    }

    emit_pops(compiler, poppedAmount, PREVIOUS_OPCODE_POS(compiler));
    if (type == SCOPE_LOOP) {
        DROP_DA(&compiler->loopScopes);
    }   
    compiler->scopeDepth--;
}

/** Compiles an array of statements (doesn't add a scope). */
static void block(Compiler *compiler, const BlockNode *node) {
    for (u32 i = 0; i < node->stmts.length; i++) {
        compile_node(compiler, node->stmts.data[i]);
    }
}

/** Compiles the array of statements in a block under the depth of a new given scope. */
static void scoped_block(Compiler *compiler, const BlockNode *node, const ScopeType type) {
    push_scope(compiler, type);
    block(compiler, node);
    pop_scope(compiler, type);
}

/** 
 * Compiles a variable delcaration statement.
 * 
 * For globals, we'll put them in a hash table at runtime.
 * Therefore, we emit their initialized value, followed by the declaration,
 * then a string to be read afterwards that represents the declared variable's name.
 * 
 * For locals we just compile their expression value
 * and leave it on the stack to be accessed by just indexing it.
 * TODO: add more explanation for multi-variable declarations when they're added.
 */
static void compile_var_decl(Compiler *compiler, const VarDeclNode *node) {
    compile_node(compiler, node->value);
    Token name = node->name;
    Obj *nameAsObj = AS_OBJ(string_obj_from_len(compiler->program, name.lexeme, name.pos.length));

    Variable declaredVar = create_variable(false, node->isConst, name, compiler->scopeDepth);
    if (compiler->scopeDepth == 0) {
        if (get_closed_var_index(compiler->globals, name) != -1) {
            compiler_error(compiler, AS_NODE(node), "Can't redeclare global variable.");
        }
        APPEND_DA(&(compiler)->globals, declaredVar);
        emit_const(compiler, OP_DECLARE_GLOBAL, nameAsObj, name.pos);
    } else {
        // Just let the previously compiled value sit on the stack as a declaration.
        if (get_top_scope_var_index(*CURRENT_LOCALS(compiler), name, compiler->scopeDepth) != -1) {
            compiler_error(compiler, AS_NODE(node), "Can't redeclare local variable.");
        }
        APPEND_DA(CURRENT_LOCALS(compiler), declaredVar);
    }
}

/** 
 * Compiles a variable assignment expression.
 * 
 * For globals it's like declarations, first we emit the value expression's bytecode,
 * then the assign instruction followed by a string which has the assigned variable's name in it.
 * For locals, we simply reassign the index of the stack where they're located,
 * which is statically kept track of in the compiler.
 * TODO: add more explanation for multi-assignments and closure upvalues when they're added.
 */
static void compile_var_assign(Compiler *compiler, const VarAssignNode *node) {
    compile_node(compiler, node->value);
    Token name = node->name;
    Obj *varName = AS_OBJ(string_obj_from_len(compiler->program, name.lexeme, name.pos.length));
    if (compiler->scopeDepth > 0) {
        // TODO: potentially wrap this whole thing in a function that returns whether or not we
        // TODO: should fall off to globals.
        i64 localIdx = get_closed_var_index(*CURRENT_LOCALS(compiler), name);
        if (localIdx != -1) {
            if (CURRENT_LOCALS(compiler)->data[localIdx].isConst) {
                compiler_error(compiler, AS_NODE(node), "Can't assign to const variable.");
            }
            emit_number(compiler, OP_ASSIGN_LOCAL, localIdx, node->name.pos);
            return;
        } else {
            // TODO: Call a function that attempts to find variables above the current closure.
            // TODO: otherwise fall off to globals.
        }
    }

    // It must be global now.
    i64 globalIdx = get_closed_var_index(compiler->globals, name);
    if (globalIdx >= 0 && compiler->globals.data[globalIdx].isConst) {
        compiler_error(compiler, AS_NODE(node), "Can't assign to const variable.");
    } else if (globalIdx == -1) {
        compiler_error(
            compiler, AS_NODE(node), "Assigned variable \"%.*s\" doesn't exist.",
            name.pos.length, name.lexeme
        );
    }
    emit_const(compiler, OP_ASSIGN_GLOBAL, varName, name.pos);
}

/** 
 * Compiles a name that is used to get the value of a variable.
 * 
 * For globals, we emit the var get instruction followede by the string of the name we wanna get
 * from the globals hash table.
 * 
 * For locals, we emit the local get instruction followed by the expected index of the local
 * relative to the execution frame's base pointer (BP) at runtime.
 * TODO: Add explanation for getting closed variables when they're added.
 */
static void compile_var_get(Compiler *compiler, const VarGetNode *node) {
    Token name = node->name;
    Obj *varName = AS_OBJ(
        string_obj_from_len(compiler->program, name.lexeme, name.pos.length)
    );
    if (compiler->scopeDepth > 0) {
        // TODO: potentially wrap this whole thing in a function that returns whether or not we
        // TODO: should fall off to globals.
        i64 localIdx = get_closed_var_index(*CURRENT_LOCALS(compiler), name);
        if (localIdx != -1) {
            emit_number(compiler, OP_GET_LOCAL, localIdx, node->name.pos);
            return;
        } else {
            // TODO: Call a function that attempts to find variables above the current closure.
        }
    }

    // It must be global now.
    if (get_closed_var_index(compiler->globals, name) == -1) {
        compiler_error(
            compiler, AS_NODE(node), "Variable \"%.*s\" doesn't exist.",
            name.pos.length, name.lexeme
        );
    }
    emit_const(compiler, OP_GET_GLOBAL, varName, node->name.pos);
}

/** 
 * Compiles an if-statement and its optional else branch if it exists.
 * 
 * Compiles the condition of the "if" then emits a conditional jump, which goes outside
 * the "if" branch if the condition evaluates to false.
 * Otherwise it falls off to the upcoming bytecode of the if branch.
 * 
 * After the if branch's bytecode there might be an optional else statement' bytecode,
 * which is implemented by adding an extra jump after the if branch's bytecode, so that
 * when the if branch executes, it jumps over the else statement's bytecode.
 * After emitting the jump that skips the else on the if branch, we compile the else branch.
 * 
 * Example:
 *     1 {Condition bytecode}.
 *     2 Go to 5 if previous condition is falsy, otherwise don't.
 *     3 {If branch bytecode}.
 *     4 Go to 6 unconditionally.
 *     5 {Else branch bytecode}.
 *     6 {Bytecode after if-else statement}.
 */
static void compile_if_else(Compiler *compiler, const IfElseNode *node) {
    ByteArray *bytecode = &compiler->func->bytecode;
    compile_node(compiler, node->condition);
    u32 skipIfBranch = emit_unpatched_jump(
        compiler, OP_POP_JUMP_IF_NOT, get_node_pos(AS_NODE(node))
    );
    compile_node(compiler, AS_NODE(node->ifBranch));
    
    if (node->elseBranch != NULL) {
        u32 skipElseBranch = emit_unpatched_jump(compiler, OP_JUMP, get_node_pos(node->elseBranch));
        patch_jump(compiler, skipIfBranch, bytecode->length, true);
        compile_node(compiler, node->elseBranch);
        patch_jump(compiler, skipElseBranch, bytecode->length, true);
    } else {
        patch_jump(compiler, skipIfBranch, bytecode->length, true);
    }
}

/** 
 * Patches a u32 array of loop control (like break or continue) jump indices.
 * Also patches the kind of loop control jump it is, which is unconditionally forward or backwards.
 */
static void patch_loop_control(
    Compiler *compiler, U32Array jumps, const u32 to, const bool isForward
) {
    for (u32 i = 0; i < jumps.length; i++) {
        u32 from = jumps.data[i];
        patch_jump(compiler, from, to, isForward);
        if (isForward) {
            compiler->func->bytecode.data[from] = OP_JUMP;
        } else {
            compiler->func->bytecode.data[from] = OP_JUMP_BACK;
        }
    }
}

/** Starts new arrays for control loop statements (breaks and continues). */
static void push_loop_controls(Compiler *compiler, U32Array *oldBreaks, U32Array *oldContinues) {
    *oldBreaks = compiler->breaks;
    *oldContinues = compiler->continues;
    INIT_DA(&compiler->breaks);
    INIT_DA(&compiler->continues);
}

/** Finishes the arrays of control loops (breaks/continues) and switches to the previous ones. */
static void pop_loop_controls(
    Compiler *compiler, U32Array oldBreaks, const u32 breakTo, const bool breakIsForward,
    U32Array oldContinues, const u32 continueTo, const bool continueIsForward
) {
    patch_loop_control(compiler, compiler->breaks, breakTo, breakIsForward);
    patch_loop_control(compiler, compiler->continues, continueTo, continueIsForward);
    FREE_DA(&compiler->breaks);
    FREE_DA(&compiler->continues);
    compiler->breaks = oldBreaks;
    compiler->continues = oldContinues;
}

/** 
 * Compiles a while loop which executes its body statements as long as its condition is truthy.
 * 
 * Compiles the condition expression of the while loop, then emits a conditional jump which
 * will go over the loop's body if the condition becomes falsy.
 * 
 * After emitting that jump, the body is emitted followed by an unconditional backwards jump,
 * which goes back to the condition of the while loop and the conditional jump over the body.
 * 
 * Example:
 *     1 {Condition bytecode}.
 *     2 Go to 5 if previous condition is falsy, otherwise don't.
 *     3 {While loop body}.
 *     4 Go to 1 unconditionally.
 *     5 {Bytecode outside while loop}.
 */
static void compile_while(Compiler *compiler, const WhileNode *node) {
    const u32 condition = compiler->func->bytecode.length;
    compile_node(compiler, node->condition);
    const u32 skipLoop = emit_unpatched_jump(
        compiler, OP_POP_JUMP_IF_NOT, get_node_pos(AS_NODE(node))
    );

    U32Array oldBreaks, oldContinues;
    push_loop_controls(compiler, &oldBreaks, &oldContinues);
    scoped_block(compiler, node->body, SCOPE_LOOP);
    emit_jump(compiler, OP_JUMP_BACK, condition, false, PREVIOUS_OPCODE_POS(compiler));

    const u32 loopExit = compiler->func->bytecode.length;
    patch_jump(compiler, skipLoop, loopExit, true);
    pop_loop_controls(compiler, oldBreaks, loopExit, true, oldContinues, condition, false);
}

/** 
 * Compiles a do while loop. It's a while loop, but with the condition at the bottom.
 * 
 * First, compile the loop itself entirely, then the loop's condition,
 * followed by a conditional jump back, which goes to the beginning of the loop if the loop's
 * condition evaluated to true, otherwise falls down to the code outside the loop.
 * 
 * Example:
 *     1 {Do while loop body}.
 *     2 {Condition bytecode}.
 *     3 Go to 1 if previous condition is truthy, otherwise don't.
 *     4 {Bytecode outside do while loop}.
 */
static void compile_do_while(Compiler *compiler, const DoWhileNode *node) {
    const u32 loopStart = compiler->func->bytecode.length;
    U32Array oldBreaks, oldContinues;
    push_loop_controls(compiler, &oldBreaks, &oldContinues);
    scoped_block(compiler, node->body, SCOPE_LOOP);
    const u32 condition = compiler->func->bytecode.length;

    compile_node(compiler, node->condition);
    emit_jump(compiler, OP_POP_JUMP_BACK_IF, loopStart, false, get_node_pos(AS_NODE(node)));
    
    const u32 loopExit = compiler->func->bytecode.length;
    pop_loop_controls(compiler, oldBreaks, loopExit, true, oldContinues, condition, true);
}

/** 
 * Compiles a for loop, which loops as long as its iterable isn't fully exhausted.
 * 
 * First thing the for loop does is declare the loop variable inside a new scope and set it to null.
 * After that, we load the object being iterated on, and wrap it around an iterator object.
 * 
 * Then, we start a second scope for the loop where the first instruction attempts
 * to iterate over the created iterator, and if it can't because it's exhausted,
 * it jumps outside the loop, otherwise assigns the iterated
 * element to the loop variable and falls to the bytecode of the loop's body.
 * 
 * Finally, there's an unconditional jump back instruction artificially added after the loop's body
 * to jump back to the "iterate or jump outside loop" instruction.
 * 
 * The reason for having a loop var + iterator scope then a different one for the loop itself is so
 * that jump statements (like break or continue) don't accidentally pop the iterator and loop var.
 * For break, it already jumps to the place where the loop var and iterator pop instructions are,
 * so we don't want to end up popping them multiple times. 
 * 
 * Example:
 *     1 Load null (as a temporary place to hold the loop variable).
 *     2 {Load the thing being iterated/looped on}.
 *     3 Wrap the iterable on an iterator object to prepare for the loop.
 *     4 Try to iterate the iterator and assign it to the loop variable, or if exhausted go to 7.
 *     5 {For loop body}.
 *     6 Go to 4 unconditionally.
 *     7 {Bytecode outside for loop}.
 */
static void compile_for(Compiler *compiler, const ForNode *node) {
    push_scope(compiler, SCOPE_NORMAL);
    emit_instr(compiler, OP_NULL, node->loopVar.pos);
    APPEND_DA(
        CURRENT_LOCALS(compiler), create_variable(false, false, node->loopVar, compiler->scopeDepth)
    );
    APPEND_DA(
        CURRENT_LOCALS(compiler),
        create_variable(false, false, create_normal_token("<iter>", 0), compiler->scopeDepth)
    );

    compile_node(compiler, node->iterable);
    emit_instr(compiler, OP_MAKE_ITER, get_node_pos(node->iterable));
    u32 iterStart = emit_unpatched_jump(compiler, OP_ITER_OR_JUMP, get_node_pos(node->iterable));

    U32Array oldBreaks, oldContinues;
    push_loop_controls(compiler, &oldBreaks, &oldContinues);
    scoped_block(compiler, node->body, SCOPE_LOOP);
    patch_loop_control(compiler, compiler->continues, iterStart, false);
    emit_jump(compiler, OP_JUMP_BACK, iterStart, false, PREVIOUS_OPCODE_POS(compiler));

    const u32 loopExit = compiler->func->bytecode.length;
    patch_jump(compiler, iterStart, loopExit, true);
    pop_loop_controls(compiler, oldBreaks, loopExit, true, oldContinues, iterStart, false);
    pop_scope(compiler, SCOPE_LOOP);
}

/** Compiles an EOF node, which is placed to indicate the end of the bytecode. */
static void compile_eof(Compiler *compiler, const EofNode *node) {
    emit_instr(compiler, OP_END, node->pos);
}

/** Compiles the bytecode for the passed node into the compiler's currently compiling function. */
static void compile_node(Compiler *compiler, const Node *node) {
    switch (node->type) {
    case AST_LITERAL: compile_literal(compiler, AS_PTR(LiteralNode, node)); break;
    case AST_STRING: compile_string(compiler, AS_PTR(StringNode, node)); break;
    case AST_KEYWORD: compile_keyword(compiler, AS_PTR(KeywordNode, node)); break;
    case AST_UNARY: compile_unary(compiler, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: compile_binary(compiler, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: compile_parentheses(compiler, AS_PTR(ParenthesesNode, node)); break;
    case AST_CALL: compile_call(compiler, AS_PTR(CallNode, node)); break;
    case AST_EXPR_STMT: compile_expr_stmt(compiler, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: scoped_block(compiler, AS_PTR(BlockNode, node), SCOPE_NORMAL); break;
    case AST_VAR_DECL: compile_var_decl(compiler, AS_PTR(VarDeclNode, node)); break;
    case AST_VAR_ASSIGN: compile_var_assign(compiler, AS_PTR(VarAssignNode, node)); break;
    case AST_VAR_GET: compile_var_get(compiler, AS_PTR(VarGetNode, node)); break;
    case AST_IF_ELSE: compile_if_else(compiler, AS_PTR(IfElseNode, node)); break;
    case AST_WHILE: compile_while(compiler, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: compile_do_while(compiler, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: compile_for(compiler, AS_PTR(ForNode, node)); break;
    case AST_EOF: compile_eof(compiler, AS_PTR(EofNode, node)); break;
    case AST_ERROR: break; // Do nothing on erroneous nodes.
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** 
 * Compiles bytecode from the AST inside compiler into it's func.
 * 
 * Returns whether or not compilation was successful.
 */
bool compile(Compiler *compiler) {
    for (u32 i = 0; i < compiler->ast.length; i++) {
        compile_node(compiler, compiler->ast.data[i]);
    }
    write_jumps(compiler);

#if DEBUG_BYTECODE
    print_bytecode(compiler->func);
#endif
    return !compiler->program->hasErrored;
}

/** 
 * Returns a compiled compiler. Either it's compiled or everything is set to 0/NULL if errored.
 * 
 * trackPositions is whether we should store positions for bytecodes or not.
 */
Compiler compile_source(ZmxProgram *program, char *source, const bool trackPositions) {
    Compiler emptyCompiler = {
        .trackPositions = false, .program = NULL, .ast = CREATE_DA(),
        .globals = CREATE_DA(), .locals = CREATE_DA(), .scopeDepth = 0, .func = NULL
    };

    Lexer lexer = create_lexer(program, source);
    if (!lex(&lexer)) {
        free_lexer(&lexer);
        return emptyCompiler;
    }

    Parser parser = create_parser(program, lexer.tokens);
    if (!parse(&parser)) {
        free_lexer(&lexer);
        free_parser(&parser);
        free_all_nodes(program);
        return emptyCompiler;
    }

    Compiler compiler = create_compiler(program, parser.ast, trackPositions);
    // Set this compiler as the one being garbage collected. Now compiler's objects can be GC'd.
    program->gc.compiler = &compiler;
    GC_CLEAR_PROTECTED(&program->gc);
    compile(&compiler);

    free_lexer(&lexer);
    free_parser(&parser);
    free_all_nodes(program);
    return compiler;
}
