#include <stdarg.h>
#include <string.h>

#include "compiler.h"
#include "emitter.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"

#if DEBUG_BYTECODE
    #include "debug_bytecode.h"
#endif

/** The position of the previous opcode. Defaults to 0s if there aren't any previous opcodes. */
#define PREVIOUS_OPCODE_POS(compiler) \
    ((compiler)->func->positions.length == 0 \
        ? create_src_pos(0, 0, 0) : LAST_ITEM_DA(&(compiler)->func->positions))

static void compile_node(Compiler *compiler, const Node *node);
static void compile_node_array(Compiler *compiler, const NodeArray *nodes);

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast) {
    GC_PUSH_PROTECTION(&program->gc);

    const char *mainName = "<main>";
    StringObj *name = new_string_obj(program, mainName, strlen(mainName));
    Compiler compiler = {
        .program = program, .ast = ast,
        .func = AS_PTR(FuncObj, new_closure_obj(program, new_func_obj(program, name, 0, -1), true)),
        .jumps = CREATE_DA(), .breaks = CREATE_DA(), .continues = CREATE_DA(),
    };

    GC_POP_PROTECTION(&program->gc);
    return compiler;
}

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler) {
    FREE_DA(&compiler->jumps);
    FREE_DA(&compiler->continues);
    FREE_DA(&compiler->breaks);
}

/**
 * Compiles a literal value.
 * It is done by adding its obj form in the constants pool and emitting the index where its at.
 */
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
        literalAsObj = AS_OBJ(
            new_string_obj(compiler->program, value.stringVal.text, value.stringVal.length)
        );
        break;
    default:
        UNREACHABLE_ERROR();
    }
    emit_const(compiler, OP_LOAD_CONST, literalAsObj, node->value.pos);
}

/** 
 * Compiles the node which holds all information of a string (including interpolation).
 * 
 * It alternates between emitting a string literal and compiling an interpolated expression.
 * When it's an interpolated expression, an instruction to convert the expression's result to
 * a string is also emitted.
 * 
 * At the end, we emit an instruction to build a "finished" string from all the string literals
 * and converted expressions that are on the top of the stack.
 */
static void compile_string(Compiler *compiler, const StringNode *node) {
    bool nextIsString = string_node_starts_with_literal(node);

    for (u32 i = 0; i < node->exprs.length; i++) {
        compile_node(compiler, node->exprs.data[i]);
        if (!nextIsString) {
            // Converts interpolated expression into a string.
            emit_number(compiler, OP_AS, TYPE_STRING, get_node_pos(node->exprs.data[i]));
        }
        nextIsString = !nextIsString;
    }
    // Only emit the concatenation if there's more than one string as optimization.
    if (node->exprs.length > 1) {
        emit_number(compiler, OP_FINISH_STRING, node->exprs.length, get_node_pos(AS_NODE(node)));
    }
}

/** Compiles a keyword node, which is one that holds a bare keyword and its position. */
static void compile_keyword(Compiler *compiler, const KeywordNode *node) {
    switch (node->keyword) {
    case TOKEN_TRUE_KW: emit_instr(compiler, OP_TRUE, node->pos); break;
    case TOKEN_FALSE_KW: emit_instr(compiler, OP_FALSE, node->pos); break;
    case TOKEN_NULL_KW: emit_instr(compiler, OP_NULL, node->pos); break;
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
 * This is to make sure both operands are accessible and loaded to the stack before applying
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
    case TOKEN_PERCENT: binaryOp = OP_MODULO; break;
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

/** Compiles the parenthesized expression as a way to isolate that expression's precedence. */
static void compile_parentheses(Compiler *compiler, const ParenthesesNode *node) {
    compile_node(compiler, node->expr);
}

/** 
 * Compiles a range expression of 3 values: start, end, and step (in that order).
 * This means the VM will have to read them on the stack in reverse to make the range object
 * using the range building instruction.
 */
static void compile_range(Compiler *compiler, const RangeNode *node) {
    compile_node(compiler, node->start);
    compile_node(compiler, node->end);
    compile_node(compiler, node->step);

    emit_instr(compiler, OP_RANGE, node->pos);
}

/** 
 * Compiles a call to a callable object.
 * 
 * First, we compile the callee (the object being called), then compile the arguments left to right.
 * After that, we emit a call operation that has a number proceeding it, which represents how many
 * argument objects are loaded on the stack.
 * 
 * Note that the callee is reliably found just one spot deeper in the stack than the first argument,
 * so the stack might look something like this: [Callee, arg-1, arg2, arg3...]
 * (The callee is lowest in the stack).
 */
static void compile_call(Compiler *compiler, const CallNode *node) {
    compile_node(compiler, node->callee);
    compile_node_array(compiler, &node->args);
    emit_number(compiler, OP_CALL, node->args.length, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles a ternary expression.
 * 
 * Loads the ternary's condition, then true side expression, then false side expression
 * in that order, after that emits the ternary instruction to keep the appropriate instruction
 * at runtime.
 */
static void compile_ternary(Compiler *compiler, const TernaryNode *node) {
    compile_node(compiler, node->condition);
    compile_node(compiler, node->trueExpr);
    compile_node(compiler, node->falseExpr);
    emit_instr(compiler, OP_TERNARY, get_node_pos(AS_NODE(node)));
}

/**  
 * Compiles a list object.
 * 
 * Loads all of the list's elements on the stack in reverse so that the first item on the list
 * will be at the top of the stack, and the last item will be the furthest away.
 * This is so the VM can read it in order while popping from the stack.
 */
static void compile_list(Compiler *compiler, const ListNode *node) {
    for (i64 i = (i64)node->items.length - 1; i >= 0; i--) {
        compile_node(compiler, node->items.data[i]);
    }
    emit_number(compiler, OP_LIST, node->items.length, node->pos);
}

/** 
 * Compiles a map object.
 * 
 * Loads a pair of key, then value elements on the stack.
 * They're loaded with no specific ordering of entries since a map object uses a hash table,
 * so the only thing that matters is
 * that they're key first on top of its corresponding value pair so the VM can
 * start by popping a key, then value, and then repeat for each entry in the map.
 */
static void compile_map(Compiler *compiler, const MapNode *node) {
    for (u32 i = 0; i < node->keys.length; i++) {
        compile_node(compiler, node->values.data[i]);
        compile_node(compiler, node->keys.data[i]);
    }
    emit_number(compiler, OP_MAP, node->keys.length, node->pos);
}

/** 
 * Compiles an expression statement.
 * It's just an expression node that pops the resulting value afterwards because it's not used.
 */
static void compile_expr_stmt(Compiler *compiler, const ExprStmtNode *node) {
    compile_node(compiler, node->expr);
    emit_local_pops(compiler, 1, get_node_pos(node->expr));
}

/**
 * Compiles a block's array of statements then emits pops for its declared variables.
 * Also emits pops for any variables which were captured within that scope.
 */
static void compile_block(Compiler *compiler, const BlockNode *node) {
    compile_node_array(compiler, &node->stmts);

    emit_local_pops(compiler, node->localsAmount, node->pos);
    emit_captured_pops(compiler, node->capturedAmount, node->pos);
}

/**
 * Compiles a normal variable's declaration statement.
 * 
 * Begins by compiling the variable's declared initial value.
 * 
 * Then, for globals, we put them in a hash table at runtime.
 * So, we emit their their name to be used for the hash table after the declaration instruction.
 * 
 * For locals, we just let the compiled value sit on the stack to be accessed/indexed later.
 * For captured locals, we do the same as locals, but emit a capture instruction so the VM
 * knows to capture the variable.
 */
static void compile_var_decl(Compiler *compiler, const VarDeclNode *node) {
    compile_node(compiler, node->value);

    const Token name = node->name;
    switch (node->resolution.scope) {
    case VAR_LOCAL:
        break;
    case VAR_CAPTURED:
        emit_instr(compiler, OP_CAPTURE, node->name.pos);
        break;
    case VAR_GLOBAL: {
        StringObj *nameAsObj = new_string_obj(compiler->program, name.lexeme, name.pos.length);
        emit_const(compiler, OP_DECLARE_GLOBAL, AS_OBJ(nameAsObj), name.pos);
        break;
    }
    case VAR_BUILT_IN:
    case VAR_UNRESOLVED:
        UNREACHABLE_ERROR();
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** 
 * Compiles a variable assignment expression.
 * 
 * Begins by compiling the value to assign to the variable.
 * 
 * For globals, assigning them is like their declarations since we use a hash table at runtime.
 * So, we emit their their name to be used for the hash table after the assignment's value is loaded
 * on the stack and opcode for declaring globals.
 * 
 * Local assignment includes emitting the local assign instruction followed by the index
 * where the variable is expected to be at during the runtime stack,
 * starting relative to the current frame's base pointer at that time.
 * The index is found inside the assign node itself assuming we ran the resolver on the AST.
 * 
 * For captured locals, we do the same as we do for locals, but emit a different instruction
 * followed by a number that access the current function's captured array, instead of the stack.
 * 
 * TODO: add more explanation for closures.
 * TODO: add more explanation for multi-assignments.
 */
static void compile_var_assign(Compiler *compiler, const VarAssignNode *node) {
    compile_node(compiler, node->value);

    const Token name = node->name;
    switch (node->resolution.scope) {
    case VAR_LOCAL:
        emit_number(compiler, OP_ASSIGN_LOCAL, node->resolution.index, name.pos);
        break;
    case VAR_CAPTURED:
        emit_number(compiler, OP_ASSIGN_CAPTURED, node->resolution.index, name.pos);
        break;
    case VAR_GLOBAL: {
        StringObj *nameAsObj = new_string_obj(compiler->program, name.lexeme, name.pos.length);
        emit_const(compiler, OP_ASSIGN_GLOBAL, AS_OBJ(nameAsObj), name.pos);
        break;
    }
    case VAR_BUILT_IN:
    case VAR_UNRESOLVED:
        UNREACHABLE_ERROR();
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** 
 * Compiles a name that is used to get the value of a variable.
 * 
 * Attempts to get locals, then closure variables, then globals, and finally built-ins.
 * 
 * For locals we emit the index they're expected to be in relative to the base pointer of the VM's
 * frame.
 * For captured variables, we do the same but with a different instruction and the number emitted
 * after represents where in the currently executing function's captured array do we expect
 * the variable to be residing.
 * 
 * For global and built-in names, they both utilize hash tables to store their variables,
 * so we just emit a constant object after their respective instruction for the hash table lookup.
 */
static void compile_var_get(Compiler *compiler, const VarGetNode *node) {
    // Just declare the name object here since it's used multiple times.
    const Token name = node->name;
    Obj *nameAsObj = AS_OBJ(new_string_obj(compiler->program, name.lexeme, name.pos.length));
    switch (node->resolution.scope) {
    case VAR_LOCAL:
        emit_number(compiler, OP_GET_LOCAL, node->resolution.index, name.pos);
        break;
    case VAR_CAPTURED:
        emit_number(compiler, OP_GET_CAPTURED, node->resolution.index, name.pos);
        break;
    case VAR_GLOBAL:
        emit_const(compiler, OP_GET_GLOBAL, AS_OBJ(nameAsObj), name.pos);
        break;
    case VAR_BUILT_IN:
        emit_const(compiler, OP_GET_BUILT_IN, AS_OBJ(nameAsObj), name.pos);
        break;
    case VAR_UNRESOLVED:
        UNREACHABLE_ERROR();
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** 
 * Compiles an if-statement and its optional else branch if one exists.
 * 
 * Compiles the condition of the "if" then emits a conditional jump, which skips over
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
    const u32 skipIfBranch = emit_unpatched_jump(
        compiler, OP_POP_JUMP_IF_NOT, get_node_pos(AS_NODE(node))
    );
    compile_node(compiler, AS_NODE(node->ifBranch));
    
    if (node->elseBranch == NULL) {
        patch_jump(compiler, skipIfBranch, bytecode->length, true);
    } else {
        const u32 skipElseBranch = emit_unpatched_jump(
            compiler, OP_JUMP, get_node_pos(node->elseBranch)
        );
        patch_jump(compiler, skipIfBranch, bytecode->length, true);
        compile_node(compiler, node->elseBranch);
        patch_jump(compiler, skipElseBranch, bytecode->length, true);
    }
}

/** Starts new arrays for loop control statements (breaks/continues). */
static void push_loop_controls(Compiler *compiler, U32Array *oldBreaks, U32Array *oldContinues) {
    *oldBreaks = compiler->breaks;
    *oldContinues = compiler->continues;
    INIT_DA(&compiler->breaks);
    INIT_DA(&compiler->continues);
}

/** 
 * Patches a u32 array of some loop control's jump indices (like break or continue).
 * Also patches the kind of jump it is, which is unconditionally either forwards or backwards.
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
    compile_block(compiler, node->body);
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
    compile_block(compiler, node->body);

    const u32 condition = compiler->func->bytecode.length;
    compile_node(compiler, node->condition);
    emit_jump(compiler, OP_POP_JUMP_BACK_IF, loopStart, false, get_node_pos(AS_NODE(node)));
    
    const u32 loopExit = compiler->func->bytecode.length;
    pop_loop_controls(compiler, oldBreaks, loopExit, true, oldContinues, condition, true);
}

/** 
 * Compiles a for loop, which loops as long as its iterable isn't fully exhausted.
 * 
 * First thing the for loop does load a null object as the loop variable.
 * After that, we load the object being iterated on, and wrap it around an iterator object.
 * 
 * Then, we start the loop where the first instruction attempts
 * to iterate over the created iterator, and if it can't because it's exhausted,
 * it jumps outside the loop, otherwise assigns the iterated
 * element to the loop variable and falls to the bytecode of the loop's body.
 * 
 * After, there's an unconditional jump back instruction added after the loop's body
 * to jump back to the "iterate or jump outside loop" instruction.
 * 
 * Finally, after the loop exit we have 2 artificial pops emitted.
 * Those are meant to pop the loop's variable and the iterator,
 * which only get popped after the whole loop is finished, not after after every iteration,
 * unlike other variables that are declared inside the loop and get popped and re-declared after
 * each iteration.
 * 
 * Example:
 *     1 Load null (The loop variable, temporarily null just to place it on the stack).
 *     2 Load the thing being iterated/looped on.
 *     3 Wrap the iterable on an iterator object to prepare for the loop.
 *     4 if exhausted go to 7, otherwise iterate the iterator and assign it to the loop variable.
 *     5 {For loop body}.
 *     6 Go to 4 unconditionally.
 *     7 Pop 2 elements off the top of the stack (the loop variable and iterator).
 *     8 {Bytecode outside for loop}.
 */
static void compile_for(Compiler *compiler, const ForNode *node) {
    compile_var_decl(compiler, node->loopVar);
    compile_node(compiler, node->iterable);
    emit_instr(compiler, OP_MAKE_ITER, get_node_pos(node->iterable));
    u32 iterStart = emit_unpatched_jump(compiler, OP_ITER_OR_JUMP, get_node_pos(node->iterable));

    U32Array oldBreaks, oldContinues;
    push_loop_controls(compiler, &oldBreaks, &oldContinues);
    compile_block(compiler, node->body);
    emit_jump(compiler, OP_JUMP_BACK, iterStart, false, PREVIOUS_OPCODE_POS(compiler));

    const u32 loopExit = compiler->func->bytecode.length;
    emit_local_pops(compiler, 2, PREVIOUS_OPCODE_POS(compiler)); // Pop loop var and iterator.
    patch_jump(compiler, iterStart, loopExit, true);
    pop_loop_controls(compiler, oldBreaks, loopExit, true, oldContinues, iterStart, false);
}

/** Compiles some form of loop control (break/continue), which is to be resolved later. */
static void compile_loop_control(Compiler *compiler, const LoopControlNode *node) {
    emit_local_pops(compiler, node->localsAmount, node->pos);
    emit_captured_pops(compiler, node->capturedAmount, node->pos);

    u32 controlSpot = emit_unpatched_jump(compiler, 0, node->pos);
    switch (node->keyword) {
    case TOKEN_BREAK_KW: APPEND_DA(&compiler->breaks, controlSpot); break;
    case TOKEN_CONTINUE_KW: APPEND_DA(&compiler->continues, controlSpot); break;
    default: UNREACHABLE_ERROR();
    }
}

/** 
 * Finishes compiling the current function on the compiler. TODO: add optional params.
 * 
 * First emits the appropriate instructions for handling the parameters declaration/capturement,
 * then compiles the loop body, after that it writes a default return statement in case the function
 * didn't have any, and finally finishes up the function by writing all statement jumps it had.
 */
static void finish_func(Compiler *compiler, const FuncNode *node) {
    for (u32 i = 0; i < node->capturedParams.length; i++) {
        emit_number(
            compiler, OP_CAPTURE_AT, node->capturedParams.data[i], get_node_pos(AS_NODE(node))
        );
    }

    JumpArray previous = compiler->jumps;
    INIT_DA(&compiler->jumps);

    // Manually compile the body's statements to optimize out pops that occur before the func's end.
    compile_node_array(compiler, &node->body->stmts);
    emit_instr(compiler, OP_NULL, PREVIOUS_OPCODE_POS(compiler));
    emit_instr(compiler, OP_RETURN, PREVIOUS_OPCODE_POS(compiler));
    write_jumps(compiler);
    FREE_DA(&compiler->jumps);
    compiler->jumps = previous;
}

/** 
 * Compiles a generic function of any kind.
 * 
 * We compile functions by switching the compiler's current function to the new one being created,
 * then we compile the function itself. After we're done with that,
 * we emit the new compiled function as a const inside the const pool of the previous function.
 * 
 * TODO: add more differentiating stuff for func types (like methods, initializers, etc.).
 */
static void compile_func(Compiler *compiler, const FuncNode *node) {
    GC_PUSH_PROTECTION(&compiler->program->gc);
    StringObj *nameAsObj = new_string_obj(
        compiler->program, node->nameDecl->name.lexeme, node->nameDecl->name.pos.length
    );
    FuncObj *previous = compiler->func;
    GC_PROTECT_OBJ(&compiler->program->gc, AS_OBJ(previous)); // Created before the protection push.

    compiler->func = new_func_obj(
        compiler->program, nameAsObj, node->params.length, previous->constPool.length
    );
    finish_func(compiler, node);

    const SourcePosition previousPos = PREVIOUS_OPCODE_POS(compiler);
    FuncObj *finished = compiler->func;
    compiler->func = previous;
#if DEBUG_BYTECODE
    print_bytecode(finished);
#endif
    emit_const(
        compiler, node->isClosure ? OP_CLOSURE : OP_LOAD_CONST, AS_OBJ(finished), previousPos
    );
    GC_POP_AND_CLEAR_PROTECTED(&compiler->program->gc);
    compile_var_decl(compiler, node->nameDecl);
}

/** 
 * Compiles a return statement with the returned value being loaded on the stack beforehand.
 * Emits a special return for closures which handles their captured variables.
 */
static void compile_return(Compiler *compiler, const ReturnNode *node) {
    ASSERT(node->capturedPops >= 0, "Expected capture pops to be 0 or positive, not negative.");

    compile_node(compiler, node->returnValue);
    if (node->capturedPops > 0) {
        emit_number(compiler, OP_CLOSURE_RETURN, node->capturedPops, get_node_pos(AS_NODE(node)));
    } else {
        emit_instr(compiler, OP_RETURN, get_node_pos(AS_NODE(node)));
    }
}

/** Compiles an EOF node, which is placed to indicate the end of the bytecode. */
static void compile_eof(Compiler *compiler, const EofNode *node) {
    emit_instr(compiler, OP_END, node->pos);
}

/** Compiles the bytecode for the passed node into the compiler's currently compiling function. */
static void compile_node(Compiler *compiler, const Node *node) {
    if (node == NULL) {
        return;
    }

    switch (node->type) {
    case AST_LITERAL: compile_literal(compiler, AS_PTR(LiteralNode, node)); break;
    case AST_STRING: compile_string(compiler, AS_PTR(StringNode, node)); break;
    case AST_KEYWORD: compile_keyword(compiler, AS_PTR(KeywordNode, node)); break;
    case AST_UNARY: compile_unary(compiler, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: compile_binary(compiler, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: compile_parentheses(compiler, AS_PTR(ParenthesesNode, node)); break;
    case AST_RANGE: compile_range(compiler, AS_PTR(RangeNode, node)); break;
    case AST_CALL: compile_call(compiler, AS_PTR(CallNode, node)); break;
    case AST_TERNARY: compile_ternary(compiler, AS_PTR(TernaryNode, node)); break;
    case AST_LIST: compile_list(compiler, AS_PTR(ListNode, node)); break;
    case AST_MAP: compile_map(compiler, AS_PTR(MapNode, node)); break;
    case AST_EXPR_STMT: compile_expr_stmt(compiler, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: compile_block(compiler, AS_PTR(BlockNode, node)); break;
    case AST_VAR_DECL: compile_var_decl(compiler, AS_PTR(VarDeclNode, node)); break;
    case AST_VAR_ASSIGN: compile_var_assign(compiler, AS_PTR(VarAssignNode, node)); break;
    case AST_VAR_GET: compile_var_get(compiler, AS_PTR(VarGetNode, node)); break;
    case AST_IF_ELSE: compile_if_else(compiler, AS_PTR(IfElseNode, node)); break;
    case AST_WHILE: compile_while(compiler, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: compile_do_while(compiler, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: compile_for(compiler, AS_PTR(ForNode, node)); break;
    case AST_LOOP_CONTROL: compile_loop_control(compiler, AS_PTR(LoopControlNode, node)); break;
    case AST_FUNC: compile_func(compiler, AS_PTR(FuncNode, node)); break;
    case AST_RETURN: compile_return(compiler, AS_PTR(ReturnNode, node)); break;
    case AST_EOF: compile_eof(compiler, AS_PTR(EofNode, node)); break;
    case AST_ERROR: break; // Do nothing on erroneous nodes.
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** Compiles all nodes within the passed array. */
static void compile_node_array(Compiler *compiler, const NodeArray *nodes) {
    for (u32 i = 0; i < nodes->length; i++) {
        compile_node(compiler, nodes->data[i]);
    }
}

/** 
 * Compiles bytecode from the AST inside compiler into its func.
 * 
 * Returns whether or not compilation was successful.
 */
bool compile(Compiler *compiler) {
    compile_node_array(compiler, &compiler->ast);
    write_jumps(compiler);

#if DEBUG_BYTECODE
    print_bytecode(compiler->func);
#endif
    return !compiler->program->hasErrored;
}

/** 
 * Frees everything used and passed from the entire compilation pipeline.
 * 
 * Simply passing NULL for any of the things that are to be freed will skip freeing it
 * (useful when we haven't reached that stage yet, but we want to free somet things.
 */
static void free_out_of_compilation(
    Lexer *lexer, Parser *parser, Resolver *resolver, Compiler *compiler
) {
    if (lexer != NULL) free_lexer(lexer);
    if (parser != NULL) free_parser(parser);
    if (resolver != NULL) free_resolver(resolver);
    if (compiler != NULL) free_compiler(compiler);
}

/** Returns a compiled compiler. Either it's compiled or everything is set to 0/NULL if errored. */
Compiler compile_source(ZmxProgram *program, char *source) {
    Compiler emptyCompiler = {.program = NULL, .func = NULL};

    Lexer lexer = create_lexer(program, source);
    if (!lex(&lexer)) {
        free_out_of_compilation(&lexer, NULL, NULL, NULL);
        return emptyCompiler;
    }

    Parser parser = create_parser(program, lexer.tokens);
    if (!parse(&parser)) {
        free_out_of_compilation(&lexer, &parser, NULL, NULL);
        return emptyCompiler;
    }

    Resolver resolver = create_resolver(program, parser.ast);
    if (!resolve(&resolver)) {
        free_out_of_compilation(&lexer, &parser, &resolver, NULL);
        return emptyCompiler;
    }

    Compiler compiler = create_compiler(program, resolver.ast);
    // Set this compiler as the one being garbage collected. Now compiler's objects can be GC'd.
    program->gc.compiler = &compiler;
    GC_CLEAR_PROTECTED(&program->gc);
    if (!compile(&compiler)) {
        free_out_of_compilation(&lexer, &parser, &resolver, &compiler);
        return emptyCompiler;
    }

    free_out_of_compilation(&lexer, &parser, &resolver, NULL);
    return compiler;
}
