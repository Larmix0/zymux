#include <stdarg.h>
#include <string.h>

#include "compiler.h"
#include "debug_bytecode.h"
#include "emitter.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"

/** The position of the previous opcode. Defaults to 0s if there aren't any previous opcodes. */
#define PREVIOUS_OPCODE_POS(compiler) \
    ((compiler)->func->positions.length == 0 \
        ? create_src_pos(0, 0, 0) : LAST_ITEM_DA(&(compiler)->func->positions))

static void compile_node(Compiler *compiler, const Node *node);
static void compile_node_array(Compiler *compiler, const NodeArray *nodes);

/** 
 * Returns a compiler initialized with the passed program and parsed AST.
 * 
 * Note that the compiler returned will have objects not covered by the GC, therefore
 * the GC's compiler should be set to this upon return immediately.
 */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast, const bool isMain) {
    StringObj *name = program->currentFile;
    FuncObj *mainFunc = new_func_obj(program, name, 0, 0, program->currentFile, true);
    GC_PROTECT(&program->gc, AS_OBJ(mainFunc));
    RuntimeFuncObj *mainClosure = new_runtime_func_obj(program, mainFunc, false, true);
    Compiler compiler = {
        .program = program, .ast = ast, .isMain = isMain, .func = AS_PTR(FuncObj, mainClosure),
        .jumps = CREATE_DA(), .breaks = CREATE_DA(), .continues = CREATE_DA(),
    };
    GC_DROP_PROTECTED(&program->gc);
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
    const Token value = node->value;
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
    emit_const(compiler, OP_LOAD_CONST, literalAsObj, get_node_pos(AS_NODE(node)));
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
    case TOKEN_TILDE: unaryOp = OP_TILDE; break;
    default: UNREACHABLE_ERROR();
    }
    emit_instr(compiler, unaryOp, get_node_pos(AS_NODE(node)));
}

/** Handles a binary operation with a data type right hand side (like "is" and "as"). */
static void binary_data_type_op(Compiler *compiler, const BinaryNode *node) {
    ASSERT(node->rhs->type == AST_KEYWORD, "Expected keyword node to compile data type operation.");
    compile_node(compiler, node->lhs);

    DataType dataType;
    switch (AS_PTR(KeywordNode, node->rhs)->keyword) {
    case TOKEN_INT_KW: dataType = TYPE_INT; break;
    case TOKEN_FLOAT_KW: dataType = TYPE_FLOAT; break;
    case TOKEN_BOOL_KW: dataType = TYPE_BOOL; break;
    case TOKEN_STRING_KW: dataType = TYPE_STRING; break;
    default: UNREACHABLE_ERROR();
    }

    emit_number(
        compiler, node->operation.type == TOKEN_IS_KW ? OP_IS : OP_AS, dataType,
        get_node_pos(AS_NODE(node))
    );
}

/** 
 * Handles binary operations where the operation is a logical OR/AND ("||" and "&&").
 * 
 * First loads the left hand side, then emits a conditional jump on the left hand side's truthiness,
 * where if it's an OR it'll short-circuit if the left object is truthy,
 * while leaving the left hand side on the stack.
 * 
 * But, if it's an AND it'll short-circuit if the left hand side is not truthy because we
 * don't need to check the right hand side since the whole expression is the falsy left thing.
 * 
 * Also, binary logical operators don't actually leave booleans on the stack,
 * rather they leave the last object that represents the condition (truthy or falsy)
 * on the stack to be used, whether as a bool or a whole object.
 * This means a truthy AND/a falsy OR will both leave the right hand side object on the stack,
 * while a failed AND will leave the first falsy object on the stack, and a successful OR will
 * leave the first truthy object on the stack.
 */
static void binary_logical_op(Compiler *compiler, const BinaryNode *node) {
    compile_node(compiler, node->lhs);

    OpCode jumpInstr = node->operation.type == TOKEN_BAR_BAR ? OP_JUMP_IF : OP_JUMP_IF_NOT;
    u32 shortCircuit = emit_unpatched_jump(compiler, jumpInstr, get_node_pos(AS_NODE(node)));
    
    emit_local_pops(compiler, 1, get_node_pos(AS_NODE(node)));
    compile_node(compiler, node->rhs);
    patch_jump(compiler, shortCircuit, compiler->func->bytecode.length, true);
}

/** 
 * Compiles a node with a value to its left and right and an operation between.
 * 
 * In compilation however, we compile the left side, right side and then the operation.
 * This is to make sure both operands are accessible and loaded to the stack before applying
 * their operations.
 */
static void compile_binary(Compiler *compiler, const BinaryNode *node) {
    if (node->operation.type == TOKEN_IS_KW || node->operation.type == TOKEN_AS_KW) {
        binary_data_type_op(compiler, node);
        return;
    } else if (node->operation.type == TOKEN_BAR_BAR || node->operation.type == TOKEN_AMPER_AMPER) {
        binary_logical_op(compiler, node);
        return;
    }
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
    case TOKEN_LSHIFT: binaryOp = OP_LSHIFT; break;
    case TOKEN_RSHIFT: binaryOp = OP_RSHIFT; break;
    case TOKEN_BAR: binaryOp = OP_BITWISE_OR; break; 
    case TOKEN_AMPER: binaryOp = OP_BITWISE_AND; break; 
    case TOKEN_CARET: binaryOp = OP_XOR; break; 
    case TOKEN_EQ_EQ: binaryOp = OP_EQUAL; break;
    case TOKEN_BANG_EQ: binaryOp = OP_NOT_EQUAL; break;
    case TOKEN_GREATER: binaryOp = OP_GREATER; break;
    case TOKEN_GREATER_EQ: binaryOp = OP_GREATER_EQ; break;
    case TOKEN_LESS: binaryOp = OP_LESS; break;
    case TOKEN_LESS_EQ: binaryOp = OP_LESS_EQ; break;
    default: UNREACHABLE_ERROR();
    }
    emit_instr(compiler, binaryOp, get_node_pos(AS_NODE(node)));
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
 * Compiles an assignment on a subscripted element.
 * 
 * Compiles the assignment expression first, so it's deepest on the stack, then the thing being
 * subscripted, and finally the subscript expression itself (which will be at the top of the stack).
 * Finally, emits the subscription set instruction.
 */
static void compile_assign_subscr(Compiler *compiler, const AssignSubscrNode *node) {
    compile_node(compiler, node->value);
    compile_node(compiler, node->callee);
    compile_node(compiler, node->subscript);
    emit_instr(compiler, OP_ASSIGN_SUBSCR, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles a subscript get expression.
 * 
 * Compiles the thing being subscripted, then compiles the subscript expression, and emits
 * the getting subscript instruction.
 */
static void compile_get_subscr(Compiler *compiler, const GetSubscrNode *node) {
    compile_node(compiler, node->callee);
    compile_node(compiler, node->subscript);
    emit_instr(compiler, OP_GET_SUBSCR, get_node_pos(AS_NODE(node)));
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

/** Emits the instruction which puts an amount of loaded objects into a list object. */
static void make_list(Compiler *compiler, const u32 length, const SourcePosition pos) {
    emit_number(compiler, OP_LIST, length, pos);
}

/**  
 * Compiles a list object.
 * 
 * Loads all of the list's elements on the stack in order, meaning the topmost stack element
 * will be the last item of the list.
 */
static void compile_list(Compiler *compiler, const ListNode *node) {
    for (u32 i = 0; i < node->items.length; i++) {
        compile_node(compiler, node->items.data[i]);
    }
    make_list(compiler, node->items.length, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles a map object.
 * 
 * Loads a pair of key, then value elements on the stack.
 * The order the pairs are loaded in doesn't matter since maps using hashing.
 * 
 * However, whether the key or the value is the higher one in the stack matters, so for each pair
 * the key is loaded first, then the value, meaning that the VM will first pop a value then its key.
 */
static void compile_map(Compiler *compiler, const MapNode *node) {
    for (u32 i = 0; i < node->keys.length; i++) {
        compile_node(compiler, node->keys.data[i]);
        compile_node(compiler, node->values.data[i]);
    }
    emit_number(compiler, OP_MAP, node->keys.length, node->pos);
}

/** 
 * Compiles a call to a callable object.
 * 
 * First, we compile the callee (the object being called), then compile the positional arguments
 * left to right. After that, compile the map of keyword arguments, then emit a call operation
 * that has a number proceeding it, which represents how many positional arguments there are.
 * 
 * Note that the callee is reliably found just one spot deeper in the stack than the first argument,
 * so the stack might look something like this: [callee, arg 1, arg 2, arg 3... keywordArgsMap]
 * Therefore, the callee is lowest in the stack.
 */
static void compile_call(Compiler *compiler, const CallNode *node) {
    compile_node(compiler, node->callee);
    compile_node_array(compiler, &node->positionalArgs);
    compile_map(compiler, node->keywordArgs);

    emit_number(compiler, OP_CALL, node->positionalArgs.length, get_node_pos(AS_NODE(node)));
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
 * Declares the node's name, with a depth on how deep the declaration's value is on the stack.
 * 
 * for globals, we put them in a hash table at runtime.
 * So, we emit their their name to be used for the hash table and assume the value is at the top.
 * 
 * For locals, we just let the compiled value sit on the stack to be accessed/indexed later.
 * 
 * For captured locals, we do the same as locals, but emit a capture depth instruction so the VM
 * knows to capture the variable and where its corresponding value is at.
 * The depth thing is especially important for multi-variable declaration where we first compile
 * all values and only after start capturing them.
 */
static void declare_variable_depth(
    Compiler *compiler, const DeclareVarNode *node, const u32 valueDepth
) {
    const Token name = node->name;
    switch (node->resolution.scope) {
    case VAR_LOCAL:
        break;
    case VAR_CAPTURED: {
        emit_number(compiler, OP_CAPTURE_DEPTH, valueDepth, name.pos);
        break;
    }
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
 * Compiles a normal variable's declaration statement.
 * 
 * First, compiles the variable's declared initial value, then declares the variable name.
 */
static void compile_declare_var(Compiler *compiler, const DeclareVarNode *node) {
    compile_node(compiler, node->value);
    declare_variable_depth(compiler, node, 0);
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
 */
static void compile_assign_var(Compiler *compiler, const AssignVarNode *node) {
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
static void compile_get_var(Compiler *compiler, const GetVarNode *node) {
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
 * Compiles setting a value to some property which is inside an object.
 */
static void compile_set_property(Compiler *compiler, const SetPropertyNode *node) {
    compile_node(compiler, node->value);
    compile_node(compiler, node->get->originalObj);
    Obj *property = AS_OBJ(new_string_obj(
        compiler->program, node->get->property.lexeme, node->get->property.pos.length
    ));
    emit_const(compiler, OP_SET_PROPERTY, property, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles getting some property out of an accessed object.
 * 
 * First, loads the accessed object to be on the top of the stack, then emits a const string
 * to be read that has the name of the property we're getting.
 */
static void compile_get_property(Compiler *compiler, const GetPropertyNode *node) {
    compile_node(compiler, node->originalObj);
    Obj *property = AS_OBJ(new_string_obj(
        compiler->program, node->property.lexeme, node->property.pos.length
    ));
    emit_const(compiler, OP_GET_PROPERTY, property, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles getting the property of a "super" keyword, which access a superclass.
 * 
 * First compiles getting the instance so it's at the top of the stack, then emits a "super" get
 * instruction that gets a specific method name const from the superclass, which is found
 * inside the loaded instance's class at runtime.
 */
static void compile_get_super(Compiler *compiler, const GetSuperNode *node) {
    compile_get_var(compiler, node->instanceGet);
    Obj *property = AS_OBJ(new_string_obj(
        compiler->program, node->property.lexeme, node->property.pos.length
    ));
    emit_const(compiler, OP_GET_SUPER, property, get_node_pos(AS_NODE(node)));   
}

/** 
 * Compiles multiple declarations on the stack in a row.
 * 
 * First either loads an amount of nulls equal to the variables declared if it's an implicit
 * declaration, or loads an iterable and destructures it on the stack.
 * 
 * After that, it starts declaring the variables in reverse, as the destructure instruction
 * will load the iterable in order, placing the last element on the top of the stack,
 * so we need to start by also declaring the last variable first.
 * 
 * Because declaring a capture doesn't pop its value off of the stack instantly, we need to emit
 * differing depths for the capture instructions so they know which index out of the destructured
 * elements on the stack their value is at.
 */
static void compile_multi_declare(Compiler *compiler, const MultiDeclareNode *node) {
    if (node->value) {
        compile_node(compiler, node->value);
        emit_number(compiler, OP_DESTRUCTURE, node->declarations.length, get_node_pos(node->value));
    } else {
        for (u32 i = 0; i < node->declarations.length; i++) {
            // Implicit null for each variable.
            compile_node(compiler, AS_NODE(NULL_NODE(compiler->program)));
        }
    }

    for (i64 i = (i64)node->declarations.length - 1; i >= 0; i--) {
        DeclareVarNode *variable = AS_PTR(DeclareVarNode, node->declarations.data[i]);
        declare_variable_depth(compiler, variable, i); // "i" is the depth where the value is at. 
    }
}

/** 
 * Compiles a multi-assign node.
 * 
 * For multi-assigns, they must have a set value, so we load that and destructure it.
 * Then, we simply compile all assignments in reverse order, as destructuring will load the iterable
 * value it's expecting in order, meaning that the last destructured item will be at the top
 * of the stack, so we must assign the last variable first.
 */
static void compile_multi_assign(Compiler *compiler, const MultiAssignNode *node) {
    compile_node(compiler, node->value);
    emit_number(compiler, OP_DESTRUCTURE, node->assignments.length, get_node_pos(node->value));
    for (i64 i = (i64)node->assignments.length - 1; i >= 0; i--) {
        compile_node(compiler, node->assignments.data[i]);
        emit_local_pops(compiler, 1, get_node_pos(node->assignments.data[i])); // Pop assigned val.
    }
    compile_node(compiler, node->value); // So original value is left on the stack for use.
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
    compile_block(compiler, node->ifBlock);
    
    if (node->elseBlock) {
        const u32 skipElseBranch = emit_unpatched_jump(
            compiler, OP_JUMP, get_node_pos(node->elseBlock)
        );
        patch_jump(compiler, skipIfBranch, bytecode->length, true);
        compile_node(compiler, node->elseBlock);
        patch_jump(compiler, skipElseBranch, bytecode->length, true);
    } else {
        patch_jump(compiler, skipIfBranch, bytecode->length, true);
    }
}

/** 
 * Compiles one case for a match-case statement.
 * 
 * Copies and compares the matched value with each one of the values in the case label
 * and if any of them match with it,
 * executes the case block which automatically jumps outside the match afterwards.
 * Otherwise, goes to the next case.
 * 
 * Example:
 *     1 Copy matched expression, putting it at the top of the stack.
 *     2 Load the current label value being compared.
 *     3 Compare them, turning them into a boolean at the top of the stack.
 *     4 Pop the boolean after checking it. If it's the last value in the label, jump to the next
 *         - case label and start comparing with it, otherwise drop down to the block's code.
 *         - If it's not the last value, then jump to the block only if the comparison was truthy,
 *         - otherwise fall down to the next label value comparison.
 */
static void compile_case(Compiler *compiler, CaseNode *node, U32Array *exitJumps) {
    U32Array toBlockJumps = CREATE_DA();
    u32 lastCase = 0;
    for (u32 i = 0; i < node->labelVals.length; i++) {
        Node *value = node->labelVals.data[i];
        emit_instr(compiler, OP_COPY_TOP, get_node_pos(value));
        compile_node(compiler, value);
        emit_instr(compiler, OP_EQUAL, get_node_pos(value));
        if (i == node->labelVals.length - 1) {
            // Last label value, will jump to the next case's label if falsy.
            lastCase = emit_unpatched_jump(compiler, OP_POP_JUMP_IF_NOT, get_node_pos(value));
        } else {
            APPEND_DA(
                &toBlockJumps, emit_unpatched_jump(compiler, OP_POP_JUMP_IF, get_node_pos(value))
            );
        }
    }
    patch_all_jumps(compiler, toBlockJumps, compiler->func->bytecode.length, true);
    FREE_DA(&toBlockJumps);

    compile_block(compiler, node->block);
    APPEND_DA(exitJumps, emit_unpatched_jump(compiler, OP_JUMP, PREVIOUS_OPCODE_POS(compiler)));
    patch_jump(compiler, lastCase, compiler->func->bytecode.length, true);
}

/** 
 * Compiles a match's matched expression, cases, and default if there is one.
 * 
 * First loads the matched expression on the stack, then compile each case node which'll append
 * its case ends into the exit jumps array, so that each case block ends with exiting the match
 * automatically without having to use a 'break'.
 * 
 * Then, if there's a 'default', execute its block and just fall down
 * to the match statement's ending.
 */
static void compile_match(Compiler *compiler, const MatchNode *node) {
    compile_node(compiler, node->matchedExpr);

    U32Array exitJumps = CREATE_DA();
    for (u32 i = 0; i < node->cases.length; i++) {
        compile_case(compiler, AS_PTR(CaseNode, node->cases.data[i]), &exitJumps);
    }
    if (node->defaultBlock) {
        compile_block(compiler, node->defaultBlock);
    }

    patch_all_jumps(compiler, exitJumps, compiler->func->bytecode.length, true);
    emit_local_pops(compiler, 1, PREVIOUS_OPCODE_POS(compiler)); // Pop matched expr.
    FREE_DA(&exitJumps);
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
        const u32 from = jumps.data[i];
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
 *     1 Load null(s) (The loop variable(s), temporarily null just to place on the stack).
 *     2 Load the thing being iterated/looped on.
 *     3 Wrap the iterable on an iterator object to prepare for the loop.
 *     4 if exhausted go to 7, otherwise iterate the iterator and assign it to the loop variable(s).
 *     5 {For loop body}.
 *     6 Go to 4 unconditionally.
 *     7 Pop all loop variables and iterator off the top of the stack.
 *     8 {Bytecode outside for loop}.
 */
static void compile_for(Compiler *compiler, const ForNode *node) {
    compile_node(compiler, node->loopVar);
    compile_node(compiler, node->iterable);
    emit_instr(compiler, OP_MAKE_ITER, get_node_pos(node->iterable));

    OpCode iterOp = node->loopVar->type == AST_DECLARE_VAR ? OP_FOR_ITER_ASSIGN : OP_FOR_ITER_LOAD;
    u32 popAmount = 2; // Default loop var and iterator.
    const u32 iterStart = emit_unpatched_jump(compiler, iterOp, get_node_pos(node->iterable));
    if (iterOp == OP_FOR_ITER_LOAD) {
        const u32 varAmount = AS_PTR(MultiDeclareNode, node->loopVar)->declarations.length;
        emit_number(compiler, OP_FOR_ASSIGN_VARS, varAmount, get_node_pos(node->iterable));
        popAmount = varAmount + 1; // +1 to also pop iterator.
    }

    U32Array oldBreaks, oldContinues;
    push_loop_controls(compiler, &oldBreaks, &oldContinues);
    compile_block(compiler, node->body);
    emit_jump(compiler, OP_JUMP_BACK, iterStart, false, PREVIOUS_OPCODE_POS(compiler));

    const u32 loopExit = compiler->func->bytecode.length;
    emit_local_pops(compiler, popAmount, PREVIOUS_OPCODE_POS(compiler)); // Loop vars and iterator.
    patch_jump(compiler, iterStart, loopExit, true);
    pop_loop_controls(compiler, oldBreaks, loopExit, true, oldContinues, iterStart, false);
}

/** Compiles some form of loop control (break/continue), which is to be resolved later. */
static void compile_loop_control(Compiler *compiler, const LoopControlNode *node) {
    emit_local_pops(compiler, node->localsAmount, node->pos);
    emit_captured_pops(compiler, node->capturedAmount, node->pos);

    const u32 controlSpot = emit_unpatched_jump(compiler, 0, node->pos);
    switch (node->keyword) {
    case TOKEN_BREAK_KW: APPEND_DA(&compiler->breaks, controlSpot); break;
    case TOKEN_CONTINUE_KW: APPEND_DA(&compiler->continues, controlSpot); break;
    default: UNREACHABLE_ERROR();
    }
}

/** 
 * Compiles an enum declaration.
 * 
 * Makes an enum object and loads it as a normal const with no special instruction, as enums
 * can be fully handled at compilation time since no changing runtime variables are involved.
 */
static void compile_enum(Compiler *compiler, const EnumNode *node) {
    GC_FREEZE(&compiler->program->gc);
    StringObj *enumName = new_string_obj(
        compiler->program, node->nameDecl->name.lexeme, node->nameDecl->name.pos.length
    );
    EnumObj *enumObj = new_enum_obj(compiler->program, enumName);
    
    for (u32 i = 0; i < node->members.length; i++) {
        StringObj *memberText = new_string_obj(
            compiler->program, node->members.data[i].lexeme, node->members.data[i].pos.length
        );
        EnumMemberObj *memberVal = new_enum_member_obj(
            compiler->program, enumObj, memberText, (ZmxInt)i
        );
        APPEND_DA(&enumObj->members, AS_OBJ(memberVal));
        table_set(
            &enumObj->lookupTable,
            AS_OBJ(memberText), AS_OBJ(new_int_obj(compiler->program, (ZmxInt)i))
        );
    }

    emit_const(compiler, OP_LOAD_CONST, AS_OBJ(enumObj), get_node_pos(AS_NODE(node)));
    compile_declare_var(compiler, node->nameDecl);
    GC_END_FREEZE(&compiler->program->gc);
}

/** Emits the import instruction that imports a file, then declares (binds) it to a variable. */
static void compile_import(Compiler *compiler, const ImportNode *node) {
    Obj *pathAsObj = AS_OBJ(new_string_obj(compiler->program, node->path, node->pathLength));
    emit_const(compiler, OP_IMPORT, pathAsObj, get_node_pos(AS_NODE(node)));
    compile_declare_var(compiler, node->importVar);
}

/** 
 * Imports some variables from a module, declaring them with the same (or different) name.
 * 
 * First imports the module, then emits an import names instruction which grabs a list of names
 * that are expected to be declared in the imported module at the top of the stack,
 * and declares each one of the imported names to a new name (or the same name by default)
 * on the module that imported them.
 * 
 * The imported names list is added in reverse so the VM can declare the imports from top to bottom,
 * which is naturally in reverse.
 */
static void compile_from_import(Compiler *compiler, const FromImportNode *node) {
    Obj *pathAsObj = AS_OBJ(new_string_obj(compiler->program, node->path, node->pathLength));
    emit_const(compiler, OP_IMPORT, pathAsObj, get_node_pos(AS_NODE(node)));

    GC_FREEZE(&compiler->program->gc);
    ListObj *importedList = new_list_obj(compiler->program, (ObjArray)CREATE_DA());
    for (u32 i = 0; i < node->importedNames.length; i++) {
        const Token importedName = node->importedNames.data[i];
        APPEND_DA(
            &importedList->items, 
            AS_OBJ(new_string_obj(compiler->program, importedName.lexeme, importedName.pos.length))
        );
    }
    emit_const(compiler, OP_IMPORT_NAMES, AS_OBJ(importedList), get_node_pos(AS_NODE(node)));
    GC_END_FREEZE(&compiler->program->gc);

    // Declares names in reverse, since declarations go from the top (last value) to bottom (first).
    for (i64 i = (i64)node->namesAs.length - 1; i >= 0; i--) {
        DeclareVarNode *variable = AS_PTR(DeclareVarNode, node->namesAs.data[i]);
        declare_variable_depth(compiler, variable, i); // "i" is the depth where the value is at. 
    }
}

/** 
 * Compiles a try-catch and their blocks.
 * 
 * Adds a start try instruction before the try block in order to include the block's bytecode
 * in the try-catch's protection. After that, the try ends with an automatic jump that goes
 * over the catch statement, as we only execute the catch if an error occurred.
 * 
 * The start try instruction has a boolean before it of whether or not the catch state wants to save
 * an error message to a variable, and emits a number after the instruction, which is where to go
 * to land in the catch statement. The number is the index in bytecode where
 * the catch starts relative to the try start instruction.
 */
static void compile_try_catch(Compiler *compiler, const TryCatchNode *node) {
    emit_instr(compiler, node->catchVar ? OP_TRUE : OP_FALSE, get_node_pos(AS_NODE(node)));
    const u32 trySpot = emit_unpatched_jump(compiler, OP_START_TRY, get_node_pos(AS_NODE(node)));

    compile_block(compiler, node->tryBlock);
    const u32 tryExit = emit_unpatched_jump(compiler, OP_JUMP, PREVIOUS_OPCODE_POS(compiler));

    emit_instr(compiler, OP_FINISH_TRY, PREVIOUS_OPCODE_POS(compiler));
    const u32 catchLocation = compiler->func->bytecode.length;
    patch_jump(compiler, trySpot, catchLocation, true);

    if (node->catchVar) {
        compile_declare_var(compiler, node->catchVar);
    }
    compile_block(compiler, node->catchBlock);
    patch_jump(compiler, tryExit, compiler->func->bytecode.length, true);
}

/** 
 * Compiles a raise statement which will artificially induce an error during the interpreter.
 * 
 * First compiles the entire message node, then emits a bare raise instruction.
 */
static void compile_raise(Compiler *compiler, const RaiseNode *node) {
    compile_node(compiler, node->message);
    emit_instr(compiler, OP_RAISE, get_node_pos(AS_NODE(node)));
}

/** 
 * Compiles a return statement with the returned value being loaded on the stack beforehand.
 * 
 * If there's no value to return, it automatically uses the default return resolved instead.
 * Also, emits a special return for closures which handles their captured variables.
 */
static void compile_return(Compiler *compiler, const ReturnNode *node) {
    ASSERT(node->capturedPops >= 0, "Expected capture pops to be 0 or positive, not negative.");

    compile_node(compiler, node->value != NULL ? node->value : node->defaultVal);
    if (node->capturedPops > 0) {
        emit_number(compiler, OP_CLOSURE_RETURN, node->capturedPops, get_node_pos(AS_NODE(node)));
    } else {
        emit_instr(compiler, OP_RETURN, get_node_pos(AS_NODE(node)));
    }
}

/** 
 * Finishes compiling the current function on the compiler.
 * 
 * First, declares the function itself a second time inside its own self,
 * which makes it treat itself as the first variable (to allow recursion).
 * 
 * After that, gives the function its parameter names and emits the appropriate instructions
 * for handling their declarations/capturement.
 * Then compiles the loop body, writes a default return statement in case the function
 * didn't have any, and finally finishes up the function by writing all statement jumps it had.
 */
static void finish_func(Compiler *compiler, const FuncNode *node) {
    if (node->isMethod) {
        compile_declare_var(compiler, node->innerDecl);
    }
    if (node->innerDecl->resolution.scope == VAR_CAPTURED) {
        // Must manually capture the function underneath the parameters (arity).
        const u32 arity = node->mandatoryParams.length + node->optionalParams.length;
        emit_number(compiler, OP_CAPTURE_DEPTH, arity, get_node_pos(AS_NODE(node)));
    }
    for (u32 i = 0; i < node->capturedParams.length; i++) {
        emit_number(
            compiler, OP_CAPTURE_AT, node->capturedParams.data[i], get_node_pos(AS_NODE(node))
        );
    }
    JumpArray previousJumps = compiler->jumps;
    INIT_DA(&compiler->jumps);
    compile_node_array(compiler, &node->body->stmts);
    compile_return(compiler, node->defaultReturn);

    write_jumps(compiler);
    FREE_DA(&compiler->jumps);
    compiler->jumps = previousJumps;
}

/** Appends an array of parameter declarations to the passed function object's param names. */
static void param_names(ZmxProgram *program, FuncObj *func, const NodeArray params) {
    for (u32 i = 0; i < params.length; i++) {
        DeclareVarNode *param = AS_PTR(DeclareVarNode, params.data[i]);
        StringObj *name = new_string_obj(program, param->name.lexeme, param->name.pos.length);
        APPEND_DA(&func->params.names, AS_OBJ(name));
    }
}

/** 
 * Compiles an array of optional values.
 * 
 * The default expression values of the optional parameters get loaded at runtime
 * in the outer function after the compiled function was loaded on the stack.
 * 
 * This makes it so the func optionals instruction will see the topmost element of the stack
 * as a list of optional values to append to the function which is the second
 * topmost element on the stack
 */
static void optional_param_values(Compiler *compiler, const FuncNode *node) {
    for (u32 i = 0; i < node->optionalParams.length; i++) {
        DeclareVarNode *param = AS_PTR(DeclareVarNode, node->optionalParams.data[i]);
        compile_node(compiler, param->value);
    }
    make_list(compiler, node->optionalParams.length, PREVIOUS_OPCODE_POS(compiler));
    emit_instr(compiler, OP_OPTIONALS_FUNC, PREVIOUS_OPCODE_POS(compiler));
}

/** 
 * Compiles the params of the passed func assuming it's on the stack of the compiler's current func.
 * 
 * The passed function is the one whose parameters get compiled, while the compiler is expected
 * to hold the function at which func is compiled in its bytecode
 * (like <main> when compiling a function created at top-level for example).
 * 
 * First compiles all the names of the parameters statically, and then also compiles default
 * C-NULL values for each parameter such that it can be modified at runtime when optionals
 * are loaded, if there are any.
 * Mandatory params will always stay as C-NULLs for the compiled static func, even at runtime.
 */
static void compile_params(Compiler *compiler, FuncObj *compiledFunc, const FuncNode *node) {
    param_names(compiler->program, compiledFunc, node->mandatoryParams);
    param_names(compiler->program, compiledFunc, node->optionalParams);

    for (u32 i = 0; i < node->mandatoryParams.length + node->optionalParams.length; i++) {
        APPEND_DA(&compiledFunc->params.values, NULL); // A C-NULL value for each param.
    }
    if (node->optionalParams.length > 0) {
        optional_param_values(compiler, node);
    }
}

/** 
 * Declares the passed function onto the bytecode of the currently compiling func in the compiler.
 * 
 * First emits code for the compiled func's constant on the bytecode, then compiles the parameters,
 * and performs an outside declaration so the func in the compiler can access that new func
 * as a variable. Doesn't do it if its a class's method though.
 * 
 */
static void declare_func(Compiler *compiler, FuncObj *compiledFunc, const FuncNode *node) {
    emit_const(
        compiler, node->isClosure ? OP_CLOSURE : OP_LOAD_CONST, AS_OBJ(compiledFunc),
        PREVIOUS_OPCODE_POS(compiler)
    );
    // Outside decl and params here since they require the compiled func to be loaded on the stack.
    compile_params(compiler, compiledFunc, node);
    if (!node->isMethod) {
        compile_declare_var(compiler, node->outerDecl); // Put the loaded func obj on a variable.
    }

    if (compiler->program->cli->debugBytecode || DEBUG_BYTECODE) {
        print_bytecode(compiledFunc);
    }
}

/** 
 * Compiles a generic function of any kind.
 * 
 * We compile functions by switching the compiler's current function to the new one being created,
 * then we compile the function itself. After that, we set the current function as the original one.
 * Finally, we declare the compiled function.
 * 
 * Also, it has to manually protect the original function while the new one is compiling,
 * as the new one will be placed in the compiler's current function instead of the original,
 * meaning it losses its GC protection.
 */
static void compile_func(Compiler *compiler, const FuncNode *node) {
    StringObj *nameAsObj = new_string_obj(
        compiler->program, node->name.lexeme, node->name.pos.length
    );
    FuncObj *original = compiler->func;
    GC_PROTECT(&compiler->program->gc, AS_OBJ(original));
    GC_PROTECT(&compiler->program->gc, AS_OBJ(nameAsObj));
    compiler->func = new_func_obj(
        compiler->program, nameAsObj,
        node->mandatoryParams.length, node->mandatoryParams.length + node->optionalParams.length,
        compiler->program->currentFile, false
    );
    finish_func(compiler, node);
    FuncObj *compiledFunc = compiler->func;
    GC_PROTECT(&compiler->program->gc, AS_OBJ(compiledFunc)); // No longer in the compiler.
    compiler->func = original;
    declare_func(compiler, compiledFunc, node); // Declare the function compiled in the original.

    // Now the name string and the 2 functions are safe stored in the compiler.
    GC_DROP_PROTECTED_AMOUNT(&compiler->program->gc, 3);
}

/** 
 * Compiles the abstract methods of a class.
 * 
 * Does so by loading each abstract method name into the stack as a string, then emitting
 * the abstract methods instruction that treats those strings as names of abstract methods.
 */
static void compile_abstract_methods(Compiler *compiler, const ClassNode *node) {
    if (!node->isAbstract) {
        return;
    }
    
    for (u32 i = 0; i < node->abstractMethods.length; i++) {
        Token *name = &AS_PTR(DeclareVarNode, node->abstractMethods.data[i])->name;
        Obj *methodString = AS_OBJ(
            new_string_obj(compiler->program, name->lexeme, name->pos.length)
        );
        emit_const(
            compiler, OP_LOAD_CONST, methodString, get_node_pos(node->abstractMethods.data[i])
        );
    }
    emit_number(
        compiler, OP_ABSTRACT_METHODS, node->abstractMethods.length, PREVIOUS_OPCODE_POS(compiler)
    );
}

/** 
 * Compile a class declaration and all others things relating to that declaration.
 * 
 * First, create a barebones version of the class with only the name, then emit all methods
 * as functions and add them to that class which is still at the back of the stack.
 * 
 * After that, add the init method separately if there is one.
 */
static void compile_class(Compiler *compiler, const ClassNode *node) {
    StringObj *nameAsObj = new_string_obj(
        compiler->program, node->nameDecl->name.lexeme, node->nameDecl->name.pos.length
    );
    emit_instr(compiler, node->isAbstract ? OP_TRUE : OP_FALSE, get_node_pos(AS_NODE(node)));
    emit_const(compiler, OP_CLASS, AS_OBJ(nameAsObj), get_node_pos(AS_NODE(node)));

    if (node->init) {
        compile_func(compiler, node->init);
        emit_instr(compiler, OP_ADD_INIT, get_node_pos(AS_NODE(node->init)));
    }

    compile_node_array(compiler, &node->methods);
    if (node->methods.length > 0) {
        // Only emit the methods instruction if there's some methods to begin with (optimization).
        emit_number(compiler, OP_METHODS, node->methods.length, get_node_pos(AS_NODE(node)));
    }
    if (node->superclass) {
        // Inherit after methods, so when inheriting abstracts we check if method's were overridden.
        compile_get_var(compiler, node->superclass);
        emit_instr(compiler, OP_INHERIT, get_node_pos(AS_NODE(node->superclass)));
    }
    compile_abstract_methods(compiler, node);
    compile_declare_var(compiler, node->nameDecl);
}

/** 
 * Compiles an EOF node, which is placed to indicate the end of the bytecode.
 * 
 * In a compiler this can indicate the end of an importable file, or the end of the program
 * as a whole (which happens when the exit as at the main file). Emits an end instruction
 * depending on whether it's ending the module or the whole program.
 */
static void compile_eof(Compiler *compiler, const EofNode *node) {
    emit_instr(compiler, compiler->isMain ? OP_EOF : OP_END_MODULE, node->pos);
}

/** Emits an exit with a specific exit code (the runtime will check if it's an integer by then). */
static void compile_exit(Compiler *compiler, const ExitNode *node) {
    compile_node(compiler, node->exitCode);
    emit_instr(compiler, OP_EXIT, node->pos);
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
    case AST_ASSIGN_SUBSCR: compile_assign_subscr(compiler, AS_PTR(AssignSubscrNode, node)); break;
    case AST_GET_SUBSCR: compile_get_subscr(compiler, AS_PTR(GetSubscrNode, node)); break;
    case AST_TERNARY: compile_ternary(compiler, AS_PTR(TernaryNode, node)); break;
    case AST_LIST: compile_list(compiler, AS_PTR(ListNode, node)); break;
    case AST_MAP: compile_map(compiler, AS_PTR(MapNode, node)); break;
    case AST_CALL: compile_call(compiler, AS_PTR(CallNode, node)); break;
    case AST_EXPR_STMT: compile_expr_stmt(compiler, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: compile_block(compiler, AS_PTR(BlockNode, node)); break;
    case AST_DECLARE_VAR: compile_declare_var(compiler, AS_PTR(DeclareVarNode, node)); break;
    case AST_ASSIGN_VAR: compile_assign_var(compiler, AS_PTR(AssignVarNode, node)); break;
    case AST_GET_VAR: compile_get_var(compiler, AS_PTR(GetVarNode, node)); break;
    case AST_SET_PROPERTY: compile_set_property(compiler, AS_PTR(SetPropertyNode, node)); break;
    case AST_GET_PROPERTY: compile_get_property(compiler, AS_PTR(GetPropertyNode, node)); break;
    case AST_GET_SUPER: compile_get_super(compiler, AS_PTR(GetSuperNode, node)); break;
    case AST_MULTI_DECLARE: compile_multi_declare(compiler, AS_PTR(MultiDeclareNode, node)); break;
    case AST_MULTI_ASSIGN: compile_multi_assign(compiler, AS_PTR(MultiAssignNode, node)); break;
    case AST_IF_ELSE: compile_if_else(compiler, AS_PTR(IfElseNode, node)); break;
    case AST_MATCH: compile_match(compiler, AS_PTR(MatchNode, node)); break;
    case AST_WHILE: compile_while(compiler, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: compile_do_while(compiler, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: compile_for(compiler, AS_PTR(ForNode, node)); break;
    case AST_LOOP_CONTROL: compile_loop_control(compiler, AS_PTR(LoopControlNode, node)); break;
    case AST_ENUM: compile_enum(compiler, AS_PTR(EnumNode, node)); break;
    case AST_IMPORT: compile_import(compiler, AS_PTR(ImportNode, node)); break;
    case AST_FROM_IMPORT: compile_from_import(compiler, AS_PTR(FromImportNode, node)); break;
    case AST_TRY_CATCH: compile_try_catch(compiler, AS_PTR(TryCatchNode, node)); break;
    case AST_RAISE: compile_raise(compiler, AS_PTR(RaiseNode, node)); break;
    case AST_RETURN: compile_return(compiler, AS_PTR(ReturnNode, node)); break;
    case AST_FUNC: compile_func(compiler, AS_PTR(FuncNode, node)); break;
    case AST_CLASS: compile_class(compiler, AS_PTR(ClassNode, node)); break;
    case AST_EXIT: compile_exit(compiler, AS_PTR(ExitNode, node)); break;
    case AST_EOF: compile_eof(compiler, AS_PTR(EofNode, node)); break;
    case AST_ERROR: break; // Do nothing on erroneous nodes.
    case AST_CASE: UNREACHABLE_ERROR(); // Should call its function directly as it returns a number.
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
 * (useful when we haven't reached that stage yet, but we want to free some things.
 */
static void free_out_of_compilation(
    Lexer *lexer, Parser *parser, Resolver *resolver, Compiler *compiler
) {
    if (lexer != NULL) free_lexer(lexer);
    if (parser != NULL) free_parser(parser);
    if (resolver != NULL) free_resolver(resolver);
    if (compiler != NULL) free_compiler(compiler);
}

/** 
 * Lexes the source into the lexer and if it didn't error then it parses it onto the parser.
 * 
 * If it fails the program errors. Otherwise, on success it just returns true.
 * In either case it doesn't free neither the lexer nor parser.
 */
static bool parse_source(ZmxProgram *program, Lexer *lexer, Parser *parser) {
    if (!lex(lexer)) {
        return false;
    }

    *parser = create_parser(program, lexer->tokens);
    if (!parse(parser)) {
        return false;
    }
    return true;
}

/** Returns a compiled function or NULL if it errored. */
FuncObj *compile_file_source(ZmxProgram *program, char *source, const bool isMain) {
    Lexer lexer = create_lexer(program, source);
    Parser parser = create_parser(program, lexer.tokens);
    if (!parse_source(program, &lexer, &parser)) {
        free_out_of_compilation(&lexer, &parser, NULL, NULL);
        return NULL;
    }
    Resolver resolver = create_resolver(program, parser.ast);
    if (!resolve(&resolver)) {
        free_out_of_compilation(&lexer, &parser, &resolver, NULL);
        return NULL;
    }

    // Create and set the compiler as the one being GCed. Now compiler's objects can be GCed.
    Compiler compiler = create_compiler(program, resolver.ast, isMain);
    program->gc.compiler = &compiler;
    const bool succeeded = compile(&compiler);
    FuncObj *func = succeeded ? compiler.func : NULL;

    free_out_of_compilation(&lexer, &parser, &resolver, &compiler);
    free_all_token_strings(program);
    free_all_nodes(program);
    program->gc.compiler = NULL;
    return func;
}

/** 
 * Returns a compiled function (NULL if failed) for a REPL source line.
 * 
 * Because REPL requires context of previous lines and what was declared in them,
 * we require passing the specific resolver here to keep track of one resolution
 * for the whole REPL session.
 * 
 * This is also why when we fail we only free the lexer, parser, and compiler,
 * because the resolver lasts the whole REPL session.
 */
FuncObj *compile_repl_source(ZmxProgram *program, char *source, Resolver *resolver) {
    Lexer lexer = create_lexer(program, source);
    Parser parser = create_parser(program, lexer.tokens);
    if (!parse_source(program, &lexer, &parser)) {
        free_out_of_compilation(&lexer, &parser, NULL, NULL);
        return NULL;
    }

    resolver->ast = parser.ast;
    if (!resolve(resolver)) {
        free_out_of_compilation(&lexer, &parser, NULL, NULL);
        return NULL;
    }

    Compiler compiler = create_compiler(program, resolver->ast, true);
    program->gc.compiler = &compiler;
    const bool succeeded = compile(&compiler);
    program->gc.compiler = NULL;
    FuncObj *func = succeeded ? compiler.func : NULL;

    free_out_of_compilation(&lexer, &parser, NULL, &compiler);
    return func;
}
