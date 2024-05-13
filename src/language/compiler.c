#include "compiler.h"
#include "emitter.h"

static void compile_node(Compiler *compiler, AstNode *node);

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, NodeArray ast, bool isDebugging) {
    Compiler compiler = {
        .program = program, .ast = ast, .isDebugging = isDebugging,
        .func = new_func_obj(program, new_string_obj(program, "<global>"))
    };
    return compiler;
}

/** Frees all memory the compiler owns. TODO: current doesn't free anything. */
void free_compiler(Compiler *compiler) {
    // Do nothing for now as the compiler still doesn't own any memory.
    // Later on when we add things like keeping track of variables at compilation time,
    // the compiler will start to own memory it should free here.
}

/** Compiles a literal value which is by adding it's obj form in the constants pool. */
static void compile_literal(Compiler *compiler, LiteralNode *node) {
    Token value = node->value;
    Obj *literalAsObj;
    switch (value.type) {
    case TOKEN_INT_LIT:
        literalAsObj = AS_PTR(new_int_obj(compiler->program, value.intVal), Obj);
        break;
    case TOKEN_FLOAT_LIT:
        literalAsObj = AS_PTR(new_float_obj(compiler->program, value.floatVal), Obj);
        break;
    case TOKEN_STRING_LIT:
        literalAsObj = AS_PTR(new_string_obj(compiler->program, value.stringVal.text), Obj);
        break;
    default: UNREACHABLE_ERROR();
    }
    emit_const(compiler, OP_LOAD_CONST, literalAsObj, node->value.pos);
}

/** 
 * Compiles the node which holds all information of a string (including interpolation).
 * 
 * It alternates between emitting a string const and compiling an interpolated expression.
 * When is compiled, an instruction to convert the expression's result to a string is also emitted.
 * 
 * At the end, we emit an instruction to build a "finished" string from all the string literals
 * and converted expressions that are on the top of the stack.
 */
static void compile_string(Compiler *compiler, StringNode *node) {
    bool nextIsInterpolated = false;
    for (int i = 0; i < node->exprs.length; i++) {
        compile_node(compiler, node->exprs.data[i]);
        if (nextIsInterpolated) {
            // Converts interpolated expression into a string.
            emit_number(compiler, OP_AS, TYPE_STRING, get_node_pos(node->exprs.data[i]));
        }
        nextIsInterpolated = nextIsInterpolated ? false : true;
    }
    emit_number(compiler, OP_FINISH_STRING, node->exprs.length, get_node_pos(node->exprs.data[0]));
}

/** Compiles a keyword node, which is one that holds a bare keyword and it's position. */
static void compile_keyword(Compiler *compiler, KeywordNode *node) {
    switch (node->keyword) {
    case TOKEN_TRUE_KW: emit_instr(compiler, OP_TRUE, node->pos); break;
    case TOKEN_FALSE_KW: emit_instr(compiler, OP_FALSE, node->pos); break;
    default: UNREACHABLE_ERROR();
    }
}

/** Compiles a unary node. */
static void compile_unary(Compiler *compiler, UnaryNode *node) {
    compile_node(compiler, node->rhs);

    OpCode unaryOp;
    switch (node->operation.type) {
    case TOKEN_MINUS: unaryOp = OP_MINUS; break;
    case TOKEN_BANG: unaryOp = OP_NOT; break;
    default: UNREACHABLE_ERROR();
    }
    emit_instr(compiler, unaryOp, node->operation.pos);
}

/** Compiles a binary node. */
static void compile_binary(Compiler *compiler, BinaryNode *node) {
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

/** 
 * Compiles an expression statement.
 * It's just an expression node that pops the resulting value afterwards.
 */
static void compile_expr_stmt(Compiler *compiler, ExprStmtNode *node) {
    compile_node(compiler, node->expr);
    emit_instr(compiler, OP_POP, get_node_pos(node->expr));
}

/** Compiles an EOF node. */
static void compile_eof(Compiler *compiler, EofNode *node) {
    emit_instr(compiler, OP_END, node->pos);
}

/** Compiles the bytecode for the passed node into the compiler's currently compiling function. */
static void compile_node(Compiler *compiler, AstNode *node) {
    switch (node->type) {
        case AST_LITERAL: compile_literal(compiler, AS_PTR(node, LiteralNode)); break;
        case AST_STRING: compile_string(compiler, AS_PTR(node, StringNode)); break;
        case AST_KEYWORD: compile_keyword(compiler, AS_PTR(node, KeywordNode)); break;
        case AST_UNARY: compile_unary(compiler, AS_PTR(node, UnaryNode)); break;
        case AST_BINARY: compile_binary(compiler, AS_PTR(node, BinaryNode)); break;
        case AST_EXPR_STMT: compile_expr_stmt(compiler, AS_PTR(node, ExprStmtNode)); break;
        case AST_EOF: compile_eof(compiler, AS_PTR(node, EofNode)); break;
        case AST_ERROR: break; // Do nothing on erroneous nodes.
        default: UNREACHABLE_ERROR();
    }
}

/** 
 * Compiles bytecode from the AST inside compiler into it's func.
 * 
 * Returns whether or not we've errored.
 */
bool compile(Compiler *compiler) {
    for (int i = 0; i < compiler->ast.length; i++) {
        compile_node(compiler, compiler->ast.data[i]);
    }

    return !compiler->program->hasErrored;
}
