#include <stdarg.h>

#include "debug_ast.h"
#include "debug_bytecode.h"
#include "debug_token.h"
#include "compiler.h"
#include "emitter.h"
#include "lexer.h"
#include "parser.h"

/** Returns the outermost closure of scopes. */
#define CURRENT_LOCALS(compiler) (&(compiler)->locals.data[(compiler)->locals.length - 1])

/** The position of the previous opcode. Defaults to (0, 0, 0) if there aren't any. */
#define PREVIOUS_OPCODE_POS(compiler) \
    (compiler)->func->positions.length == 0 \
        ? create_src_pos(0, 0, 0) \
        : (compiler)->func->positions.data[(compiler)->func->positions.length - 1]

static void compile_node(Compiler *compiler, const Node *node);

/** Reports a compilation error on a specific node's main token. */
static void compiler_error(Compiler *compiler, const Node *erroredNode, const char *format, ...) {
    va_list args;
    va_start(args, format);
    zmx_user_error(compiler->program, get_node_pos(erroredNode), "Compiler error", format, &args);
    va_end(args);
}

/** Returns a compiler initialized with the passed program and parsed AST. */
Compiler create_compiler(ZmxProgram *program, const NodeArray ast, bool isDebugging) {
    Compiler compiler = {
        .isDebugging = isDebugging, .program = program, .ast = ast,
        .globals = CREATE_DA(), .locals = CREATE_DA(), .scopeDepth = 0,
        .func = new_func_obj(program, new_string_obj(program, "<main>"), -1)
    };
    // Add the default, permanent closure for locals not covered by functions.
    APPEND_DA(&compiler.locals, (ClosedVariables)CREATE_DA());
    return compiler;
}

/** Frees all memory the compiler owns. */
void free_compiler(Compiler *compiler) {
    for (u32 i = 0; i < compiler->locals.length; i++) {
        FREE_DA(&compiler->locals.data[i]);
    }
    FREE_DA(&compiler->locals);
    FREE_DA(&compiler->globals);
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

/** Compiles a literal value which is by adding it's obj form in the constants pool. */
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
    emit_number(compiler, OP_FINISH_STRING, node->exprs.length, get_node_pos(node->exprs.data[0]));
}

/** Compiles a keyword node, which is one that holds a bare keyword and it's position. */
static void compile_keyword(Compiler *compiler, const KeywordNode *node) {
    switch (node->keyword) {
    case TOKEN_TRUE_KW: emit_instr(compiler, OP_TRUE, node->pos); break;
    case TOKEN_FALSE_KW: emit_instr(compiler, OP_FALSE, node->pos); break;
    case TOKEN_NULL_KW: emit_instr(compiler, OP_NULL, node->pos); break;
    default: UNREACHABLE_ERROR();
    }
}

/** Compiles a unary node. */
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

/** Compiles a binary node. */
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

/** 
 * Compiles an expression statement.
 * It's just an expression node that pops the resulting value afterwards.
 */
static void compile_expr_stmt(Compiler *compiler, const ExprStmtNode *node) {
    compile_node(compiler, node->expr);
    emit_instr(compiler, OP_POP, get_node_pos(node->expr));
}

/** Begins a scope that's one layer deeper. */
static void add_scope(Compiler *compiler) {
    compiler->scopeDepth++;
}

/** Collapses the outermost scope, its variables and sends their respective pop instructions. */
static void collapse_scope(Compiler *compiler) {
    ClosedVariables *currentClosure = CURRENT_LOCALS(compiler);
    while (currentClosure->length > 0 
            && currentClosure->data[currentClosure->length - 1].scope == compiler->scopeDepth) {
        emit_instr(compiler, OP_POP, PREVIOUS_OPCODE_POS(compiler));
        DROP_DA(currentClosure);
    }
    compiler->scopeDepth--;
}

/** Compiles the array of statements in a block under the depth of the given scope. */
static void compile_block(Compiler *compiler, const BlockNode *node) {
    add_scope(compiler);
    for (u32 i = 0; i < node->stmts.length; i++) {
        compile_node(compiler, node->stmts.data[i]);
    }
    collapse_scope(compiler);
}

/** Compiles a variable delcaration statement. */
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

/** Compiles a variable assignment expression. */
static void compile_var_assign(Compiler *compiler, const VarAssignNode *node) {
    compile_node(compiler, node->value);
    Token name = node->name;
    Obj *varName = AS_OBJ(string_obj_from_len(compiler->program, name.lexeme, name.pos.length));
    if (compiler->scopeDepth > 0) {
        // TODO: potentially wrap this whole thing in a function that returns whether or not we
        // TODO: should fall off to globals.
        i64 localIdx = get_closed_var_index(*CURRENT_LOCALS(compiler), name);
        if (localIdx != -1) {
            emit_number(compiler, OP_ASSIGN_LOCAL, localIdx, node->name.pos);
            return;
        } else {
            // TODO: Call a function that attempts to find variables above the current closure.
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

/** Compiles a name that is used to get the value of a variable. */
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

/** Compiles an EOF node. */
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
        case AST_EXPR_STMT: compile_expr_stmt(compiler, AS_PTR(ExprStmtNode, node)); break;
        case AST_BLOCK: compile_block(compiler, AS_PTR(BlockNode, node)); break;
        case AST_VAR_DECL: compile_var_decl(compiler, AS_PTR(VarDeclNode, node)); break;
        case AST_VAR_ASSIGN: compile_var_assign(compiler, AS_PTR(VarAssignNode, node)); break;
        case AST_VAR_GET: compile_var_get(compiler, AS_PTR(VarGetNode, node)); break;
        case AST_EOF: compile_eof(compiler, AS_PTR(EofNode, node)); break;
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
    for (u32 i = 0; i < compiler->ast.length; i++) {
        compile_node(compiler, compiler->ast.data[i]);
    }

    return !compiler->program->hasErrored;
}

/** 
 * Returns a compiler whose main func is fully compiled.
 * 
 * debugByteCode controls whether or not we keep track of the positions of each bytecode,
 * and also whether or not we should print the lexed tokens, parsed AST, and compiled bytecode.
 * 
 * Returns NULL instead of the main function if an error has occurred during any point
 * of the compilation.
 */
FuncObj *compile_source(ZmxProgram *program, char *source, const bool debugBytecode) {
    Lexer lexer = create_lexer(program, source);
    if (!lex(&lexer)) {
        free_lexer(&lexer);
        return NULL;
    }
    if (debugBytecode && DEBUG_LEXER) {
        print_tokens(lexer.tokens);
    }

    Parser parser = create_parser(program, lexer.tokens);
    if (!parse(&parser)) {
        free_lexer(&lexer);
        free_parser(&parser);
        free_all_nodes(program);
        return NULL;
    }
    if (debugBytecode && DEBUG_PARSER) {
        print_ast(&parser.ast);
    }

    Compiler compiler = create_compiler(program, parser.ast, debugBytecode);
    compile(&compiler);
    if (!program->hasErrored && debugBytecode && DEBUG_COMPILER) {
        print_bytecode(compiler.func);
    }
    free_lexer(&lexer);
    free_parser(&parser);
    free_compiler(&compiler);
    free_all_nodes(program);
    return program->hasErrored ? NULL : compiler.func;
}
