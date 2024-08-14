#include <stdio.h>

#include "constants.h"
#include "debug_ast.h"
#include "report_error.h"

/** Holds information about an AST string representation being built. */
typedef struct {
    CharBuffer string; /** The string being built. */
    u32 scopeDepth; /** The depth of the blocks in the AST (mostly for indentation). */
    bool forPrinting; /** Whether the AST is built for pretty terminal output or other use. */
} AstBuilder;

static void append_node(AstBuilder *ast, const Node *node);

/** Creates a starting AST builder ready to be written into. */
static AstBuilder create_ast_builder(const bool forPrinting) {
    AstBuilder ast = {.string = create_char_buffer(), .scopeDepth = 0, .forPrinting = forPrinting};
    return ast;
}

/** Frees the contents of the passed AST builder. */
static void free_ast_builder(AstBuilder *ast) {
    free_char_buffer(&ast->string);
}

/** Abstraction for appending a token to the passed ast. */
static void buffer_append_token(CharBuffer *astString, const Token token) {
    if (token.type == TOKEN_STRING_LIT) {
        buffer_append_char(astString, '\''); // Append starting quote of string literal.
    }
    buffer_append_string_len(astString, token.lexeme, token.pos.length);
    if (token.type == TOKEN_STRING_LIT) {
        buffer_append_char(astString, '\''); // Append ending quote of string literal.
    }
    buffer_append_char(astString, ' ');
}

/** Appends the appropriate amount of indentation for a line in the passed AST builder. */
static void append_indents(AstBuilder *ast) {
    for (u32 i = 0; i < ast->scopeDepth; i++) {
        buffer_append_string(&ast->string, INDENT);
    }
}

/** Appends the full array of passed nodes. */
static void append_node_array(AstBuilder *ast, const NodeArray nodes) {
    if (nodes.length == 0) {
        buffer_append_string(&ast->string, "<empty array> ");
        return;
    }

    buffer_append_char(&ast->string, '(');
    buffer_append_string(&ast->string, "<array> ");
    for (u32 i = 0; i < nodes.length; i++) {
        if (i != 0) {
            buffer_pop(&ast->string);
            buffer_append_string(&ast->string, ", ");
        }
        append_node(ast, nodes.data[i]);
    }
    // Move space to after the closing parenthesis
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, ") ");
}

/** Appends a placeholder string for an erroneous node. */
static void append_error_node(AstBuilder *ast) {
    buffer_append_string(&ast->string, "<error>");
    buffer_append_char(&ast->string, ' ');
}

/** Appends a literal's information. */
static void append_literal_node(AstBuilder *ast, const LiteralNode *node) {
    buffer_append_token(&ast->string, node->value);
}

/** Appends the node which holds a full string (including the formatting) in it. */
static void append_string_node(AstBuilder *ast, const StringNode *node) {
    buffer_append_string(&ast->string, "<string> ");
    append_node_array(ast, node->exprs);
}

/** Appends a node which just holds a keyword token. */
static void append_keyword_node(AstBuilder *ast, const KeywordNode *node) {
    buffer_append_string(&ast->string, token_type_string(node->keyword));
    buffer_append_char(&ast->string, ' ');
}

/** Appends a unary node's information. */
static void append_unary_node(AstBuilder *ast, const UnaryNode *node) {
    buffer_append_token(&ast->string, node->operation);
    append_node(ast, node->rhs);
}

/** Appends a binary node's information. */
static void append_binary_node(AstBuilder *ast, const BinaryNode *node) {
    buffer_append_token(&ast->string, node->operation);
    append_node(ast, node->lhs);
    append_node(ast, node->rhs);
}

/** Appends a parentheses node, which just orders an expression's precedence manually. */
static void append_parentheses_node(AstBuilder *ast, const ParenthesesNode *node) {
    buffer_append_char(&ast->string, '(');
    append_node(ast, node->expr);
    buffer_append_char(&ast->string, ')');
}

/** 
 * Appends a range node, which stores nodes that are supposed to resolve to integers later.
 * Appends a backspace between each number in the range to remove the automatically appended space
 * from the node appending function.
 */
static void append_range_node(AstBuilder *ast, const RangeNode *node) {
    append_node(ast, node->start);
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, "..");

    append_node(ast, node->end);
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, "..");

    append_node(ast, node->step);
}

/** Appends a call with some arguments on an expression that is suppoed to be callable. */
static void append_call_node(AstBuilder *ast, const CallNode *node) {
    buffer_append_string(&ast->string, "<call> ");
    append_node(ast, node->callee);
    if (node->args.length == 0) {
        buffer_append_string(&ast->string, "<no args> ");
    }
    append_node_array(ast, node->args);
}

/** Appends an expression statement, which is just an expression that ends in semicolon. */
static void append_expr_stmt_node(AstBuilder *ast, const ExprStmtNode *node) {
    append_node(ast, node->expr);
}

/** Appends a block statement and the other statements/declarations it encapsulates. */
static void append_block_node(AstBuilder *ast, const BlockNode *node) {
    if (node->stmts.length == 0) {
        buffer_append_string(&ast->string, "<Empty block> ");
        return;
    }

    ast->scopeDepth++;
    buffer_append_string(&ast->string, "<block>");
    for (u32 i = 0; i < node->stmts.length; i++) {
        if (ast->forPrinting) {
            buffer_append_string(&ast->string, "\n");
            append_indents(ast);
        }
        append_node(ast, node->stmts.data[i]);
    }
    ast->scopeDepth--;
    buffer_pop(&ast->string);
    if (ast->forPrinting) {
        buffer_append_char(&ast->string, '\n');
        append_indents(ast);
    }
    buffer_append_string(&ast->string, ") ");
}

/** Appends a variable declaration statement's information. */
static void append_var_decl_node(AstBuilder *ast, const VarDeclNode *node) {
    buffer_append_string(&ast->string, node->isConst ? "const " : "let ");
    buffer_append_token(&ast->string, node->name);
    buffer_append_string(&ast->string, "= ");
    append_node(ast, node->value);
}

/** Appends a variable assignment. */
static void append_var_assign_node(AstBuilder *ast, const VarAssignNode *node) {
    buffer_append_token(&ast->string, node->name);
    buffer_append_string(&ast->string, "= ");
    append_node(ast, node->value);
}

/** Appends the name of a variable whose value is to be extracted in the program. */
static void append_var_get_node(AstBuilder *ast, const VarGetNode *node) {
    buffer_append_token(&ast->string, node->name);
}

/** Appends a representation of an if-else statement where the else is optional. */
static void append_if_else_node(AstBuilder *ast, const IfElseNode *node) {
    buffer_append_string(&ast->string, "<if> ");
    append_node(ast, node->condition);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->ifBranch));
    if (node->elseBranch != NULL) {
        buffer_append_string(&ast->string, "<else> -> ");
        append_node(ast, node->elseBranch);
    }
}

/** Appends a while loop, its condition, and body. */
static void append_while_node(AstBuilder *ast, const WhileNode *node) {
    buffer_append_string(&ast->string, "<while> ");
    append_node(ast, node->condition);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->body));
}

/** Appends a do while loop, its body, and condition (which is after the body in do while). */
static void append_do_while_node(AstBuilder *ast, const DoWhileNode *node) {
    buffer_append_string(&ast->string, "<do> -> ");
    append_node(ast, AS_NODE(node->body));
    buffer_append_string(&ast->string, "<while> ");
    append_node(ast, node->condition);
}

/** Appends a for loop, which also includes the loop's variable, iterable, and body. */
static void append_for_node(AstBuilder *ast, const ForNode *node) {
    buffer_append_string(&ast->string, "<for> ");
    buffer_append_token(&ast->string, node->loopVar);
    buffer_append_string(&ast->string, "<in> ");
    append_node(ast, node->iterable);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->body));
}

/** Appends a generic function node, which includes its name, parameters, and body. */
static void append_func_node(AstBuilder *ast, const FuncNode *node) {
    buffer_append_string(&ast->string, "<func> ");
    buffer_append_token(&ast->string, node->name);
    append_node_array(ast, node->params);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->body));
}

/** Appends an EOF string to the AST string. */
static void append_eof_node(AstBuilder *ast) {
    buffer_append_string(&ast->string, "EOF");
    buffer_append_char(&ast->string, ' ');
}

/** Calls the appropriate append function for the passed node. */
static void append_node(AstBuilder *ast, const Node *node) {
    if (node->type == AST_LITERAL) {
        // Literals don't get wrapped in parenthesis.
        append_literal_node(ast, AS_PTR(LiteralNode, node));
        return;
    }

    buffer_append_char(&ast->string, '(');
    switch (node->type) {
    case AST_LITERAL: UNREACHABLE_ERROR(); // Handled at the top of the function.
    case AST_ERROR: append_error_node(ast); break;
    case AST_STRING: append_string_node(ast, AS_PTR(StringNode, node)); break;
    case AST_KEYWORD: append_keyword_node(ast, AS_PTR(KeywordNode, node)); break;
    case AST_UNARY: append_unary_node(ast, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: append_binary_node(ast, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: append_parentheses_node(ast, AS_PTR(ParenthesesNode, node)); break;
    case AST_RANGE: append_range_node(ast, AS_PTR(RangeNode, node)); break;
    case AST_CALL: append_call_node(ast, AS_PTR(CallNode, node)); break;
    case AST_EXPR_STMT: append_expr_stmt_node(ast, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: append_block_node(ast, AS_PTR(BlockNode, node)); break;
    case AST_VAR_DECL: append_var_decl_node(ast, AS_PTR(VarDeclNode, node)); break;
    case AST_VAR_ASSIGN: append_var_assign_node(ast, AS_PTR(VarAssignNode, node)); break;
    case AST_VAR_GET: append_var_get_node(ast, AS_PTR(VarGetNode, node)); break;
    case AST_IF_ELSE: append_if_else_node(ast, AS_PTR(IfElseNode, node)); break;
    case AST_WHILE: append_while_node(ast, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: append_do_while_node(ast, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: append_for_node(ast, AS_PTR(ForNode, node)); break;
    case AST_FUNC: append_func_node(ast, AS_PTR(FuncNode, node)); break;
    case AST_EOF: append_eof_node(ast); break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    // Ensures string never has whitespace before closing parenthesis.
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, ") ");
}

/** Prints the entire passed ast. */
void print_ast(const NodeArray *ast) {
    printf("-------------------- AST START --------------------\n");
    AstBuilder astString = create_ast_builder(true);
    for (u32 i = 0; i < ast->length; i++) {
        append_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString.string);
        buffer_append_char(&astString.string, '\n');
    }
    printf("%s", astString.string.text);
    free_ast_builder(&astString);
    printf("-------------------- AST END --------------------\n");
}

/** Allocates a string representation of the AST using the debug delimiter. */
CharBuffer get_ast_string(const NodeArray *ast) {
    AstBuilder astString = create_ast_builder(false);
    for (u32 i = 0; i < ast->length; i++) {
        if (i != 0) {
            buffer_append_string(&astString.string, AST_DEBUG_DELIMITER);
        }
        append_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString.string);
    }
    return astString.string;
}
