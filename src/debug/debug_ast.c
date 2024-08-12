#include <stdio.h>

#include "constants.h"
#include "debug_ast.h"
#include "report_error.h"

static void append_node(CharBuffer *astString, const Node *node);

/** Abstraction for appending a token to the passed astString. */
static void buffer_append_token(CharBuffer *astString, const Token token) {
    if (token.type == TOKEN_STRING_LIT) {
        buffer_append_char(astString, '"'); // Append starting quote of string literal.
    }
    buffer_append_string_len(astString, token.lexeme, token.pos.length);
    if (token.type == TOKEN_STRING_LIT) {
        buffer_append_char(astString, '"'); // Append ending quote of string literal.
    }
    buffer_append_char(astString, ' ');
}

/** Appends a placeholder string for an erroneous node. */
static void append_error_node(CharBuffer *astString) {
    buffer_append_string(astString, "<Error>");
    buffer_append_char(astString, ' ');
}

/** Appends a literal's information. */
static void append_literal_node(CharBuffer *astString, const LiteralNode *node) {
    buffer_append_token(astString, node->value);
}

/** Appends the node which holds a full string (including the formatting) in it. */
static void append_string_node(CharBuffer *astString, const StringNode *node) {
    buffer_append_string(astString, "<String> ");
    for (u32 i = 0; i < node->exprs.length; i++) {
        append_node(astString, node->exprs.data[i]);
    }
}

/** Appends a node which just holds a keyword token. */
static void append_keyword_node(CharBuffer *astString, const KeywordNode *node) {
    buffer_append_string(astString, token_type_string(node->keyword));
    buffer_append_char(astString, ' ');
}

/** Appends a unary node's information. */
static void append_unary_node(CharBuffer *astString, const UnaryNode *node) {
    buffer_append_token(astString, node->operation);
    append_node(astString, node->rhs);
}

/** Appends a binary node's information. */
static void append_binary_node(CharBuffer *astString, const BinaryNode *node) {
    buffer_append_token(astString, node->operation);
    append_node(astString, node->lhs);
    append_node(astString, node->rhs);
}

/** Appends a parentheses node, which just orders an expression's precedence manually. */
static void append_parentheses_node(CharBuffer *astString, const ParenthesesNode *node) {
    buffer_append_char(astString, '(');
    append_node(astString, node->expr);
    buffer_append_char(astString, ')');
}

/** 
 * Appends a range node, which stores nodes that are supposed to resolve to integers later.
 * Appends a backspace between each number in the range to remove the automatically appended space
 * from the node appending function.
 */
static void append_range_node(CharBuffer *astString, const RangeNode *node) {
    append_node(astString, node->start);
    buffer_append_string(astString, "\b..");
    append_node(astString, node->end);
    buffer_append_string(astString, "\b..");
    append_node(astString, node->step);
}

/** Appends a call with some arguments on an expression that is suppoed to be callable. */
static void append_call_node(CharBuffer *astString, const CallNode *node) {
    buffer_append_string(astString, "<Call> ");
    append_node(astString, node->callee);
    if (node->args.length != 0) {
        buffer_append_string(astString, "-> ");
    }
    for (u32 i = 0; i < node->args.length; i++) {
        append_node(astString, node->args.data[i]);
    }
}

/** Appends an expression statement, which is just an expression that ends in semicolon. */
static void append_expr_stmt_node(CharBuffer *astString, const ExprStmtNode *node) {
    append_node(astString, node->expr);
}

/** Appends a block statement and the other statements/declarations it encapsulates. */
static void append_block_node(CharBuffer *astString, const BlockNode *node) {
    buffer_append_string(astString, "<Block> ");
    for (u32 i = 0; i < node->stmts.length; i++) {
        append_node(astString, node->stmts.data[i]);
    }
}

/** Appends a variable declaration statement's information. */
static void append_var_decl_node(CharBuffer *astString, const VarDeclNode *node) {
    buffer_append_string(astString, node->isConst ? "const " : "let ");
    buffer_append_token(astString, node->name);
    buffer_append_string(astString, "= ");
    append_node(astString, node->value);
}

/** Appends a variable assignment. */
static void append_var_assign_node(CharBuffer *astString, const VarAssignNode *node) {
    buffer_append_token(astString, node->name);
    buffer_append_string(astString, "= ");
    append_node(astString, node->value);
}

/** Appends the name of a variable whose value is to be extracted in the program. */
static void append_var_get_node(CharBuffer *astString, const VarGetNode *node) {
    buffer_append_token(astString, node->name);
}

/** Appends a representation of an if-else statement where the else is optional. */
static void append_if_else_node(CharBuffer *astString, const IfElseNode *node) {
    buffer_append_string(astString, "<if> ");
    append_node(astString, node->condition);
    buffer_append_string(astString, "-> ");
    append_node(astString, AS_NODE(node->ifBranch));
    if (node->elseBranch != NULL) {
        buffer_append_string(astString, "<else> -> ");
        append_node(astString, node->elseBranch);
    }
}

/** Appends a while loop, its condition, and body. */
static void append_while_node(CharBuffer *astString, const WhileNode *node) {
    buffer_append_string(astString, "<while> ");
    append_node(astString, node->condition);
    buffer_append_string(astString, "-> ");
    append_node(astString, AS_NODE(node->body));
}

/** Appends a do while loop, its body, and condition (which is after the body in do while). */
static void append_do_while_node(CharBuffer *astString, const DoWhileNode *node) {
    buffer_append_string(astString, "<do> ");
    append_node(astString, AS_NODE(node->body));
    buffer_append_string(astString, "<while> ");
    append_node(astString, node->condition);
}

/** Appends a for loop, which also includes the loop's variable, iterable, and body. */
static void append_for_node(CharBuffer *astString, const ForNode *node) {
    buffer_append_string(astString, "<for> ");
    buffer_append_token(astString, node->loopVar);
    buffer_append_string(astString, "<in> ");
    append_node(astString, node->iterable);
    buffer_append_string(astString, "-> ");
    append_node(astString, AS_NODE(node->body));
}

/** Appends an EOF string to the AST string. */
static void append_eof_node(CharBuffer *astString) {
    buffer_append_string(astString, "EOF");
    buffer_append_char(astString, ' ');
}

/** Calls the appropriate append function for the passed node. */
static void append_node(CharBuffer *astString, const Node *node) {
    if (node->type == AST_LITERAL) {
        // Literals don't get wrapped in parenthesis.
        append_literal_node(astString, AS_PTR(LiteralNode, node));
        return;
    }

    buffer_append_char(astString, '(');
    switch (node->type) {
    case AST_LITERAL: UNREACHABLE_ERROR(); // Handled at the top of the function.
    case AST_ERROR: append_error_node(astString); break;
    case AST_STRING: append_string_node(astString, AS_PTR(StringNode, node)); break;
    case AST_KEYWORD: append_keyword_node(astString, AS_PTR(KeywordNode, node)); break;
    case AST_UNARY: append_unary_node(astString, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: append_binary_node(astString, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: append_parentheses_node(astString, AS_PTR(ParenthesesNode, node)); break;
    case AST_RANGE: append_range_node(astString, AS_PTR(RangeNode, node)); break;
    case AST_CALL: append_call_node(astString, AS_PTR(CallNode, node)); break;
    case AST_EXPR_STMT: append_expr_stmt_node(astString, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: append_block_node(astString, AS_PTR(BlockNode, node)); break;
    case AST_VAR_DECL: append_var_decl_node(astString, AS_PTR(VarDeclNode, node)); break;
    case AST_VAR_ASSIGN: append_var_assign_node(astString, AS_PTR(VarAssignNode, node)); break;
    case AST_VAR_GET: append_var_get_node(astString, AS_PTR(VarGetNode, node)); break;
    case AST_IF_ELSE: append_if_else_node(astString, AS_PTR(IfElseNode, node)); break;
    case AST_WHILE: append_while_node(astString, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: append_do_while_node(astString, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: append_for_node(astString, AS_PTR(ForNode, node)); break;
    case AST_EOF: append_eof_node(astString); break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    // Ensures string never has whitespace before closing parenthesis.
    buffer_pop(astString);
    buffer_append_string(astString, ") ");
}

/** Prints the entire passed ast. */
void print_ast(const NodeArray *ast) {
    printf("-------------------- AST START --------------------\n");
    CharBuffer astString = create_char_buffer();
    for (u32 i = 0; i < ast->length; i++) {
        buffer_append_string(&astString, INDENT);
        append_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString);
        buffer_append_char(&astString, '\n');
    }
    printf("%s", astString.text);
    free_char_buffer(&astString);
    printf("-------------------- AST END --------------------\n");
}

/** Allocates a string representation of the AST using the debug delimiter. */
CharBuffer get_ast_string(const NodeArray *ast) {
    CharBuffer astString = create_char_buffer();
    for (u32 i = 0; i < ast->length; i++) {
        if (i != 0) {
            buffer_append_string(&astString, AST_DEBUG_DELIMITER);
        }
        append_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString);
    }
    return astString;
}
