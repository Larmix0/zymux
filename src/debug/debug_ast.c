#include <stdio.h>

#include "constants.h"
#include "debug_ast.h"
#include "report_error.h"

static void eval_node(CharBuffer *astString, const Node *node);

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
        eval_node(astString, node->exprs.data[i]);
    }
}

/** Appends a bare keyword using token_type_as_string. */
static void append_keyword_node(CharBuffer *astString, const KeywordNode *node) {
    buffer_append_string(astString, token_type_as_string(node->keyword));
}

/** Appends a unary node's information. */
static void append_unary_node(CharBuffer *astString, const UnaryNode *node) {
    buffer_append_token(astString, node->operation);
    eval_node(astString, node->rhs);
}

/** Appends a binary node's information. */
static void append_binary_node(CharBuffer *astString, const BinaryNode *node) {
    buffer_append_token(astString, node->operation);
    eval_node(astString, node->lhs);
    eval_node(astString, node->rhs);
}

/** Appends an expression statement, which is just an expression that ends in semicolon. */
static void append_expr_stmt_node(CharBuffer *astString, const ExprStmtNode *node) {
    eval_node(astString, node->expr);
}

/** Appends an EOF string to the AST string. */
static void append_eof_node(CharBuffer *astString) {
    buffer_append_string(astString, "EOF");
    buffer_append_char(astString, ' ');
}

/** Calls the appropriate append function for the passed node. */
static void eval_node(CharBuffer *astString, const Node *node) {
    if (node->type == AST_LITERAL) {
        // Literals don't get wrapped in parenthesis.
        append_literal_node(astString, AS_PTR(LiteralNode, node));
        return;
    }

    buffer_append_char(astString, '(');
    switch (node->type) {
    case AST_ERROR: append_error_node(astString); break;
    case AST_STRING: append_string_node(astString, AS_PTR(StringNode, node)); break;
    case AST_KEYWORD: append_keyword_node(astString, AS_PTR(KeywordNode, node)); break;
    case AST_UNARY: append_unary_node(astString, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: append_binary_node(astString, AS_PTR(BinaryNode, node)); break;
    case AST_EXPR_STMT: append_expr_stmt_node(astString, AS_PTR(ExprStmtNode, node)); break;
    case AST_EOF: append_eof_node(astString); break;
    default: UNREACHABLE_ERROR();
    }
    // Ensures string never has whitespace before closing parenthesis.
    buffer_pop(astString);
    buffer_append_string(astString, ") ");
}

/** Prints the entire passed ast. */
void print_ast(const NodeArray *ast) {
    printf("Abstract syntax tree:\n");

    CharBuffer astString = create_char_buffer();
    for (u32 i = 0; i < ast->length; i++) {
        buffer_append_string(&astString, INDENT);
        eval_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString);
        buffer_append_char(&astString, '\n');
    }
    printf("%s", astString.text);
    free_char_buffer(&astString);
}

/** Allocates a string representation of the AST using the debug delimiter. */
CharBuffer get_ast_string(const NodeArray *ast) {
    CharBuffer astString = create_char_buffer();
    for (u32 i = 0; i < ast->length; i++) {
        if (i != 0) {
            buffer_append_string(&astString, AST_DEBUG_DELIMITER);
        }
        eval_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString);
    }
    return astString;
}
