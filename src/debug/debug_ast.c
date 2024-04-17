#include <stdio.h>

#include "debug_ast.h"
#include "report_error.h"

static void eval_node(CharBuffer *astString, AstNode *node);

/** Abstraction for appending a token to the passed astString. */
static void append_token_to_buffer(CharBuffer *astString, Token token) {
    buffer_append_string_len(astString, token.lexeme, token.pos.length);
    buffer_append_char(astString, ' ');
}

/** Appends a placeholder string for an erroneous node. */
static void append_error_node(CharBuffer *astString) {
    buffer_append_string(astString, "<Error>");
    buffer_append_char(astString, ' ');
}

/** Appends a literal's information. */
static void append_literal_node(CharBuffer *astString, LiteralNode *node) {
    append_token_to_buffer(astString, node->value);
}

/** Appends a unary node's information. */
static void append_unary_node(CharBuffer *astString, UnaryNode *node) {
    append_token_to_buffer(astString, node->operation);
    eval_node(astString, node->rhs);
}

/** Appends a binary node's information. */
static void append_binary_node(CharBuffer *astString, BinaryNode *node) {
    append_token_to_buffer(astString, node->operation);
    eval_node(astString, node->lhs);
    eval_node(astString, node->rhs);
}

/** Calls the appropriate append function for the passed node. */
static void eval_node(CharBuffer *astString, AstNode *node) {
    // Literals don't get wrapped in parenthesis.
    if (node->type == AST_LITERAL) {
        append_literal_node(astString, AS_PTR(node, LiteralNode));
        return;
    }

    buffer_append_char(astString, '(');
    switch (node->type) {
    case AST_UNARY: append_unary_node(astString, AS_PTR(node, UnaryNode)); break;
    case AST_BINARY: append_binary_node(astString, AS_PTR(node, BinaryNode)); break;
    case AST_ERROR: append_error_node(astString); break;
    default: UNREACHABLE_ERROR();
    }
    // Ensures string never has whitespace before closing parenthesis.
    buffer_pop(astString);
    buffer_append_string(astString, ") ");
}

/** Prints the entire passed ast. */
void print_ast(NodeArray *ast) {
    printf("Abstract syntax tree:\n");

    CharBuffer astString = create_char_buffer();
    for (int i = 0; i < ast->length; i++) {
        buffer_append_char(&astString, '\t');
        eval_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString);
        buffer_append_char(&astString, '\n');
    }
    printf("%s", astString.data);
    free_char_buffer(&astString);
}

/** Allocates a string representation of the AST using the debug delimiter. */
CharBuffer allocate_ast_string(NodeArray *ast) {
    CharBuffer astString = create_char_buffer();
    for (int i = 0; i < ast->length; i++) {
        if (i != 0) {
            buffer_append_string(&astString, DEBUG_DELIMITER);
        }
        eval_node(&astString, ast->data[i]);

        // Whitespace after closing parenthesis is spurious here, so pop.
        buffer_pop(&astString);
    }
    return astString;
}
