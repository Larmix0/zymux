#include <stdio.h>

#include "debug_ast.h"
#include "report_error.h"

/** Print an erroneous node. */
static void print_error() {
    printf("<Error>");
}

/** Print a literal's value. */
static void print_literal(LiteralNode *node) {
    printf("%.*s", node->value.pos.length, node->value.lexeme);
}

/** Calls the appropriate print function for the passed node. */
static void print_node(AstNode *node) {
    switch (node->type) {
    case AST_LITERAL: print_literal(AS_PTR(node, LiteralNode)); break;
    case AST_ERROR: print_error(); break;
    default: UNREACHABLE_ERROR();
    }
}

/** Prints the entire passed ast with a newline separated between each statement. */
void print_ast(NodeArray *ast) {
    for (int i = 0; i < ast->length; i++) {
        putchar('(');
        print_node(ast->data[i]);
        printf(")\n");
    }
}
