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
static void append_node_array(AstBuilder *ast, const NodeArray *nodes);

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

/** Appends a placeholder string for an erroneous node. */
static void append_error(AstBuilder *ast) {
    buffer_append_string(&ast->string, "<error> ");
}

/** Appends a literal's information. */
static void append_literal_node(AstBuilder *ast, const LiteralNode *node) {
    buffer_append_token(&ast->string, node->value);
}

/** Appends the node which holds a full string (including the formatting) in it. */
static void append_string(AstBuilder *ast, const StringNode *node) {
    buffer_append_string(&ast->string, "<string> ");
    append_node_array(ast, &node->exprs);
}

/** Appends a node which just holds a keyword token. */
static void append_keyword(AstBuilder *ast, const KeywordNode *node) {
    buffer_append_format(&ast->string, "%s ", token_type_string(node->keyword));
}

/** Appends a unary node's information. */
static void append_unary(AstBuilder *ast, const UnaryNode *node) {
    buffer_append_token(&ast->string, node->operation);
    append_node(ast, node->rhs);
}

/** Appends a binary node's information. */
static void append_binary(AstBuilder *ast, const BinaryNode *node) {
    buffer_append_token(&ast->string, node->operation);
    append_node(ast, node->lhs);
    append_node(ast, node->rhs);
}

/** Appends a parentheses node, which just orders an expression's precedence manually. */
static void append_parentheses(AstBuilder *ast, const ParenthesesNode *node) {
    buffer_append_char(&ast->string, '(');
    append_node(ast, node->expr);
    buffer_append_char(&ast->string, ')');
}

/** 
 * Appends a range node, which stores nodes that are supposed to resolve to integers later.
 * Appends a backspace between each number in the range to remove the automatically appended space
 * from the node appending function.
 */
static void append_range(AstBuilder *ast, const RangeNode *node) {
    append_node(ast, node->start);
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, "..");

    append_node(ast, node->end);
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, "..");

    append_node(ast, node->step);
}

/** Appends a call with some arguments on an expression that is suppoed to be callable. */
static void append_call(AstBuilder *ast, const CallNode *node) {
    buffer_append_string(&ast->string, "<call> ");
    append_node(ast, node->callee);
    append_node_array(ast, &node->args);
}

/** Appends a subscript assignment's subscript, subscripted, and value being assigned. */
static void append_assign_subscr(AstBuilder *ast, const AssignSubscrNode *node) {
    buffer_append_string(&ast->string, "<get subscript> ");
    append_node(ast, node->callee);
    append_node(ast, node->subscript);
    buffer_append_string(&ast->string, "= ");
    append_node(ast, node->value);
}

/** Appends a subscript get node's callee being subscripted, and the subscript expression itself. */
static void append_get_subscr(AstBuilder *ast, const GetSubscrNode *node) {
    buffer_append_string(&ast->string, "<get subscript> ");
    append_node(ast, node->callee);
    append_node(ast, node->subscript);
}

/** Appends a ternary node to the AST. */
static void append_ternary(AstBuilder *ast, const TernaryNode *node) {
    buffer_append_string(&ast->string, "<ternary> ");
    append_node(ast, node->condition);
    append_node(ast, node->trueExpr);
    append_node(ast, node->falseExpr);
}

/** Appends a list object that stores multiple nodes in an order. */
static void append_list(AstBuilder *ast, const ListNode *node) {
    buffer_append_string(&ast->string, "<list> ");
    append_node_array(ast, &node->items);
}

/** Appends a map of multiple key-value pairs to the AST. */
static void append_map(AstBuilder *ast, const MapNode *node) {
    if (node->keys.length == 0) {
        buffer_append_string(&ast->string, "<empty map> ");
        return;
    }
    buffer_append_string(&ast->string, "<map> ");
    for (u32 i = 0; i < node->keys.length; i++) {
        if (i != 0) {
            buffer_pop(&ast->string); // Pop the spurious space before the comma.
            buffer_append_string(&ast->string, ", ");
        }
        append_node(ast, node->keys.data[i]);
        buffer_append_string(&ast->string, ": ");
        append_node(ast, node->values.data[i]);
    }
}

/** Appends an expression statement, which is just an expression that ends in semicolon. */
static void append_expr_stmt(AstBuilder *ast, const ExprStmtNode *node) {
    append_node(ast, node->expr);
}

/** Appends a block statement and the other statements/declarations it encapsulates. */
static void append_block(AstBuilder *ast, const BlockNode *node) {
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
static void append_declare_var(AstBuilder *ast, const DeclareVarNode *node) {
    buffer_append_string(&ast->string, node->isConst ? "const " : "let ");
    buffer_append_token(&ast->string, node->name);
    if (node->value) {
        buffer_append_string(&ast->string, "= ");
        append_node(ast, node->value);
    }
}

/** Appends a variable assignment. */
static void append_assign_var(AstBuilder *ast, const AssignVarNode *node) {
    buffer_append_token(&ast->string, node->name);
    if (node->value) {
        buffer_append_string(&ast->string, "= ");
        append_node(ast, node->value);
    }
}

/** Appends the name of a variable whose value is to be extracted in the program. */
static void append_get_var(AstBuilder *ast, const GetVarNode *node) {
    buffer_append_token(&ast->string, node->name);
}

/** Appends a property set */
static void append_set_property(AstBuilder *ast, const SetPropertyNode *node) {
    append_node(ast, AS_NODE(node->get));
    buffer_append_string(&ast->string, "= ");
    append_node(ast, node->value);
}

/** Appends a property get, which also has an original object that we get the property from. */
static void append_get_property(AstBuilder *ast, const GetPropertyNode *node) {
    append_node(ast, node->originalObj);
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, ".");
    buffer_append_token(&ast->string, node->property);
}

/** Appends a multi-declaration statement's declared names and the value they're set to. */
static void append_multi_declare(AstBuilder *ast, const MultiDeclareNode *node) {
    buffer_append_string(&ast->string, "<multi declare> ");
    append_node_array(ast, &node->declarations);
    if (node->value) {
        buffer_append_string(&ast->string, "= ");
        append_node(ast, node->value);
    }
}

/** Appends an expression which assigns multiple variables to a value. */
static void append_multi_assign(AstBuilder *ast, const MultiAssignNode *node) {
    buffer_append_string(&ast->string, "<multi assign> ");
    append_node_array(ast, &node->assignments);
    if (node->value) {
        buffer_append_string(&ast->string, "= ");
        append_node(ast, node->value);
    }
}

/** Appends a representation of an if-else statement where the else is optional. */
static void append_if_else(AstBuilder *ast, const IfElseNode *node) {
    buffer_append_string(&ast->string, "<if> ");
    append_node(ast, node->condition);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->ifBlock));
    if (node->elseBlock) {
        buffer_append_string(&ast->string, "<else> -> ");
        append_node(ast, node->elseBlock);
    }
}

/** Appends a try-catch statements with both of its blocks. */
static void append_try_catch(AstBuilder *ast, const TryCatchNode *node) {
    buffer_append_string(&ast->string, "<try> ");
    append_node(ast, AS_NODE(node->tryBlock));
    buffer_append_string(&ast->string, "<catch> ");
    append_node(ast, AS_NODE(node->catchBlock));
}

/** Appends the matched expression, case labels, their corresponding blocks, and default if used. */
static void append_match(AstBuilder *ast, const MatchNode *node) {
    buffer_append_string(&ast->string, "<match> ");
    append_node(ast, node->matchedExpr);
    buffer_append_string(&ast->string, "-> ");
    for (u32 i = 0; i < node->caseLabels.length; i++) {
        buffer_append_string(&ast->string, "<case> ");
        append_node(ast, node->caseLabels.data[i]);
        buffer_append_string(&ast->string, "-> ");
        append_node(ast, node->caseBlocks.data[i]);
    }
    if (node->defaultCase) {
        buffer_append_string(&ast->string, "<default> -> ");
        append_node(ast, AS_NODE(node->defaultCase));
    } else {
        buffer_append_string(&ast->string, "<no default> ");
    }
}

/** Appends a while loop, its condition, and body. */
static void append_while(AstBuilder *ast, const WhileNode *node) {
    buffer_append_string(&ast->string, "<while> ");
    append_node(ast, node->condition);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->body));
}

/** Appends a do while loop, its body, and condition (which is after the body in do while). */
static void append_do_while(AstBuilder *ast, const DoWhileNode *node) {
    buffer_append_string(&ast->string, "<do> -> ");
    append_node(ast, AS_NODE(node->body));
    buffer_append_string(&ast->string, "<while> ");
    append_node(ast, node->condition);
}

/** Appends a for loop, which also includes the loop's variable(s), iterable, and body. */
static void append_for(AstBuilder *ast, const ForNode *node) {
    buffer_append_string(&ast->string, "<for> ");
    append_node(ast, node->loopVar);
    buffer_append_string(&ast->string, "<in> ");
    append_node(ast, node->iterable);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->body));
}

/** Appends a loop control statement/keyword (break or continue). */
static void append_loop_control(AstBuilder *ast, const LoopControlNode *node) {
    buffer_append_format(&ast->string, "%s ", token_type_string(node->keyword));
}

/** Appends an enum and its enumerators (members). */
static void append_enum(AstBuilder *ast, const EnumNode *node) {
    buffer_append_format(&ast->string, "<enum> ");
    buffer_append_token(&ast->string, node->nameDecl->name);
    buffer_append_string(&ast->string, "-> ");
    for (u32 i = 0; i < node->members.length; i++) {
        if (i != 0) {
            buffer_pop(&ast->string); // Pop spurious space.
            buffer_append_string(&ast->string, ", ");
        }
        buffer_append_token(&ast->string, node->members.data[i]);
    }
}

/** Appends a return node + the returned value. */
static void append_return(AstBuilder *ast, const ReturnNode *node) {
    if (node->value) {
        buffer_append_string(&ast->string, "<return> ");
        append_node(ast, node->value);
    } else {
        buffer_append_string(&ast->string, "<empty return> ");
    }
}

/** Appends a generic function node, which includes its name, parameters, and body. */
static void append_func(AstBuilder *ast, const FuncNode *node) {
    buffer_append_string(&ast->string, "<func> ");
    buffer_append_token(&ast->string, node->nameDecl->name);
    append_node_array(ast, &node->params);
    buffer_append_string(&ast->string, "-> ");
    append_node(ast, AS_NODE(node->body));
}

/** Appends a class node and all the information it has. */
static void append_class(AstBuilder *ast, const ClassNode *node) {
    buffer_append_string(&ast->string, "<class> ");
    buffer_append_token(&ast->string, node->nameDecl->name);

    append_node(ast, AS_NODE(node->init));
    append_node_array(ast, &node->methods);
}

/** Appends an EOF string to the AST string. */
static void append_eof(AstBuilder *ast) {
    buffer_append_string(&ast->string, "EOF ");
}

/** Calls the appropriate append function for the passed node. */
static void append_node(AstBuilder *ast, const Node *node) {
    if (node == NULL) {
        return;
    }
    if (node->type == AST_LITERAL) {
        // Literals don't get wrapped in parenthesis.
        append_literal_node(ast, AS_PTR(LiteralNode, node));
        return;
    }

    buffer_append_char(&ast->string, '(');
    switch (node->type) {
    case AST_LITERAL: UNREACHABLE_ERROR(); // Handled at the top of the function.
    case AST_ERROR: append_error(ast); break;
    case AST_STRING: append_string(ast, AS_PTR(StringNode, node)); break;
    case AST_KEYWORD: append_keyword(ast, AS_PTR(KeywordNode, node)); break;
    case AST_UNARY: append_unary(ast, AS_PTR(UnaryNode, node)); break;
    case AST_BINARY: append_binary(ast, AS_PTR(BinaryNode, node)); break;
    case AST_PARENTHESES: append_parentheses(ast, AS_PTR(ParenthesesNode, node)); break;
    case AST_RANGE: append_range(ast, AS_PTR(RangeNode, node)); break;
    case AST_CALL: append_call(ast, AS_PTR(CallNode, node)); break;
    case AST_ASSIGN_SUBSCR: append_assign_subscr(ast, AS_PTR(AssignSubscrNode, node)); break;
    case AST_GET_SUBSCR: append_get_subscr(ast, AS_PTR(GetSubscrNode, node)); break;
    case AST_TERNARY: append_ternary(ast, AS_PTR(TernaryNode, node)); break;
    case AST_LIST: append_list(ast, AS_PTR(ListNode, node)); break;
    case AST_MAP: append_map(ast, AS_PTR(MapNode, node)); break;
    case AST_EXPR_STMT: append_expr_stmt(ast, AS_PTR(ExprStmtNode, node)); break;
    case AST_BLOCK: append_block(ast, AS_PTR(BlockNode, node)); break;
    case AST_DECLARE_VAR: append_declare_var(ast, AS_PTR(DeclareVarNode, node)); break;
    case AST_ASSIGN_VAR: append_assign_var(ast, AS_PTR(AssignVarNode, node)); break;
    case AST_GET_VAR: append_get_var(ast, AS_PTR(GetVarNode, node)); break;
    case AST_SET_PROPERTY: append_set_property(ast, AS_PTR(SetPropertyNode, node)); break;
    case AST_GET_PROPERTY: append_get_property(ast, AS_PTR(GetPropertyNode, node)); break;
    case AST_MULTI_DECLARE: append_multi_declare(ast, AS_PTR(MultiDeclareNode, node)); break;
    case AST_MULTI_ASSIGN: append_multi_assign(ast, AS_PTR(MultiAssignNode, node)); break;
    case AST_IF_ELSE: append_if_else(ast, AS_PTR(IfElseNode, node)); break;
    case AST_TRY_CATCH: append_try_catch(ast, AS_PTR(TryCatchNode, node)); break;
    case AST_MATCH: append_match(ast, AS_PTR(MatchNode, node)); break;
    case AST_WHILE: append_while(ast, AS_PTR(WhileNode, node)); break;
    case AST_DO_WHILE: append_do_while(ast, AS_PTR(DoWhileNode, node)); break;
    case AST_FOR: append_for(ast, AS_PTR(ForNode, node)); break;
    case AST_LOOP_CONTROL: append_loop_control(ast, AS_PTR(LoopControlNode, node)); break;
    case AST_ENUM: append_enum(ast, AS_PTR(EnumNode, node)); break;
    case AST_RETURN: append_return(ast, AS_PTR(ReturnNode, node)); break;
    case AST_FUNC: append_func(ast, AS_PTR(FuncNode, node)); break;
    case AST_CLASS: append_class(ast, AS_PTR(ClassNode, node)); break;
    case AST_EOF: append_eof(ast); break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    // Ensures string never has whitespace before closing parenthesis.
    buffer_pop(&ast->string);
    buffer_append_string(&ast->string, ") ");
}

/** Appends the full array of passed nodes. */
static void append_node_array(AstBuilder *ast, const NodeArray *nodes) {
    if (nodes->length == 0) {
        buffer_append_string(&ast->string, "<empty array> ");
        return;
    }

    buffer_append_char(&ast->string, '(');
    buffer_append_string(&ast->string, "<array> ");
    for (u32 i = 0; i < nodes->length; i++) {
        if (i != 0) {
            buffer_pop(&ast->string);
            buffer_append_string(&ast->string, ", ");
        }
        append_node(ast, nodes->data[i]);
    }
    // Move space to after the closing parenthesis
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
