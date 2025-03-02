#include <stdlib.h>

#include "allocator.h"
#include "constants.h"
#include "node.h"
#include "resolver.h"

/** Allocates a new node of actualType. */
#define NEW_NODE(program, astType, actualType) \
    ((actualType *)new_node(program, astType, sizeof(actualType)))

/** Allocates a new node of the passed type. */
static Node *new_node(ZmxProgram *program, const AstType type, const size_t size) {
    Node *node = ALLOC(size);
    node->type = type;    

    node->next = program->allNodes;
    program->allNodes = node;
    return node;
}

/** Returns an erroneous node which serves as a placeholder for returning a valid token. */
Node *new_error_node(ZmxProgram *program) {
    ErrorNode *node = NEW_NODE(program, AST_ERROR, ErrorNode);
    return AS_NODE(node);
}

/** Allocates a new literal node which just holds the literal value. */
Node *new_literal_node(ZmxProgram *program, const Token value) {
    LiteralNode *node = NEW_NODE(program, AST_LITERAL, LiteralNode);
    node->value = value;
    return AS_NODE(node);
}

/** Returns a string node: An array of nodes that alternate between string literals and exprs. */
Node *new_string_node(ZmxProgram *program, const NodeArray exprs) {
    StringNode *node = NEW_NODE(program, AST_STRING, StringNode);
    node->exprs = exprs;
    return AS_NODE(node);
}

/** Returns a boolean of whether or not the string node starts with an literal. */
bool string_node_starts_with_literal(const StringNode *node) {
    Node *first = node->exprs.data[0];
    return first->type == AST_LITERAL && AS_PTR(LiteralNode, first)->value.type == TOKEN_STRING_LIT;
}

/** Returns a keyword node that is denoted by the token type. */
Node *new_keyword_node(ZmxProgram *program, const Token keyword) {
    KeywordNode *node = NEW_NODE(program, AST_KEYWORD, KeywordNode);
    node->keyword = keyword.type;
    node->pos = keyword.pos;
    return AS_NODE(node);
}

/** Allocates a unary node, which is something that has one operation done on it. */
Node *new_unary_node(ZmxProgram *program, const Token operation, Node *rhs) {
    UnaryNode *node = NEW_NODE(program, AST_UNARY, UnaryNode);
    node->operation = operation;
    node->rhs = rhs;
    return AS_NODE(node);
}

/** Allocates a binary node, which holds information of an operation done between 2 operands. */
Node *new_binary_node(ZmxProgram *program, Node *lhs, const Token operation, Node *rhs) {
    BinaryNode *node = NEW_NODE(program, AST_BINARY, BinaryNode);
    node->lhs = lhs;
    node->operation = operation;
    node->rhs = rhs;
    return AS_NODE(node);
}

/** Allocates a parentheses node which wraps an expression inside it to dictate precedence. */
Node *new_parentheses_node(ZmxProgram *program, Node *expr) {
    ParenthesesNode *node = NEW_NODE(program, AST_PARENTHESES, ParenthesesNode);
    node->expr = expr;
    return AS_NODE(node);
}

/** Allocates a number range node, which serves as an iterable over numbers. */
Node *new_range_node(
    ZmxProgram *program, Node *start, Node *end, Node *step, const SourcePosition pos
) {
    RangeNode *node = NEW_NODE(program, AST_RANGE, RangeNode);
    node->start = start;
    node->end = end;
    node->step = step;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates a call node, which simply wraps around a callee expression with some optional args. */
Node *new_call_node(ZmxProgram *program, Node *callee, const NodeArray args) {
    CallNode *node = NEW_NODE(program, AST_CALL, CallNode);
    node->callee = callee;
    node->args = args;
    return AS_NODE(node);
}

/** Allocates a ternary conditional expression node. */
Node *new_ternary_node(ZmxProgram *program, Node *condition, Node *trueExpr, Node *falseExpr) {
    TernaryNode *node = NEW_NODE(program, AST_TERNARY, TernaryNode);
    node->condition = condition;
    node->trueExpr = trueExpr;
    node->falseExpr = falseExpr;
    return AS_NODE(node);
}

/** Allocates an expression statement node, which just holds an expression. */
Node *new_expr_stmt_node(ZmxProgram *program, Node *expr) {
    ExprStmtNode *node = NEW_NODE(program, AST_EXPR_STMT, ExprStmtNode);
    node->expr = expr;
    return AS_NODE(node);
}

/** ALlocates a block that holds multiple statements/declarations. */
Node *new_block_node(ZmxProgram *program, const NodeArray stmts, const SourcePosition pos) {
    BlockNode *node = NEW_NODE(program, AST_BLOCK, BlockNode);
    node->stmts = stmts;
    node->pos = pos;
    node->localsAmount = UNRESOLVED_NUMBER;
    node->capturedAmount = UNRESOLVED_NUMBER;
    return AS_NODE(node);
}

/** 
 * Allocates a variable declaration node.
 * 
 * Variable delcaration is the first variable assignment which also includes constness.
 * It doesn't include whether or not it is private as private is a node that wraps around names.
 */
Node *new_var_decl_node(ZmxProgram *program, const Token name, Node *value, const bool isConst) {
    VarDeclNode *node = NEW_NODE(program, AST_VAR_DECL, VarDeclNode);
    node->name = name;
    node->value = value;
    node->isConst = isConst;
    node->resolution.index = UNRESOLVED_NUMBER;
    node->resolution.scope = UNRESOLVED_NAME_SCOPE;
    return AS_NODE(node);
}

/** Allocates an assignment expression, which changes the value of a variable already set. */
Node *new_var_assign_node(ZmxProgram *program, const Token name, Node *value) {
    VarAssignNode *node = NEW_NODE(program, AST_VAR_ASSIGN, VarAssignNode);
    node->name = name;
    node->value = value;
    node->resolution.index = UNRESOLVED_NUMBER;
    node->resolution.scope = UNRESOLVED_NAME_SCOPE;
    return AS_NODE(node);
}

/** Allocates a node which holds the name of a variable to get its value. */
Node *new_var_get_node(ZmxProgram *program, const Token name) {
    VarGetNode *node = NEW_NODE(program, AST_VAR_GET, VarGetNode);
    node->name = name;
    node->resolution.index = UNRESOLVED_NUMBER;
    node->resolution.scope = UNRESOLVED_NAME_SCOPE;
    return AS_NODE(node);
}


/** Allocates an if-else statement with their condition. The else branch can optionally be NULL. */
Node *new_if_else_node(
    ZmxProgram *program, Node *condition, BlockNode *ifBranch, Node *elseBranch
) {
    IfElseNode *node = NEW_NODE(program, AST_IF_ELSE, IfElseNode);
    node->condition = condition;
    node->ifBranch = ifBranch;
    node->elseBranch = elseBranch;
    return AS_NODE(node);
}

/** Allocates a while loop statement. */
Node *new_while_node(ZmxProgram *program, Node *condition, BlockNode *body) {
    WhileNode *node = NEW_NODE(program, AST_WHILE, WhileNode);
    node->condition = condition;
    node->body = body;
    return AS_NODE(node);
}

/** Allocates a do while loop node. */
Node *new_do_while_node(ZmxProgram *program, Node *condition, BlockNode *body) {
    DoWhileNode *node = NEW_NODE(program, AST_DO_WHILE, DoWhileNode);
    node->condition = condition;
    node->body = body;
    return AS_NODE(node);
}

/** Allocates a for loop node, which is a for-in loop for Zymux. */
Node *new_for_node(ZmxProgram *program, VarDeclNode *loopVar, Node *iterable, BlockNode *body) {
    ForNode *node = NEW_NODE(program, AST_FOR, ForNode);
    node->loopVar = loopVar;
    node->iterable = iterable;
    node->body = body;
    return AS_NODE(node);
}

/** Allocates a node for some loop control statement (break or continue). */
Node *new_loop_control_node(ZmxProgram *program, const Token keyword) {
    LoopControlNode *node = NEW_NODE(program, AST_LOOP_CONTROL, LoopControlNode);
    node->keyword = keyword.type;
    node->pos = keyword.pos;

    node->localsAmount = UNRESOLVED_NUMBER;
    node->capturedAmount = UNRESOLVED_NUMBER;
    return AS_NODE(node);
}

/** Allocates a general node for any type of function written from the user. */
Node *new_func_node(
    ZmxProgram *program, VarDeclNode *nameDecl, const NodeArray params, BlockNode *body
) {
    FuncNode *node = NEW_NODE(program, AST_FUNC, FuncNode);
    node->nameDecl = nameDecl;
    node->params = params;
    node->body = body;

    node->isClosure = false;
    INIT_DA(&node->capturedParams);
    return AS_NODE(node);
}

/** Allocates a return node, which exits a functino with a specific object/value. */
Node *new_return_node(ZmxProgram *program, Node *returnValue) {
    ReturnNode *node = NEW_NODE(program, AST_RETURN, ReturnNode);
    node->returnValue = returnValue;
    node->capturedPops = UNRESOLVED_NUMBER;
    return AS_NODE(node);
}

/** Returns a node which holds the position an EOF token. */
Node *new_eof_node(ZmxProgram *program, const SourcePosition eofPos) {
    EofNode *node = NEW_NODE(program, AST_EOF, EofNode);
    node->pos = eofPos;
    return AS_NODE(node);
}

/** 
 * Returns the source position of an arbitrary node.
 * 
 * This function becomes recursive when getting the lines of a node that doesn't directly have a
 * token/position inside it.
 */
SourcePosition get_node_pos(const Node *node) {
    switch (node->type) {
    case AST_ERROR: return create_src_pos(0, 0, 0);
    case AST_LITERAL: return AS_PTR(LiteralNode, node)->value.pos;
    case AST_KEYWORD: return AS_PTR(KeywordNode, node)->pos;
    case AST_UNARY: return AS_PTR(UnaryNode, node)->operation.pos;
    case AST_BINARY: return AS_PTR(BinaryNode, node)->operation.pos;

    // Can safely index 0 here since a string node is gauranteed to at least have one empty string.
    case AST_STRING: return get_node_pos(AS_PTR(StringNode, node)->exprs.data[0]);
    case AST_PARENTHESES: return get_node_pos(AS_PTR(ParenthesesNode, node)->expr);
    case AST_RANGE: return AS_PTR(RangeNode, node)->pos;
    case AST_CALL: return get_node_pos(AS_PTR(CallNode, node)->callee);
    case AST_TERNARY: return get_node_pos(AS_PTR(TernaryNode, node)->condition);
    case AST_EXPR_STMT: return get_node_pos(AS_PTR(ExprStmtNode, node)->expr);
    case AST_BLOCK: return AS_PTR(BlockNode, node)->pos;
    case AST_VAR_DECL: return AS_PTR(VarDeclNode, node)->name.pos;
    case AST_VAR_ASSIGN: return AS_PTR(VarAssignNode, node)->name.pos;
    case AST_VAR_GET: return AS_PTR(VarGetNode, node)->name.pos;
    case AST_IF_ELSE: return get_node_pos(AS_PTR(IfElseNode, node)->condition);
    case AST_WHILE: return get_node_pos(AS_PTR(WhileNode, node)->condition);
    case AST_DO_WHILE: return AS_PTR(DoWhileNode, node)->body->pos;
    case AST_FOR: return AS_PTR(ForNode, node)->loopVar->name.pos;
    case AST_LOOP_CONTROL: return AS_PTR(LoopControlNode, node)->pos;
    case AST_FUNC: return AS_PTR(FuncNode, node)->nameDecl->name.pos;
    case AST_RETURN: return get_node_pos(AS_PTR(ReturnNode, node)->returnValue);
    case AST_EOF: return AS_PTR(EofNode, node)->pos;
    }
    UNREACHABLE_ERROR();
}

/** Frees the passed node and its allocated members. */
static void free_node(Node *node) {
    switch (node->type) {
    case AST_STRING:
        FREE_DA(&AS_PTR(StringNode, node)->exprs);
        break;
    case AST_BLOCK:
        FREE_DA(&AS_PTR(BlockNode, node)->stmts);
        break;
    case AST_CALL:
        FREE_DA(&AS_PTR(CallNode, node)->args);
        break;
    case AST_FUNC:
        FREE_DA(&AS_PTR(FuncNode, node)->params);
        FREE_DA(&AS_PTR(FuncNode, node)->capturedParams);
        break;
    case AST_ERROR:
    case AST_LITERAL:
    case AST_KEYWORD:
    case AST_UNARY:
    case AST_BINARY:
    case AST_PARENTHESES:
    case AST_RANGE:
    case AST_TERNARY:
    case AST_EXPR_STMT:
    case AST_VAR_DECL:
    case AST_VAR_ASSIGN:
    case AST_VAR_GET:
    case AST_IF_ELSE:
    case AST_WHILE:
    case AST_DO_WHILE:
    case AST_FOR:
    case AST_LOOP_CONTROL:
    case AST_RETURN:
    case AST_EOF:
        break; // Nothing to free.
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    free(node);
}

// TODO: change to traversing the AST so we can remove "next" pointer from nodes for optimization.
/** Frees all nodes that were allocated and placed inside the passed program. */
void free_all_nodes(ZmxProgram *program) {
    if (program->allNodes == NULL) {
        return;
    }

    Node *current = program->allNodes;
    Node *next = current->next;
    while (current != NULL) {
        free_node(current);
        current = next;
        if (next != NULL) {
            next = next->next;
        }
    }
}
