#include <stdlib.h>

#include "allocator.h"
#include "constants.h"
#include "node.h"
#include "resolver.h"

/** Allocates and returns a node. astType is the enum type, cType is the C struct itself. */
#define NEW_NODE(program, astType, cType) \
    ((cType *)new_node(program, astType, sizeof(cType)))

/** Returns a default empty variable resolution struct. */
static VarResolution unresolved_resolution() {
    VarResolution resolution = {.index = UNRESOLVED_NUMBER, .scope = UNRESOLVED_NAME_SCOPE};
    return resolution;
}

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
Node *new_string_node(ZmxProgram *program, const NodeArray exprs, const SourcePosition pos) {
    StringNode *node = NEW_NODE(program, AST_STRING, StringNode);
    node->exprs = exprs;
    node->pos = pos;
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

/** Allocates a subscript assignment, which assigns to some subscripted thing. */
Node *new_assign_subscr_node(ZmxProgram *program, Node *callee, Node *subscript, Node *value) {
    AssignSubscrNode *node = NEW_NODE(program, AST_ASSIGN_SUBSCR, AssignSubscrNode);
    node->callee = callee;
    node->subscript = subscript;
    node->value = value;
    return AS_NODE(node);
}

/** Allocates a subscript get, which grabs a part of the thing it's being used on. */
Node *new_get_subscr_node(ZmxProgram *program, Node *callee, Node *subscript) {
    GetSubscrNode *node = NEW_NODE(program, AST_GET_SUBSCR, GetSubscrNode);
    node->callee = callee;
    node->subscript = subscript;
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

/** Allocates a list node to hold an array of multiple nodes. */
Node *new_list_node(ZmxProgram *program, const NodeArray items, const SourcePosition pos) {
    ListNode *node = NEW_NODE(program, AST_LIST, ListNode);
    node->items = items;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates a map of key-value pairs represented in 2 equally long arrays. */
Node *new_map_node(
    ZmxProgram *program, const NodeArray keys, const NodeArray values, const SourcePosition pos
) {
    ASSERT(keys.length == values.length, "Map must have an equal number for keys and values.");

    MapNode *node = NEW_NODE(program, AST_MAP, MapNode);
    node->keys = keys;
    node->values = values;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates a call node, which calls a callee with some positional, then keyword args. */
Node *new_call_node(
    ZmxProgram *program, Node *callee, const NodeArray positionalArgs, MapNode *keywordArgs
) {
    CallNode *node = NEW_NODE(program, AST_CALL, CallNode);
    node->callee = callee;
    node->positionalArgs = positionalArgs;
    node->keywordArgs = keywordArgs;
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
Node *new_declare_var_node(ZmxProgram *program, const Token name, Node *value, const bool isConst) {
    DeclareVarNode *node = NEW_NODE(program, AST_DECLARE_VAR, DeclareVarNode);
    node->name = name;
    node->value = value;
    node->isConst = isConst;

    node->resolution = unresolved_resolution();
    return AS_NODE(node);
}

/** Allocates an assignment expression, which changes the value of a variable already set. */
Node *new_assign_var_node(ZmxProgram *program, const Token name, Node *value) {
    AssignVarNode *node = NEW_NODE(program, AST_ASSIGN_VAR, AssignVarNode);
    node->name = name;
    node->value = value;

    node->resolution = unresolved_resolution();
    return AS_NODE(node);
}

/** Allocates a node which holds the name of a variable to get its value. */
Node *new_get_var_node(ZmxProgram *program, const Token name) {
    GetVarNode *node = NEW_NODE(program, AST_GET_VAR, GetVarNode);
    node->name = name;

    node->resolution = unresolved_resolution();
    return AS_NODE(node);
}

/** Allocates a node which declares/assigns a property in some object. */
Node *new_set_property_node(ZmxProgram *program, GetPropertyNode *get, Node *value) {
    SetPropertyNode *node = NEW_NODE(program, AST_SET_PROPERTY, SetPropertyNode);
    node->get = get;
    node->value = value;
    return AS_NODE(node);
}

/** Allocates a node which holds a property access into some object. */
Node *new_get_property_node(ZmxProgram *program, const Token property, Node *originalObj) {
    GetPropertyNode *node = NEW_NODE(program, AST_GET_PROPERTY, GetPropertyNode);
    node->property = property;
    node->originalObj = originalObj;
    return AS_NODE(node);
}

/** Returns a "super" keyword get with a property that's gotten from the superclass. */
Node *new_get_super_node(
    ZmxProgram *program, const Token property, GetVarNode *instanceGet, const SourcePosition pos
) {
    GetSuperNode *node = NEW_NODE(program, AST_GET_SUPER, GetSuperNode);
    node->property = property;
    node->instanceGet = instanceGet;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates a node for declaring multiple variables in one declarative statement. */
Node *new_multi_declare_node(
    ZmxProgram *program, const NodeArray declarations, Node *value,
    const bool isConst, const SourcePosition pos
) {
    MultiDeclareNode *node = NEW_NODE(program, AST_MULTI_DECLARE, MultiDeclareNode);
    node->declarations = declarations;
    node->value = value;
    node->isConst = isConst;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates a node for multiple assignments on an iterable value. */
Node *new_multi_assign_node(
    ZmxProgram *program, const NodeArray assignments, Node *value, const SourcePosition pos
) {
    MultiAssignNode *node = NEW_NODE(program, AST_MULTI_ASSIGN, MultiAssignNode);
    node->assignments = assignments;
    node->value = value;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates an if-else statement with their condition. The else branch can optionally be NULL. */
Node *new_if_else_node(ZmxProgram *program, Node *condition, BlockNode *ifBlock, Node *elseBlock) {
    IfElseNode *node = NEW_NODE(program, AST_IF_ELSE, IfElseNode);
    node->condition = condition;
    node->ifBlock = ifBlock;
    node->elseBlock = elseBlock;
    return AS_NODE(node);
}

/** Allocates a statement which doesn't immediately exit and print a message on error. */
Node *new_try_catch_node(
    ZmxProgram *program, BlockNode *tryBlock, BlockNode *catchBlock, DeclareVarNode *catchVar
) {
    TryCatchNode *node = NEW_NODE(program, AST_TRY_CATCH, TryCatchNode);
    node->tryBlock = tryBlock;
    node->catchBlock = catchBlock;
    node->catchVar = catchVar;
    return AS_NODE(node);
}

/** Allocates a node which will try to raise an error at runtime. */
Node *new_raise_node(ZmxProgram *program, Node *message, const SourcePosition pos) {
    RaiseNode *node = NEW_NODE(program, AST_RAISE, RaiseNode);
    node->message = message;
    node->pos = pos;
    return AS_NODE(node);    
}

/** Allocates one new case for some match statement. */
Node *new_case_node(
    ZmxProgram *program, const NodeArray labelVals, BlockNode *block, const SourcePosition pos
) {
    CaseNode *node = NEW_NODE(program, AST_CASE, CaseNode);
    node->labelVals = labelVals;
    node->block = block;
    node->pos = pos;
    return AS_NODE(node);
}

/** Allocates a new match-case statement, with an optional default case. */
Node *new_match_node(
    ZmxProgram *program, Node *matchedExpr, const NodeArray cases, BlockNode *defaultBlock
) {
    MatchNode *node = NEW_NODE(program, AST_MATCH, MatchNode);
    node->matchedExpr = matchedExpr;
    node->cases = cases;
    node->defaultBlock = defaultBlock;
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
Node *new_for_node(ZmxProgram *program, Node *loopVar, Node *iterable, BlockNode *body) {
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

/** Allocates an enum with members that represent a text/readable number. */
Node *new_enum_node(ZmxProgram *program, DeclareVarNode *nameDecl, const TokenArray members) {
    EnumNode *node = NEW_NODE(program, AST_ENUM, EnumNode);
    node->nameDecl = nameDecl;
    node->members = members;
    return AS_NODE(node);
}

/** Allocates a return node, which exits a function with a specific object/value. */
Node *new_return_node(ZmxProgram *program, Node *value, const SourcePosition pos) {
    ReturnNode *node = NEW_NODE(program, AST_RETURN, ReturnNode);
    node->value = value;
    node->pos = pos;

    node->defaultVal = NULL;
    node->capturedPops = UNRESOLVED_NUMBER;
    return AS_NODE(node);
}

/** Allocates a general node for any type of function written from the user. */
Node *new_func_node(
    ZmxProgram *program, DeclareVarNode *nameDecl, const NodeArray mandatoryParams,
    const NodeArray optionalParams, BlockNode *body
) {
    FuncNode *node = NEW_NODE(program, AST_FUNC, FuncNode);
    node->nameDecl = nameDecl;
    node->mandatoryParams = mandatoryParams;
    node->optionalParams = optionalParams;
    node->body = body;

    node->defaultReturn = AS_PTR(
        ReturnNode, new_return_node(program, NULL, get_node_pos(AS_NODE(body)))
    );
    node->isClosure = false;
    INIT_DA(&node->capturedParams);
    return AS_NODE(node);
}

/** 
 * Allocates a node which represents a class and all of its information.
 * 
 * Note that this only takes information before the body of the class as arguments,
 * while the information inside the body should be added to the node whilst parsing for convenience
 * (and not having to create too many variables).
 */
Node *new_class_node(ZmxProgram *program, DeclareVarNode *nameDecl, const bool isAbstract) {
    ClassNode *node = NEW_NODE(program, AST_CLASS, ClassNode);
    node->nameDecl = nameDecl;
    node->isAbstract = isAbstract;

    node->superclass = NULL;
    node->init = NULL;

    INIT_DA(&node->methods);
    INIT_DA(&node->abstractMethods);
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
    case AST_STRING: return AS_PTR(StringNode, node)->pos;
    case AST_PARENTHESES: return get_node_pos(AS_PTR(ParenthesesNode, node)->expr);
    case AST_RANGE: return AS_PTR(RangeNode, node)->pos;
    case AST_ASSIGN_SUBSCR: return get_node_pos(AS_PTR(AssignSubscrNode, node)->callee);
    case AST_GET_SUBSCR: return get_node_pos(AS_PTR(GetSubscrNode, node)->callee);
    case AST_TERNARY: return get_node_pos(AS_PTR(TernaryNode, node)->condition);
    case AST_LIST: return AS_PTR(ListNode, node)->pos;
    case AST_MAP: return AS_PTR(MapNode, node)->pos;
    case AST_CALL: return get_node_pos(AS_PTR(CallNode, node)->callee);
    case AST_EXPR_STMT: return get_node_pos(AS_PTR(ExprStmtNode, node)->expr);
    case AST_BLOCK: return AS_PTR(BlockNode, node)->pos;
    case AST_DECLARE_VAR: return AS_PTR(DeclareVarNode, node)->name.pos;
    case AST_ASSIGN_VAR: return AS_PTR(AssignVarNode, node)->name.pos;
    case AST_GET_VAR: return AS_PTR(GetVarNode, node)->name.pos;
    case AST_SET_PROPERTY: return get_node_pos(AS_NODE(AS_PTR(SetPropertyNode, node)->get));
    case AST_GET_PROPERTY: return AS_PTR(GetPropertyNode, node)->property.pos;
    case AST_GET_SUPER: return AS_PTR(GetSuperNode, node)->pos;
    case AST_MULTI_DECLARE: return AS_PTR(MultiDeclareNode, node)->pos;
    case AST_MULTI_ASSIGN: return AS_PTR(MultiAssignNode, node)->pos;
    case AST_IF_ELSE: return get_node_pos(AS_PTR(IfElseNode, node)->condition);
    case AST_TRY_CATCH: return get_node_pos(AS_NODE(AS_PTR(TryCatchNode, node)->tryBlock));
    case AST_RAISE: return AS_PTR(RaiseNode, node)->pos;
    case AST_CASE: return AS_PTR(CaseNode, node)->pos;
    case AST_MATCH: return get_node_pos(AS_PTR(MatchNode, node)->matchedExpr);
    case AST_WHILE: return get_node_pos(AS_PTR(WhileNode, node)->condition);
    case AST_DO_WHILE: return AS_PTR(DoWhileNode, node)->body->pos;
    case AST_FOR: return get_node_pos(AS_PTR(ForNode, node)->loopVar);
    case AST_LOOP_CONTROL: return AS_PTR(LoopControlNode, node)->pos;
    case AST_ENUM: return AS_PTR(EnumNode, node)->nameDecl->name.pos;
    case AST_RETURN: return AS_PTR(ReturnNode, node)->pos;
    case AST_FUNC: return AS_PTR(FuncNode, node)->nameDecl->name.pos;
    case AST_CLASS: return AS_PTR(ClassNode, node)->nameDecl->name.pos;
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
    case AST_LIST:
        FREE_DA(&AS_PTR(ListNode, node)->items);
        break;
    case AST_MAP:
        FREE_DA(&AS_PTR(MapNode, node)->keys);
        FREE_DA(&AS_PTR(MapNode, node)->values);
        break;
    case AST_CALL:
        FREE_DA(&AS_PTR(CallNode, node)->positionalArgs);
        break;
    case AST_BLOCK:
        FREE_DA(&AS_PTR(BlockNode, node)->stmts);
        break;
    case AST_MULTI_DECLARE:
        FREE_DA(&AS_PTR(MultiDeclareNode, node)->declarations);
        break;
    case AST_MULTI_ASSIGN:
        FREE_DA(&AS_PTR(MultiAssignNode, node)->assignments);
        break;
    case AST_CASE:
        FREE_DA(&AS_PTR(CaseNode, node)->labelVals);
        break;
    case AST_MATCH:
        FREE_DA(&AS_PTR(MatchNode, node)->cases);
        break;
    case AST_ENUM:
        FREE_DA(&AS_PTR(EnumNode, node)->members);
        break;
    case AST_FUNC:
        FREE_DA(&AS_PTR(FuncNode, node)->mandatoryParams);
        FREE_DA(&AS_PTR(FuncNode, node)->optionalParams);
        FREE_DA(&AS_PTR(FuncNode, node)->capturedParams);
        break;
    case AST_CLASS:
        FREE_DA(&AS_PTR(ClassNode, node)->methods);
        FREE_DA(&AS_PTR(ClassNode, node)->abstractMethods);
        break;
    case AST_ERROR:
    case AST_LITERAL:
    case AST_KEYWORD:
    case AST_UNARY:
    case AST_BINARY:
    case AST_PARENTHESES:
    case AST_RANGE:
    case AST_ASSIGN_SUBSCR:
    case AST_GET_SUBSCR:
    case AST_TERNARY:
    case AST_EXPR_STMT:
    case AST_DECLARE_VAR:
    case AST_ASSIGN_VAR:
    case AST_GET_VAR:
    case AST_SET_PROPERTY:
    case AST_GET_PROPERTY:
    case AST_GET_SUPER:
    case AST_IF_ELSE:
    case AST_TRY_CATCH:
    case AST_RAISE:
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
    while (current) {
        free_node(current);
        current = next;
        if (next) {
            next = next->next;
        }
    }
}
