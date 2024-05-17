#include "lukip.h"

#include "constants.h"
#include "token.h"

#include "node.c"

/** Tests that we can get the position of a node correctly. */
PRIVATE_TEST_CASE(test_get_node_pos) {
    ZmxProgram program = create_zmx_program("no_file", false);

    Node *leftLiteral = new_literal_node(&program, (Token){.pos = create_src_pos(3, 12, 4)});
    Node *rightLiteral = new_literal_node(&program, (Token){.pos = create_src_pos(1, 8, 2)});
    Node *binary = new_binary_node(
        &program, leftLiteral, (Token){.pos = create_src_pos(88, 99, 7)}, rightLiteral
    );
    Node *exprStmt = new_expr_stmt_node(&program, binary);
    Node *eof = new_eof_node(&program, create_src_pos(101, 102, 0));

    ASSERT_TRUE(equal_position(get_node_pos(leftLiteral), create_src_pos(3, 12, 4)));
    ASSERT_TRUE(equal_position(get_node_pos(rightLiteral), create_src_pos(1, 8, 2)));
    ASSERT_TRUE(equal_position(get_node_pos(binary), create_src_pos(88, 99, 7)));
    ASSERT_TRUE(equal_position(get_node_pos(exprStmt), create_src_pos(88, 99, 7)));
    ASSERT_TRUE(equal_position(get_node_pos(eof), create_src_pos(101, 102, 0)));

    free_all_nodes(&program);
    free_zmx_program(&program);
}

/** Tests that we can generally create new nodes correctly. */
PRIVATE_TEST_CASE(test_new_node) {
    ZmxProgram program = create_zmx_program("no_file", false);
    BinaryNode *binary = NEW_NODE(&program, AST_BINARY, BinaryNode);

    ASSERT_INT_EQUAL(sizeof(*binary), sizeof(BinaryNode));
    ASSERT_TRUE(AS_NODE(binary)->type == AST_BINARY);

    free_all_nodes(&program);
    free_zmx_program(&program);
}

/** Tests node.c. */
void test_node() {
    TEST(test_new_node);
    TEST(test_get_node_pos);
}
