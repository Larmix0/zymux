#include "lukip.h"

#include "constants.h"
#include "node.c"

PRIVATE_TEST_CASE(test_new_node) {
    ZmxProgram program = create_zmx_program("no_file", false);
    BinaryNode *binary = NEW_NODE(&program, AST_BINARY, BinaryNode);

    ASSERT_INT_EQUAL(sizeof(*binary), sizeof(BinaryNode));
    ASSERT_TRUE(AS_PTR(binary, AstNode)->type == AST_BINARY);

    free_all_nodes(&program);
    free_zmx_program(&program);
}

void test_node() {
    TEST(test_new_node);
}
