#include "lukip.h"

#include "object.c"

PRIVATE_TEST_CASE(test_new_obj) {
    ZmxProgram program = create_zmx_program("no_file", false);
    IntObj *integer = NEW_OBJ(&program, OBJ_INT, IntObj);

    ASSERT_INT_EQUAL(sizeof(*integer), sizeof(IntObj));
    ASSERT_TRUE(AS_OBJ(integer)->type == OBJ_INT);

    free_zmx_program(&program); // Frees the objects too.
}

void test_object() {
    TEST(test_new_obj);
}
