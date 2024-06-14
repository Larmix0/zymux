#include "lukip.h"

#include "object.c"

ZmxProgram *defaultProgram; /** Default program to have objects stored inside of during tests. */

PRIVATE_DECLARE_SETUP(setup_default_program) {
    defaultProgram = TYPE_ALLOC(ZmxProgram);
    *defaultProgram = create_zmx_program("no_file", false);
}

PRIVATE_DECLARE_TEARDOWN(teardown_default_program) {
    free_zmx_program(defaultProgram);
    free(defaultProgram);
}

PRIVATE_TEST_CASE(test_new_obj) {
    IntObj *integer = NEW_OBJ(defaultProgram, OBJ_INT, IntObj);
    ASSERT_INT_EQUAL(sizeof(*integer), sizeof(IntObj));
    ASSERT_TRUE(AS_OBJ(integer)->type == OBJ_INT);
}

PRIVATE_TEST_CASE(test_equal_obj) {
    IntObj *int1 = new_int_obj(defaultProgram, 55);
    IntObj *int2 = new_int_obj(defaultProgram, 55);
    ASSERT_TRUE(equal_obj(AS_OBJ(int1), AS_OBJ(int2)));

    IntObj *intNotEqual = new_int_obj(defaultProgram, 23);
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(intNotEqual)));

    StringObj *name1 = new_string_obj(defaultProgram, "name");
    StringObj *name2 = new_string_obj(defaultProgram, "name");
    ASSERT_TRUE(equal_obj(AS_OBJ(name1), AS_OBJ(name2)));

    StringObj *differentName = new_string_obj(defaultProgram, "different");
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(intNotEqual)));

    FuncObj *func1 = new_func_obj(defaultProgram, name1, 0);
    ASSERT_TRUE(equal_obj(AS_OBJ(func1), AS_OBJ(func1)));

    FuncObj *func2 = new_func_obj(defaultProgram, name1, 0);
    ASSERT_FALSE(equal_obj(AS_OBJ(func1), AS_OBJ(func2))); // Must be the same address for equality.
}

void test_object() {
    MAKE_FIXTURE(setup_default_program, teardown_default_program);
    TEST(test_new_obj);
    TEST(test_equal_obj);
    RESET_FIXTURE();
}
