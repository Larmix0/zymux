#include "lukip.h"

#include "object.c"

#define NEW_TEST_STRING(program, string) (new_string_obj(program, string, strlen(string)))

ZmxProgram *defaultProgram; /** Default program to have objects stored inside of during tests. */

PRIVATE_DECLARE_SETUP(setup_default_program) {
    defaultProgram = TYPE_ALLOC(ZmxProgram);
    *defaultProgram = create_zmx_program("no_file", false);
    GC_FREEZE(&defaultProgram->gc);
}

PRIVATE_DECLARE_TEARDOWN(teardown_default_program) {
    free_zmx_program(defaultProgram);
    free(defaultProgram);
}

PRIVATE_TEST_CASE(test_new_obj) {
    IntObj *integer = NEW_OBJ(defaultProgram, OBJ_INT, IntObj);
    integer->number = 0; // To avoid uninitialized errors when GC printing is on.
    ASSERT_INT_EQUAL(sizeof(*integer), sizeof(IntObj));
    ASSERT_TRUE(AS_OBJ(integer)->type == OBJ_INT);
}

PRIVATE_TEST_CASE(test_equal_obj) {
    IntObj *int1 = new_int_obj(defaultProgram, 55);
    IntObj *int2 = new_int_obj(defaultProgram, 55);
    ASSERT_TRUE(equal_obj(AS_OBJ(int1), AS_OBJ(int2)));

    IntObj *intNotEqual = new_int_obj(defaultProgram, 23);
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(intNotEqual)));

    FloatObj *float1 = new_float_obj(defaultProgram, 55.0);
    FloatObj *float2 = new_float_obj(defaultProgram, 55);
    FloatObj *floatNotEqual = new_float_obj(defaultProgram, 55.3);
    ASSERT_TRUE(equal_obj(AS_OBJ(float1), AS_OBJ(float2)));
    ASSERT_TRUE(equal_obj(AS_OBJ(int1), AS_OBJ(float1)));
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(intNotEqual)));
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(floatNotEqual)));
    ASSERT_FALSE(equal_obj(AS_OBJ(float1), AS_OBJ(floatNotEqual)));

    StringObj *name1 = NEW_TEST_STRING(defaultProgram, "name");
    StringObj *name2 = NEW_TEST_STRING(defaultProgram, "name");
    ASSERT_TRUE(equal_obj(AS_OBJ(name1), AS_OBJ(name2)));

    StringObj *numAsString = NEW_TEST_STRING(defaultProgram, "55");
    ASSERT_FALSE(equal_obj(AS_OBJ(name1), AS_OBJ(numAsString)));
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(numAsString)));

    FuncObj *func1 = new_func_obj(defaultProgram, name1, 0, 0, name1, true);
    ASSERT_TRUE(equal_obj(AS_OBJ(func1), AS_OBJ(func1)));

    FuncObj *func2 = new_func_obj(defaultProgram, name1, 0, 0, name1, true);
    ASSERT_FALSE(equal_obj(AS_OBJ(func1), AS_OBJ(func2))); // Must be the same address for equality.
}

PRIVATE_TEST_CASE(test_concatenate) {
    StringObj *start = NEW_TEST_STRING(defaultProgram, "start");
    StringObj *middle = NEW_TEST_STRING(defaultProgram, " middle");
    StringObj *end = NEW_TEST_STRING(defaultProgram, " end");

    StringObj *startMiddle = concatenate(defaultProgram, start, middle);
    StringObj *expectedStartMiddle = NEW_TEST_STRING(defaultProgram, "start middle");
    ASSERT_TRUE(equal_obj(AS_OBJ(startMiddle), AS_OBJ(expectedStartMiddle)));

    StringObj *full = concatenate(defaultProgram, startMiddle, end);
    StringObj *expectedFull = NEW_TEST_STRING(defaultProgram, "start middle end");
    ASSERT_TRUE(equal_obj(AS_OBJ(full), AS_OBJ(expectedFull)));
}

void test_object() {
    MAKE_FIXTURE(setup_default_program, teardown_default_program);
    TEST(test_new_obj);
    TEST(test_equal_obj);
    TEST(test_concatenate);
    RESET_FIXTURE();
}
