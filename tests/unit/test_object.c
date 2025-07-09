#include "lukip.h"

#include "cli_handler.h"

#include "object.c"

#define NEW_TEST_STRING(vulnObjs, string) (new_string_obj(vulnObjs, string, strlen(string)))

VulnerableObjs *defaultVulnObjs; /** Default vulnerables for objects. */

PRIVATE_DECLARE_SETUP(setup_default_program) {
    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram *program = new_zmx_program("no_file", &cli, false);
    defaultVulnObjs = &program->gc.startupVulnObjs;
}

PRIVATE_DECLARE_TEARDOWN(teardown_default_program) {
    free_zmx_program(defaultVulnObjs->program);
}

PRIVATE_TEST_CASE(test_new_obj) {
    IntObj *integer = NEW_OBJ(defaultVulnObjs, OBJ_INT, IntObj);
    integer->number = 0; // To avoid uninitialized errors when GC printing is on.
    ASSERT_INT_EQUAL(sizeof(*integer), sizeof(IntObj));
    ASSERT_TRUE(AS_OBJ(integer)->type == OBJ_INT);
}

PRIVATE_TEST_CASE(test_equal_obj) {
    IntObj *int1 = new_int_obj(defaultVulnObjs, 55);
    IntObj *int2 = new_int_obj(defaultVulnObjs, 55);
    ASSERT_TRUE(equal_obj(AS_OBJ(int1), AS_OBJ(int2)));

    IntObj *intNotEqual = new_int_obj(defaultVulnObjs, 23);
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(intNotEqual)));

    FloatObj *float1 = new_float_obj(defaultVulnObjs, 55.0);
    FloatObj *float2 = new_float_obj(defaultVulnObjs, 55);
    FloatObj *floatNotEqual = new_float_obj(defaultVulnObjs, 55.3);
    ASSERT_TRUE(equal_obj(AS_OBJ(float1), AS_OBJ(float2)));
    ASSERT_TRUE(equal_obj(AS_OBJ(int1), AS_OBJ(float1)));
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(intNotEqual)));
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(floatNotEqual)));
    ASSERT_FALSE(equal_obj(AS_OBJ(float1), AS_OBJ(floatNotEqual)));

    StringObj *name1 = NEW_TEST_STRING(defaultVulnObjs, "name");
    StringObj *name2 = NEW_TEST_STRING(defaultVulnObjs, "name");
    ASSERT_TRUE(equal_obj(AS_OBJ(name1), AS_OBJ(name2)));

    StringObj *numAsString = NEW_TEST_STRING(defaultVulnObjs, "55");
    ASSERT_FALSE(equal_obj(AS_OBJ(name1), AS_OBJ(numAsString)));
    ASSERT_FALSE(equal_obj(AS_OBJ(int1), AS_OBJ(numAsString)));

    FuncObj *func1 = new_func_obj(defaultVulnObjs, name1, 0, 0, true);
    ASSERT_TRUE(equal_obj(AS_OBJ(func1), AS_OBJ(func1)));

    FuncObj *func2 = new_func_obj(defaultVulnObjs, name1, 0, 0, true);
    ASSERT_FALSE(equal_obj(AS_OBJ(func1), AS_OBJ(func2))); // Must be the same address for equality.
}

PRIVATE_TEST_CASE(test_concatenate) {
    StringObj *start = NEW_TEST_STRING(defaultVulnObjs, "start");
    StringObj *middle = NEW_TEST_STRING(defaultVulnObjs, " middle");
    StringObj *end = NEW_TEST_STRING(defaultVulnObjs, " end");

    StringObj *startMiddle = concatenate(defaultVulnObjs, start, middle);
    StringObj *expectedStartMiddle = NEW_TEST_STRING(defaultVulnObjs, "start middle");
    ASSERT_TRUE(equal_obj(AS_OBJ(startMiddle), AS_OBJ(expectedStartMiddle)));

    StringObj *full = concatenate(defaultVulnObjs, startMiddle, end);
    StringObj *expectedFull = NEW_TEST_STRING(defaultVulnObjs, "start middle end");
    ASSERT_TRUE(equal_obj(AS_OBJ(full), AS_OBJ(expectedFull)));
}

void test_object() {
    MAKE_FIXTURE(setup_default_program, teardown_default_program);
    TEST(test_new_obj);
    TEST(test_equal_obj);
    TEST(test_concatenate);
    RESET_FIXTURE();
}
