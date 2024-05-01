#include "lukip.h"

#include "program.c"

/** Tests that zmx_power properly implements exponentiation. */
PRIVATE_TEST_CASE(test_zmx_power) {
    ASSERT_INT_EQUAL(zmx_power(7, 2), 49);
    ASSERT_INT_EQUAL(zmx_power(3, 7), 2187);
    ASSERT_INT_EQUAL(zmx_power(23, 1), 23);
    ASSERT_INT_EQUAL(zmx_power(9, 0), 1);
    ASSERT_INT_EQUAL(zmx_power(-4, 4), 256);
    ASSERT_INT_EQUAL(zmx_power(-6, 3), -216);
}

/** Tests that we can properly create an initialized Zymux program from create_zmx_program(). */
PRIVATE_TEST_CASE(test_create_zmx_program) {
    ZmxProgram program = create_zmx_program("zymux", false);
    ASSERT_STRING_EQUAL(program.currentFile, "zymux");
    ASSERT_FALSE(program.showErrors);
    free_zmx_program(&program);
}

/** Tests that all the allocation abstractions of Zymux work and allocate memory properly. */
PRIVATE_TEST_CASE(test_zmx_allocators) {
    int *singleInt = ZMX_TYPE_ALLOC(int);
    *singleInt = 2147483647;
    ASSERT_INT_EQUAL(*singleInt, 2147483647);

    i32 *single32 = ZMX_ALLOC(sizeof(i32));
    *single32 = 1844465920;
    ASSERT_INT_EQUAL(*single32, 1844465920);

    u8 *array = ZMX_ARRAY_ALLOC(3, u8);
    for (int i = 3; i < 6; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 3; i < 6; i++) {
        ASSERT_UINT8_EQUAL(array[i - 3], i * i);
    }

    array = ZMX_REALLOC_ARRAY(array, 5, sizeof(u8));
    for (int i = 6; i < 8; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 6; i < 8; i++) {
        ASSERT_UINT8_EQUAL(array[i - 3], i * i);
    }

    array = ZMX_REALLOC(array, 7);
    for (int i = 8; i < 10; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 8; i < 10; i++) {
        ASSERT_UINT8_EQUAL(array[i - 3], i * i);
    }
    free(singleInt);
    free(single32);
    free(array);
}

/** Tests zmx_program.c. */
void test_program() {
    TEST(test_zmx_power);
    TEST(test_create_zmx_program);
    TEST(test_zmx_allocators);
}
