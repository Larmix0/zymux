#include "lukip.h"

#include "zymux_program.c"

/** Tests that zmx_power properly implements exponentiation. */
static void test_zmx_power() {
    LUKIP_INT_EQUAL(zmx_power(7, 2), 49);
    LUKIP_INT_EQUAL(zmx_power(3, 7), 2187);
    LUKIP_INT_EQUAL(zmx_power(23, 1), 23);
    LUKIP_INT_EQUAL(zmx_power(9, 0), 1);
    LUKIP_INT_EQUAL(zmx_power(-4, 4), 256);
    LUKIP_INT_EQUAL(zmx_power(-6, 3), -216);
}

/** Tests that we can properly create an initialized Zymux program from create_zymux_program(). */
static void test_create_zymux_program() {
    ZymuxProgram program = create_zymux_program("zymux", false);
    LUKIP_STRING_EQUAL(program.currentFile, "zymux");
    LUKIP_IS_FALSE(program.showErrors);
    free_zymux_program(&program);
}

/** Tests that all the allocation abstractions of Zymux work and allocate memory properly. */
static void test_zmx_allocators() {
    int *singleInt = ZMX_TYPE_ALLOC(int);
    *singleInt = 2147483647;
    LUKIP_INT_EQUAL(*singleInt, 2147483647);

    i32 *single32 = ZMX_ALLOC(sizeof(i32));
    *single32 = 1844465920;
    LUKIP_INT_EQUAL(*single32, 1844465920);

    u8 *array = ZMX_ARRAY_ALLOC(3, u8);
    for (int i = 3; i < 6; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 3; i < 6; i++) {
        LUKIP_UINT8_EQUAL(array[i - 3], i * i);
    }

    array = ZMX_REALLOC_ARRAY(array, 5, sizeof(u8));
    for (int i = 6; i < 8; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 6; i < 8; i++) {
        LUKIP_UINT8_EQUAL(array[i - 3], i * i);
    }

    array = ZMX_REALLOC(array, 7);
    for (int i = 8; i < 10; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 8; i < 10; i++) {
        LUKIP_UINT8_EQUAL(array[i - 3], i * i);
    }
    free(singleInt);
    free(single32);
    free(array);
}

/** Tests zymux_program.c. */
void test_zymux_program() {
    TEST(test_zmx_power);
    TEST(test_create_zymux_program);
    TEST(test_zmx_allocators);
}
