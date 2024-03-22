#include "lukip.h"

#include "zymux_program.c"

static void test_zmx_power() {
    LUKIP_INT_EQUAL(zmx_power(7, 2), 49);
    LUKIP_INT_EQUAL(zmx_power(3, 7), 2187);
    LUKIP_INT_EQUAL(zmx_power(23, 1), 23);
    LUKIP_INT_EQUAL(zmx_power(9, 0), 1);
    LUKIP_INT_EQUAL(zmx_power(-4, 4), 256);
    LUKIP_INT_EQUAL(zmx_power(-6, 3), -216);
}

static void test_create_zymux_program() {
    ZymuxProgram program = create_zymux_program("zymux", false);
    LUKIP_STRING_EQUAL(program.currentFile, "zymux");
    LUKIP_IS_FALSE(program.showErrors);
    free_zymux_program(&program);
}

static void test_zmx_allocators() {
    int *single = ZMX_ALLOC(int);
    *single = 2147483647;
    LUKIP_INT_EQUAL(*single, 2147483647);

    int *array = ZMX_ARRAY_ALLOC(3, int);
    for (int i = 3; i < 6; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 3; i < 6; i++) {
        LUKIP_INT_EQUAL(array[i - 3], i * i);
    }

    array = ZMX_REALLOC(array, 5, sizeof(int));
    for (int i = 6; i < 8; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 3; i < 6; i++) {
        LUKIP_INT_EQUAL(array[i - 3], i * i);
    }
    free(single);
    free(array);
}

void test_zymux_program() {
    TEST(test_zmx_power);
    TEST(test_create_zymux_program);
    TEST(test_zmx_allocators);
}
