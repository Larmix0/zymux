#include "lukip.h"

#include "data_structures.h"

/** An integer array purely made to test our generic dynamic array implementation. */
DECLARE_DA_STRUCT(IntArray, int);

/** Tests that dynamic arrays properly pop and drop elements. */
static void test_pop_and_drop() {
    IntArray integers = CREATE_DA();
    APPEND_DA(&integers, 453);
    APPEND_DA(&integers, 0);
    APPEND_DA(&integers, -945832);
    APPEND_DA(&integers, 4857393);
    LUKIP_INT_EQUAL(integers.length, 4);

    int last = POP_DA(&integers);
    DROP_DA(&integers);
    LUKIP_INT_EQUAL(integers.length, 2);
    LUKIP_INT_EQUAL(last, 4857393);

    int second = POP_DA(&integers);
    DROP_DA(&integers);
    LUKIP_INT_EQUAL(integers.length, 0);
    LUKIP_INT_EQUAL(second, 0);

    APPEND_DA(&integers, 888);
    LUKIP_INT_EQUAL(integers.data[0], 888);
    LUKIP_INT_EQUAL(integers.length, 1);

    int appendedAfter = POP_DA(&integers);
    LUKIP_INT_EQUAL(integers.length, 0);
    LUKIP_INT_EQUAL(appendedAfter, 888);

    FREE_DA(&integers);
}

/** Tests that dynamic arrays properly append elements. */
static void test_append() {
    IntArray integers = CREATE_DA();
    APPEND_DA(&integers, 22);
    APPEND_DA(&integers, 0);
    APPEND_DA(&integers, -23821593);
    LUKIP_INT_EQUAL(integers.length, 3);

    LUKIP_INT_EQUAL(integers.data[0], 22);
    LUKIP_INT_EQUAL(integers.data[1], 0);
    LUKIP_INT_EQUAL(integers.data[2], -23821593);

    FREE_DA(&integers);
}

/** Tests that we can create and initialize dynamic arrays properly */
static void test_creation_macros() {
    IntArray integers = CREATE_DA();
    LUKIP_INT_EQUAL(integers.length, 0);
    LUKIP_INT_EQUAL(integers.capacity, 0);
    LUKIP_IS_NULL(integers.data);

    integers.length += 2;
    integers.capacity += 3;
    integers.data = ZMX_ARRAY_ALLOC(3, int);
    int *arrayPtr = integers.data; // So we can still free the array after re-initializing.

    INIT_DA(&integers);
    LUKIP_INT_EQUAL(integers.length, 0);
    LUKIP_INT_EQUAL(integers.capacity, 0);
    LUKIP_IS_NULL(integers.data);
    free(arrayPtr);
}

/** Tests our generic dynamic arrays implementation. */
void test_dynamic_array() {
    TEST(test_creation_macros);
    TEST(test_append);
    TEST(test_pop_and_drop);
}
