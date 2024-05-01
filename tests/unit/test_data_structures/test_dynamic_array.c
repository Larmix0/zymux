#include "lukip.h"

#include "data_structures.h"

/** An integer array purely made to test our generic dynamic array implementation. */
DECLARE_DA_STRUCT(IntArray, int);

/** Tests that dynamic arrays properly pop and drop elements. */
PRIVATE_TEST_CASE(test_pop_and_drop) {
    IntArray integers = CREATE_DA();
    APPEND_DA(&integers, 453);
    APPEND_DA(&integers, 0);
    APPEND_DA(&integers, -945832);
    APPEND_DA(&integers, 4857393);
    ASSERT_INT_EQUAL(integers.length, 4);

    int last = POP_DA(&integers);
    DROP_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 2);
    ASSERT_INT_EQUAL(last, 4857393);

    int second = POP_DA(&integers);
    DROP_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(second, 0);

    APPEND_DA(&integers, 888);
    ASSERT_INT_EQUAL(integers.data[0], 888);
    ASSERT_INT_EQUAL(integers.length, 1);

    int appendedAfter = POP_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(appendedAfter, 888);

    FREE_DA(&integers);
}

/** Tests that dynamic arrays properly append elements. */
PRIVATE_TEST_CASE(test_append) {
    IntArray integers = CREATE_DA();
    APPEND_DA(&integers, 22);
    APPEND_DA(&integers, 0);
    APPEND_DA(&integers, -23821593);
    ASSERT_INT_EQUAL(integers.length, 3);

    ASSERT_INT_EQUAL(integers.data[0], 22);
    ASSERT_INT_EQUAL(integers.data[1], 0);
    ASSERT_INT_EQUAL(integers.data[2], -23821593);

    FREE_DA(&integers);
}

/** Tests that we can create and initialize dynamic arrays properly */
PRIVATE_TEST_CASE(test_creation_macros) {
    IntArray integers = CREATE_DA();
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(integers.capacity, 0);
    ASSERT_NULL(integers.data);

    integers.length += 2;
    integers.capacity += 3;
    integers.data = ZMX_ARRAY_ALLOC(3, int);
    int *arrayPtr = integers.data; // So we can still free the array after re-initializing.

    INIT_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(integers.capacity, 0);
    ASSERT_NULL(integers.data);
    free(arrayPtr);
}

/** Tests our generic dynamic arrays implementation. */
void test_dynamic_array() {
    TEST(test_creation_macros);
    TEST(test_append);
    TEST(test_pop_and_drop);
}
