#include "lukip.h"

#include "constants.h"
#include "dynamic_array.h"

/** Tests that dynamic arrays properly pop and drop elements. */
PRIVATE_TEST_CASE(test_pop_and_drop) {
    IntArray integers = CREATE_DA();
    PUSH_DA(&integers, 453);
    PUSH_DA(&integers, 0);
    PUSH_DA(&integers, -945832);
    PUSH_DA(&integers, 4857393);
    ASSERT_INT_EQUAL(integers.length, 4);

    int last = POP_DA(&integers);
    DROP_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 2);
    ASSERT_INT_EQUAL(last, 4857393);

    int second = POP_DA(&integers);
    DROP_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(second, 0);

    PUSH_DA(&integers, 5);
    PUSH_DA(&integers, 4);
    PUSH_DA(&integers, 3);
    PUSH_DA(&integers, 2);
    DROP_AMOUNT_DA(&integers, 3);
    ASSERT_INT_EQUAL(integers.length, 1);
    ASSERT_INT_EQUAL(integers.data[0], 5);

    DROP_DA(&integers);
    PUSH_DA(&integers, 888);
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
    PUSH_DA(&integers, 22);
    PUSH_DA(&integers, 0);
    PUSH_DA(&integers, -23821593);
    ASSERT_INT_EQUAL(integers.length, 3);

    ASSERT_INT_EQUAL(integers.data[0], 22);
    ASSERT_INT_EQUAL(integers.data[1], 0);
    ASSERT_INT_EQUAL(integers.data[2], -23821593);

    FREE_DA(&integers);
}

/** Tests that we can create and initialize dynamic arrays properly. */
PRIVATE_TEST_CASE(test_creation_macros) {
    IntArray integers = CREATE_DA();
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(integers.capacity, 0);
    ASSERT_NULL(integers.data);

    integers.length += 2;
    integers.capacity += 3;
    integers.data = ARRAY_ALLOC(3, int);
    int *arrayPtr = integers.data; // So we can still free the array after re-initializing.

    INIT_DA(&integers);
    ASSERT_INT_EQUAL(integers.length, 0);
    ASSERT_INT_EQUAL(integers.capacity, 0);
    ASSERT_NULL(integers.data);
    free(arrayPtr);
}

/** Tests that the convenience macro for getting the last item in a dynamic array works properly. */
PRIVATE_TEST_CASE(test_last_item) {
    IntArray integers = CREATE_DA();
    PUSH_DA(&integers, 10);
    ASSERT_INT_EQUAL(LAST_ITEM_DA(&integers), 10);
    PUSH_DA(&integers, 11);
    ASSERT_INT_EQUAL(LAST_ITEM_DA(&integers), 11);
    PUSH_DA(&integers, 120);
    ASSERT_INT_EQUAL(LAST_ITEM_DA(&integers), 120);
    PUSH_DA(&integers, 130);
    ASSERT_INT_EQUAL(LAST_ITEM_DA(&integers), 130);

    DROP_DA(&integers);
    ASSERT_INT_EQUAL(LAST_ITEM_DA(&integers), 120);
    DROP_AMOUNT_DA(&integers, 2);
    ASSERT_INT_EQUAL(LAST_ITEM_DA(&integers), 10);
    
    FREE_DA(&integers);
}

/** Tests our generic dynamic arrays implementation. */
void test_dynamic_array() {
    TEST(test_creation_macros);
    TEST(test_append);
    TEST(test_pop_and_drop);
    TEST(test_last_item);
}
