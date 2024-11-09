#include "lukip.h"

#include "constants.h"

#include "allocator.c"

/** Tests that all the allocation abstraction for arbitrary sizes/bytes work properly. */
PRIVATE_TEST_CASE(test_size_allocators) {
    int *singleInt = TYPE_ALLOC(int);
    *singleInt = 2147;
    ASSERT_INT_EQUAL(*singleInt, 2147);

    u32 *single32 = ALLOC(sizeof(u32));
    *single32 = 3844465920;
    ASSERT_INT_EQUAL(*single32, 3844465920);

    free(singleInt);
    free(single32);
}

/** Tests all allocators that deal with arrays. */
PRIVATE_TEST_CASE(test_array_allocators) {
    u8 *array = ARRAY_ALLOC(3, u8);
    for (int i = 3; i < 6; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 3; i < 6; i++) {
        ASSERT_UINT8_EQUAL(array[i - 3], i * i);
    }

    array = ARRAY_REALLOC(array, 5, sizeof(u8));
    for (int i = 6; i < 8; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 6; i < 8; i++) {
        ASSERT_UINT8_EQUAL(array[i - 3], i * i);
    }

    array = ARRAY_REALLOC(array, 7, sizeof(u8));
    for (int i = 8; i < 10; i++) {
        array[i - 3] = i * i;
    }
    for (int i = 8; i < 10; i++) {
        ASSERT_UINT8_EQUAL(array[i - 3], i * i);
    }
    free(array);
}


/** Tests the entire allocator.c file. */
void test_allocator() {
    TEST(test_size_allocators);
    TEST(test_array_allocators);
}
