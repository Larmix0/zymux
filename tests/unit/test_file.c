#include <string.h>

#include "lukip.h"

#include "file.c"

void test_open_file() {
    char *expected = "Zymux text test file.\n";
    int expectedLength = strlen(expected);

    char *source = alloc_source("tests/unit/test.txt");
    int sourceLength = strlen(source);

    LUKIP_INT_EQUAL(sourceLength, expectedLength);
    LUKIP_IS_TRUE(strncmp(source, expected, sourceLength) == 0);
    free(source);
}

void test_file() {
    TEST(test_open_file);
}
