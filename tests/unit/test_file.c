#include <string.h>

#include "lukip.h"

#include "file.c"

/** Tests if the alloc_fixed_source function correctly resolves different types of line breaks into LF. */
void test_fixed_source() {
    char *fixed = alloc_fixed_source("hello \n \r\n middle\n\r \r\nend\r\r");
    LUKIP_STRING_EQUAL(fixed, "hello \n \n middle\n\n \nend\n\n");

    char *noFix = "hello; this shouldn't \n\n be fixed as it only uses\n.\n";
    char *same = alloc_fixed_source(noFix);
    LUKIP_STRING_EQUAL(same, noFix);

    free(same);
    free(fixed);
}

/** Tests if we can properly open and read files with alloc_source(). */
void test_open_file() {
    char *expected = "Zymux text test file.\n";
    int expectedLength = strlen(expected);
    char *source = alloc_source("tests/unit/test.txt");
    int sourceLength = strlen(source);

    LUKIP_INT_EQUAL(sourceLength, expectedLength);
    LUKIP_IS_TRUE(strncmp(source, expected, sourceLength) == 0);
    free(source);
}

/** Tests file.c. */
void test_file() {
    TEST(test_fixed_source);
    TEST(test_open_file);
}
