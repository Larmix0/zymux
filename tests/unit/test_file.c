#include <string.h>

#include "lukip.h"

#include "file.c"

/** Tests that get_path_type() correctly determines whether something is a file/directory. */
static void test_get_path_type() {
    LUKIP_IS_TRUE(get_path_type("tests/unit/open_file_test.txt") == PATH_FILE);
    LUKIP_IS_TRUE(get_path_type("tests/unit/open_folder_test") == PATH_DIRECTORY);
}

/** Tests if the alloc_fixed_source function correctly resolves different types of line breaks into LF. */
static void test_fixed_source() {
    char *fixed = alloc_fixed_source("hello \n \r\n middle\n\r \r\nend\r\r");
    LUKIP_STRING_EQUAL(fixed, "hello \n \n middle\n\n \nend\n\n");

    char *noFix = "hello; this shouldn't \n\n be fixed as it only uses\n.\n";
    char *noFixAfter = alloc_fixed_source(noFix);
    LUKIP_STRING_EQUAL(noFixAfter, noFix);

    free(noFixAfter);
    free(fixed);
}

/** Tests if we can properly open and read files with alloc_source(). */
static void test_open_file() {
    char *expected = "File to test opening files.\n";
    int expectedLength = strlen(expected);
    char *source = alloc_source("tests/unit/open_file_test.txt");
    int sourceLength = strlen(source);

    LUKIP_INT_EQUAL(sourceLength, expectedLength);
    LUKIP_IS_TRUE(strncmp(source, expected, sourceLength) == 0);
    free(source);
}

/** Tests file.c. */
void test_file() {
    TEST(test_get_path_type);
    TEST(test_fixed_source);
    TEST(test_open_file);
}
