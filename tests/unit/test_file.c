#include <string.h>

#include "lukip.h"

#include "file.c"

/** Tests that get_path_type() correctly determines whether something is a file/directory. */
PRIVATE_TEST_CASE(test_get_path_type) {
    CharBuffer path = create_char_buffer();
    buffer_append_format(&path, "tests%cunit%ctest_open_dir", PATH_SEPARATOR, PATH_SEPARATOR);
    ASSERT_TRUE(get_path_type(path.text) == PATH_DIRECTORY);
    
    buffer_append_format(&path, "%cfile_test.txt", PATH_SEPARATOR);
    ASSERT_TRUE(get_path_type(path.text) == PATH_FILE);

    free_char_buffer(&path);
}

/** Tests that parse_file_mode() returns the correct enum for a file type (including invalid). */
PRIVATE_TEST_CASE(test_parse_file_mode) {
    ASSERT_TRUE(parse_file_mode("r", 1) == FILE_READ_ONLY);
    ASSERT_TRUE(parse_file_mode("rb", 2) == FILE_READ_ONLY);
    ASSERT_TRUE(parse_file_mode("a", 1) == FILE_WRITE_ONLY);
    ASSERT_TRUE(parse_file_mode("wb", 2) == FILE_WRITE_ONLY);

    ASSERT_TRUE(parse_file_mode("w+", 2) == FILE_READ_AND_WRITE);
    ASSERT_TRUE(parse_file_mode("r+", 2) == FILE_READ_AND_WRITE);
    ASSERT_TRUE(parse_file_mode("ab+", 3) == FILE_READ_AND_WRITE);
    ASSERT_TRUE(parse_file_mode("rb+", 3) == FILE_READ_AND_WRITE);

    ASSERT_TRUE(parse_file_mode("", 0) == FILE_INVALID);
    ASSERT_TRUE(parse_file_mode("fullyInvalid", strlen("fullyInvalid")) == FILE_INVALID);
    ASSERT_TRUE(parse_file_mode("rj", 2) == FILE_INVALID);
    ASSERT_TRUE(parse_file_mode("k+", 2) == FILE_INVALID);
}


/** Tests if the alloc_fixed_source() correctly resolves different types of line breaks into LF. */
PRIVATE_TEST_CASE(test_alloc_fixed_source) {
    char *fixed = alloc_fixed_source("hello \n \r\n middle\n\r \r\nend\r\r");
    ASSERT_STRING_EQUAL(fixed, "hello \n \n middle\n\n \nend\n\n");

    const char *noFix = "hello; this shouldn't \n\n be fixed as it only uses\n.\n";
    char *noFixAfter = alloc_fixed_source(noFix);
    ASSERT_STRING_EQUAL(noFixAfter, noFix);

    free(noFixAfter);
    free(fixed);
}

/** Tests file.c. */
void test_file() {
    TEST(test_get_path_type);
    TEST(test_parse_file_mode);
    TEST(test_alloc_fixed_source);
}
