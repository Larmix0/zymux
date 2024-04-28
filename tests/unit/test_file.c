#include <string.h>

#include "lukip.h"

#include "file.c"

/** Tests that get_path_type() correctly determines whether something is a file/directory. */
static void test_get_path_type() {
    CharBuffer path = create_char_buffer();
    buffer_append_format(&path, "tests%cunit%ctest_open_dir", PATH_DELIMITER, PATH_DELIMITER);
    LUKIP_IS_TRUE(get_path_type(path.data) == PATH_DIRECTORY);
    
    buffer_append_format(&path, "%cfile_test.txt", PATH_DELIMITER);
    LUKIP_IS_TRUE(get_path_type(path.data) == PATH_FILE);

    free_char_buffer(&path);
}

/** Tests if the alloc_fixed_source function correctly resolves different types of line breaks into LF. */
static void test_fixed_source() {
    char *fixed = alloc_fixed_source("hello \n \r\n middle\n\r \r\nend\r\r");
    LUKIP_STRING_EQUAL(fixed, "hello \n \n middle\n\n \nend\n\n");

    const char *noFix = "hello; this shouldn't \n\n be fixed as it only uses\n.\n";
    char *noFixAfter = alloc_fixed_source(noFix);
    LUKIP_STRING_EQUAL(noFixAfter, noFix);

    free(noFixAfter);
    free(fixed);
}

/** Tests if we can properly open and read files with alloc_source(). */
static void test_open_file() {
    const char *expected = "File to test opening files.\n";
    const int expectedLength = strlen(expected);

    CharBuffer sourcePath = create_char_buffer();
    buffer_append_format(
        &sourcePath, "tests%cunit%ctest_open_dir%cfile_test.txt",
        PATH_DELIMITER, PATH_DELIMITER, PATH_DELIMITER
    );
    char *source = alloc_source(sourcePath.data);
    const int sourceLength = strlen(source);

    LUKIP_INT_EQUAL(sourceLength, expectedLength);
    LUKIP_IS_TRUE(strncmp(source, expected, sourceLength) == 0);
    free(source);
    free_char_buffer(&sourcePath);
}

/** Tests file.c. */
void test_file() {
    TEST(test_get_path_type);
    TEST(test_fixed_source);
    TEST(test_open_file);
}
