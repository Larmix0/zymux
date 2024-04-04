#include <string.h>

#include "lukip.h"

#include "file.c"

/** Tests that get_path_type() correctly determines whether something is a file/directory. */
static void test_get_path_type() {
    char delimiter = '/';
    if (OS == WINDOWS_OS) {
        delimiter = '\\';
    }

    CharBuffer filePath = create_char_buffer();
    buffer_append_string(&filePath, "tests");
    buffer_append_char(&filePath, delimiter);
    buffer_append_string(&filePath, "unit");
    buffer_append_char(&filePath, delimiter);
    buffer_append_string(&filePath, "open_file_test.txt");

    CharBuffer directoryPath = create_char_buffer();
    buffer_append_string(&directoryPath, "tests");
    buffer_append_char(&directoryPath, delimiter);
    buffer_append_string(&directoryPath, "unit");
    buffer_append_char(&directoryPath, delimiter);
    buffer_append_string(&directoryPath, "open_folder_test");

    LUKIP_IS_TRUE(get_path_type(filePath.text) == PATH_FILE);
    LUKIP_IS_TRUE(get_path_type(directoryPath.text) == PATH_DIRECTORY);

    free_char_buffer(&filePath);
    free_char_buffer(&directoryPath);
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

    char delimiter = '/';
    if (OS == WINDOWS_OS) {
        delimiter = '\\';
    }
    CharBuffer sourcePath = create_char_buffer();
    buffer_append_string(&sourcePath, "tests");
    buffer_append_char(&sourcePath, delimiter);
    buffer_append_string(&sourcePath, "unit");
    buffer_append_char(&sourcePath, delimiter);
    buffer_append_string(&sourcePath, "open_file_test.txt");

    char *source = alloc_source(sourcePath.text);
    int sourceLength = strlen(source);

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
