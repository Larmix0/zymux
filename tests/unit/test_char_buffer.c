#include "lukip.h"

#include "char_buffer.c"

CharBuffer defaultBuffer;

/** Setup to automatically create a default buffer that is only initialized. */
PRIVATE_DECLARE_SETUP(buffer_setup) {
    defaultBuffer = create_char_buffer();
}

/** Teardown for the default empty buffer. */
PRIVATE_DECLARE_TEARDOWN(buffer_teardown) {
    free_char_buffer(&defaultBuffer);
}

/** Tests the creation of a char buffer. */
PRIVATE_TEST_CASE(test_char_buffer_create) {
    ASSERT_STRING_EQUAL(defaultBuffer.text, "");
}

/** Tests the char buffer's appending functions. */
PRIVATE_TEST_CASE(test_char_buffer_appenders) {
    buffer_append_string(&defaultBuffer, "");
    buffer_append_char(&defaultBuffer, 'J');
    ASSERT_STRING_EQUAL(defaultBuffer.text, "J");
    buffer_append_string_len(&defaultBuffer, "ack", strlen("ack"));
    buffer_append_char(&defaultBuffer, '\0');

    ASSERT_STRING_EQUAL(defaultBuffer.text, "Jack");
    buffer_append_string(&defaultBuffer, "");
    buffer_append_char(&defaultBuffer, ',');
    buffer_append_char(&defaultBuffer, ' ');
    buffer_append_strings(&defaultBuffer, 7, "", "Sam", ", ", "Liam,", "", " ", "and John");
    ASSERT_STRING_EQUAL(defaultBuffer.text, "Jack, Sam, Liam, and John");

    buffer_append_format(&defaultBuffer, ". Also, %s who is %d", "Sean", 23);
    ASSERT_STRING_EQUAL(defaultBuffer.text, "Jack, Sam, Liam, and John. Also, Sean who is 23");
}

/** Tests the char buffer's popping functions. */
PRIVATE_TEST_CASE(test_char_buffer_poppers) {
    buffer_append_string(&defaultBuffer, "Hello, world!");
    buffer_append_string(&defaultBuffer, "");
    buffer_pop(&defaultBuffer);
    ASSERT_STRING_EQUAL(defaultBuffer.text, "Hello, world");
    buffer_pop_amount(&defaultBuffer, 6);

    ASSERT_STRING_EQUAL(defaultBuffer.text, "Hello,");
    buffer_append_char(&defaultBuffer, ' ');
    buffer_append_string(&defaultBuffer, "mom");
    buffer_pop(&defaultBuffer);
    buffer_append_char(&defaultBuffer, '\0');
    buffer_pop(&defaultBuffer);
    buffer_append_char(&defaultBuffer, 'e');
    ASSERT_STRING_EQUAL(defaultBuffer.text, "Hello, me");
}

/** Tests char_buffer.c. */
void test_char_buffer() {
    MAKE_FIXTURE(buffer_setup, buffer_teardown);
    TEST(test_char_buffer_create);
    TEST(test_char_buffer_appenders);
    TEST(test_char_buffer_poppers);
    RESET_FIXTURE();
}
