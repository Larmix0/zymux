#include "lukip.h"

#include "zymux_program.h"

#include "char_buffer.c"

CharBuffer defaultBuffer;

/** Setup to automatically create a default buffer that is only initialized. */
static void buffer_setup() {
    defaultBuffer = create_char_buffer();
}

/** Teardown for the default empty buffer. */
static void buffer_teardown() {
    free_char_buffer(&defaultBuffer);
}

/** Tests the creation of a char buffer. */
static void test_char_buffer_create() {
    LUKIP_STRING_EQUAL(defaultBuffer.text, "");
}

/** Tests the char buffer's appending functions. */
static void test_char_buffer_appenders() {
    buffer_append_char(&defaultBuffer, 'J');
    LUKIP_STRING_EQUAL(defaultBuffer.text, "J");
    buffer_append_string(&defaultBuffer, "ack");
    
    LUKIP_STRING_EQUAL(defaultBuffer.text, "Jack");
    buffer_append_char(&defaultBuffer, ',');
    buffer_append_char(&defaultBuffer, ' ');
    buffer_append_strings(&defaultBuffer, 5, "Sam", ", ", "Liam,", " ", "and John");
    LUKIP_STRING_EQUAL(defaultBuffer.text, "Jack, Sam, Liam, and John");
}

/** Tests the char buffer's popping functions. */
static void test_char_buffer_poppers() {
    buffer_append_string(&defaultBuffer, "Hello, world!");
    buffer_pop(&defaultBuffer);
    LUKIP_STRING_EQUAL(defaultBuffer.text, "Hello, world");
    buffer_pop_amount(&defaultBuffer, 6);

    LUKIP_STRING_EQUAL(defaultBuffer.text, "Hello,");
    buffer_append_char(&defaultBuffer, ' ');
    buffer_append_string(&defaultBuffer, "mom");
    buffer_pop(&defaultBuffer);
    buffer_pop(&defaultBuffer);
    buffer_append_char(&defaultBuffer, 'e');
    LUKIP_STRING_EQUAL(defaultBuffer.text, "Hello, me");
}

/** Tests char_buffer.c. */
void test_char_buffer() {
    MAKE_TEST_FIXTURE(buffer_setup, buffer_teardown);
    TEST(test_char_buffer_create);
    TEST(test_char_buffer_appenders);
    TEST(test_char_buffer_poppers);
    RESET_TEST_FIXTURE();
}
