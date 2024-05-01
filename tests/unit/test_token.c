#include "lukip.h"

#include "char_buffer.h"

#include "token.c"

/** Tests if equal_token handles equality properly. */
PRIVATE_TEST_CASE(test_equal_token) {
    Token equal1 = {.lexeme = "test", .pos = create_src_pos(1, 1, 4), .type = TOKEN_IDENTIFIER};
    Token equal2 = {.lexeme = "test", .pos = create_src_pos(1, 1, 4), .type = TOKEN_IDENTIFIER};
    ASSERT_TRUE(equal_token(equal1, equal2));

    CharBuffer buffer1 = create_char_buffer();
    buffer_append_string(&buffer1, "hello");
    Token string1 = {
        .lexeme = "str1", .pos = create_src_pos(1, 1, 4),
        .type = TOKEN_STRING_LIT, .stringVal = {.length = buffer1.length, .text = buffer1.data}
    };
    CharBuffer buffer2 = create_char_buffer();
    buffer_append_string(&buffer2, "hello");
    Token string2 = {
        .lexeme = "str2", .pos = create_src_pos(1, 1, 4),
        .type = TOKEN_STRING_LIT, .stringVal = {.length = buffer2.length, .text = buffer2.data}
    };
    ASSERT_TRUE(equal_token(string1, string2));
    free_char_buffer(&buffer1);
    free_char_buffer(&buffer2);

    Token integer1 = {
        .lexeme = "10", .pos = create_src_pos(1, 1, 2), .intVal = 10, .type = TOKEN_INT_LIT
    };
    Token integer2 = {
        .lexeme = "0xA", .pos = create_src_pos(1, 1, 3), .intVal = 10, .type = TOKEN_INT_LIT
    };
    ASSERT_TRUE(equal_token(integer1, integer2));

    Token float1 = {
        .lexeme = "2.30", .pos = create_src_pos(1, 1, 4), .floatVal = 2.30, .type = TOKEN_FLOAT_LIT
    };
    Token float2 = {
        .lexeme = "2.3", .pos = create_src_pos(1, 1, 3), .floatVal = 2.3, .type = TOKEN_FLOAT_LIT
    };
    ASSERT_TRUE(equal_token(float1, float2));

    Token notEqual1 = {
        .lexeme = "first", .pos = create_src_pos(1, 1, 3), .type = TOKEN_IDENTIFIER
    };
    Token notEqual2 = {
        .lexeme = "different", .pos = create_src_pos(1, 1, 3), .type = TOKEN_IDENTIFIER
    };
    ASSERT_FALSE(equal_token(notEqual1, notEqual2));
}

/** Tests if converting a token type to a string literal works properly. */
PRIVATE_TEST_CASE(test_type_to_string) {
    ASSERT_STRING_EQUAL(type_to_string(TOKEN_LIST_KW), "LIST");
    ASSERT_STRING_EQUAL(type_to_string(TOKEN_GREATER), "GREATER");
    ASSERT_STRING_EQUAL(type_to_string(TOKEN_IDENTIFIER), "IDENTIFIER");
}

/** Tests token.c. */
void test_token() {
    TEST(test_type_to_string);
    TEST(test_equal_token);
}
