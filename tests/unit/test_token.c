#include "lukip.h"

#include "char_buffer.h"

#include "token.c"

/** Tests if converting a token type to a string literal works properly. */
PRIVATE_TEST_CASE(test_token_type_to_string) {
    ASSERT_STRING_EQUAL(token_type_string(TOKEN_FLOAT_KW), "FLOAT");
    ASSERT_STRING_EQUAL(token_type_string(TOKEN_GREATER), "GREATER");
    ASSERT_STRING_EQUAL(token_type_string(TOKEN_IDENTIFIER), "IDENTIFIER");
}

/** Tests if the functions for creating arbitrary non-positioned tokens work. */
PRIVATE_TEST_CASE(test_token_creators) {
    const Token leftShift = create_token("<<", TOKEN_LSHIFT);
    ASSERT_STRING_EQUAL(leftShift.lexeme, "<<");
    ASSERT_TRUE(leftShift.type == TOKEN_LSHIFT);

    const Token decimalInt = create_int_token("23", 10);
    ASSERT_STRING_EQUAL(decimalInt.lexeme, "23");
    ASSERT_INT_EQUAL(decimalInt.intVal, 23);

    const Token hexInt = create_int_token("E2D", 16);
    ASSERT_STRING_EQUAL(hexInt.lexeme, "E2D");
    ASSERT_INT_EQUAL(hexInt.intVal, 3629);

    const Token string = create_string_token("String.", strlen("String."));
    ASSERT_STRING_EQUAL(string.lexeme, "String.");
    ASSERT_STRING_EQUAL(string.stringVal.text, "String.");
}

/** Tests if equal_token handles equality properly. */
PRIVATE_TEST_CASE(test_equal_token) {
    const Token equal1 = {
        .lexeme = "test", .pos = create_src_pos(1, 1, 4), .type = TOKEN_IDENTIFIER
    };
    const Token equal2 = {
        .lexeme = "test", .pos = create_src_pos(1, 1, 4), .type = TOKEN_IDENTIFIER
    };
    ASSERT_TRUE(equal_token(equal1, equal2));

    CharBuffer buffer1 = create_char_buffer();
    buffer_append_string(&buffer1, "hello");
    const Token string1 = {
        .lexeme = "str1", .pos = create_src_pos(1, 1, 4),
        .type = TOKEN_STRING_LIT, .stringVal = {.length = buffer1.length, .text = buffer1.text}
    };
    CharBuffer buffer2 = create_char_buffer();
    buffer_append_string(&buffer2, "hello");
    const Token string2 = {
        .lexeme = "str2", .pos = create_src_pos(1, 1, 4),
        .type = TOKEN_STRING_LIT, .stringVal = {.length = buffer2.length, .text = buffer2.text}
    };
    ASSERT_TRUE(equal_token(string1, string2));
    free_char_buffer(&buffer1);
    free_char_buffer(&buffer2);

    const Token integer1 = {
        .lexeme = "10", .pos = create_src_pos(1, 1, 2), .intVal = 10, .type = TOKEN_INT_LIT
    };
    const Token integer2 = {
        .lexeme = "0xA", .pos = create_src_pos(1, 1, 3), .intVal = 10, .type = TOKEN_INT_LIT
    };
    ASSERT_TRUE(equal_token(integer1, integer2));

    const Token float1 = {
        .lexeme = "2.30", .pos = create_src_pos(1, 1, 4), .floatVal = 2.30, .type = TOKEN_FLOAT_LIT
    };
    const Token float2 = {
        .lexeme = "2.3", .pos = create_src_pos(1, 1, 3), .floatVal = 2.3, .type = TOKEN_FLOAT_LIT
    };
    ASSERT_TRUE(equal_token(float1, float2));

    const Token notEqual1 = {
        .lexeme = "first", .pos = create_src_pos(1, 1, 3), .type = TOKEN_IDENTIFIER
    };
    const Token notEqual2 = {
        .lexeme = "different", .pos = create_src_pos(1, 1, 3), .type = TOKEN_IDENTIFIER
    };
    ASSERT_FALSE(equal_token(notEqual1, notEqual2));
}

/** Tests token.c. */
void test_token() {
    TEST(test_token_type_to_string);
    TEST(test_token_creators);
    TEST(test_equal_token);
}
