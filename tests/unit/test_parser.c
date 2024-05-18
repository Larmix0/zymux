#include <stdarg.h>

#include "lukip.h"

#include "debug_ast.h"
#include "lexer.h"

#include "parser.c"

/**
 * Appends debug strings separated by the debug delimiter defined in constants.
 * 
 * A lot of times in unit testing we want to compare a stage like the lexer, parser, etc.,
 * with some expected result. Instead of manually building the "expected" AST for example,
 * we typically use this function to append some string that is written in the same format
 * as the one that the stage's debugger uses.
 * Which is by using the debug delimiter constant between each piece (like a lexer's token,
 * or parser's statement)
 * 
 * This allows us to just run the debugger on the code, copy and paste a string
 * that represents the whole structure, and append its pieces one by one
 * instead of manually building up the structure.
 * Finally, just compare the pre-built/copy-pasted string with the one the debugger returned
 * on the tested unit.
 */
static void buffer_append_ast_strings(CharBuffer *buffer, const int amount, ...) {
    va_list args;
    va_start(args, amount);
    for (int i = 0; i < amount; i++) {
        buffer_append_string(buffer, va_arg(args, char *));
        buffer_append_string(buffer, AST_DEBUG_DELIMITER);
    }
    buffer_append_string(buffer, "(EOF)");
    va_end(args);
}

/** Lexes and parses the passed source, then returns the AST debugger's string representation. */
static CharBuffer source_to_ast_string(char *source) {
    ZmxProgram program = create_zmx_program("test", false);
    Lexer lexer = create_lexer(&program, source);
    ASSERT_TRUE(lex(&lexer));

    Parser parser = create_parser(&program, lexer.tokens);
    ASSERT_TRUE(parse(&parser));

    CharBuffer astString = get_ast_string(&parser.ast);
    free_lexer(&lexer);
    free_parser(&parser);
    free_zmx_program(&program);
    free_all_nodes(&program);
    return astString;
}

/** Tests that the parser handles expressions correctly. */
PRIVATE_TEST_CASE(test_parser_expression) {
    CharBuffer actual = source_to_ast_string(
        "232; 3 ** 2; 10 --10; 2 * 10 ** 5 / 4 - 9; 0 * 0xff ** ----0 % 0;"
    );
    CharBuffer expected = create_char_buffer();
    buffer_append_ast_strings(
        &expected, 5,
        "(232)", "((** 3 2))", "((- 10 (- 10)))", "((- (/ (* 2 (** 10 5)) 4) 9))",
        "((% (* 0 (** 0xff (- (- (- (- 0)))))) 0))"
    );
    ASSERT_STRING_EQUAL(actual.data, expected.data);
    free_char_buffer(&actual);
    free_char_buffer(&expected);
}

/** Tests parser.c. */
void test_parser() {
    TEST(test_parser_expression);
}
