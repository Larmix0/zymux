#include <stdarg.h>

#include "lukip.h"

#include "debug_ast.h"
#include "lexer.h"

#include "parser.c"

/** Lexes and parses the passed source, then returns the AST debugger's string representation. */
static CharBuffer source_to_ast_string(char *source) {
    ZmxProgram program = create_zmx_program("test", false);
    Lexer lexer = create_lexer(&program, source);
    LUKIP_IS_TRUE(lex(&lexer));

    Parser parser = create_parser(&program, lexer.tokens);
    LUKIP_IS_TRUE(parse(&parser));

    CharBuffer astString = allocate_ast_string(&parser.ast);
    free_lexer(&lexer);
    free_parser(&parser);
    free_zmx_program(&program);
    free_all_nodes(&program);
    return astString;
}

/** Tests that the parser handles expressions correctly. */
static void test_parser_expression() {
    CharBuffer actual = source_to_ast_string(
        "232; 3 ** 2; 10 --10; 2 * 10 ** 5 / 4 - 9; 0 * 0xff ** ----0 % 0;"
    );
    CharBuffer expected = create_char_buffer();
    buffer_append_debug(
        &expected, 5,
        "232", "(** 3 2)", "(- 10 (- 10))", "(- (/ (* 2 (** 10 5)) 4) 9)",
        "(% (* 0 (** 0xff (- (- (- (- 0)))))) 0)"
    );
    LUKIP_STRING_EQUAL(actual.data, expected.data);
    free_char_buffer(&actual);
    free_char_buffer(&expected);
}

/** Tests parser.c. */
void test_parser() {
    TEST(test_parser_expression);
}
