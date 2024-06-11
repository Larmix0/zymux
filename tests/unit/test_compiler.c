#include "lukip.h"

#include "lexer.h"
#include "parser.h"

#include "compiler.c"

/** Tests that math expressions compile properly.*/
PRIVATE_TEST_CASE(test_compile_math_expr) {
    ZmxProgram program = create_zmx_program("no_file", false);
    char *source = "3 + 4 * --9 * 2 / 7 ** 6 / -2;";
    Lexer lexer = create_lexer(&program, source);
    ASSERT_TRUE(lex(&lexer));
    Parser parser = create_parser(&program, lexer.tokens);
    ASSERT_TRUE(parse(&parser));
    Compiler compiler = create_compiler(&program, parser.ast, true);
    ASSERT_TRUE(compile(&compiler));

    const u8 expected[] = {
        OP_LOAD_CONST, 0, OP_LOAD_CONST, 1, OP_LOAD_CONST, 2, OP_MINUS, OP_MINUS, OP_MULTIPLY,
        OP_LOAD_CONST, 3, OP_MULTIPLY, OP_LOAD_CONST, 4, OP_LOAD_CONST, 5, OP_EXPONENT, OP_DIVIDE,
        OP_LOAD_CONST, 6, OP_MINUS, OP_DIVIDE, OP_ADD, OP_POP, OP_END
    };
    ASSERT_BYTES_EQUAL(compiler.func->bytecode.data, expected, compiler.func->bytecode.length);
    
    free_lexer(&lexer);
    free_all_nodes(&program);
    free_parser(&parser);
    free_compiler(&compiler);
    free_zmx_program(&program);
}

/** Tests compiler.c. */
void test_compiler() {
    TEST(test_compile_math_expr);
}
