#include "lukip.h"

#include "lexer.h"
#include "parser.h"

#include "compiler.c"

/** Tests that math expressions compile properly.*/
PRIVATE_TEST_CASE(test_compile_expr) {
    ZmxProgram program = create_zmx_program("no_file", false);
    char *source = "3 + 4 * --9 * 2 / 7 ** 6 / -2;"
        "$\"start {1 + 2} middle {'inner' + ' string'}\" + \" end.\";";
    FuncObj *compiledFunc = compile_source(&program, source, true);
    ASSERT_NOT_NULL(compiledFunc);

    const u8 expected[] = {
        OP_LOAD_CONST, 0, OP_LOAD_CONST, 1, OP_LOAD_CONST, 2, OP_MINUS, OP_MINUS, OP_MULTIPLY,
        OP_LOAD_CONST, 3, OP_MULTIPLY, OP_LOAD_CONST, 4, OP_LOAD_CONST, 5, OP_EXPONENT, OP_DIVIDE,
        OP_LOAD_CONST, 6, OP_MINUS, OP_DIVIDE, OP_ADD, OP_POP,
        OP_LOAD_CONST, 7, OP_LOAD_CONST, 8, OP_LOAD_CONST, 9, OP_ADD, OP_AS, 3, OP_LOAD_CONST, 10,
        OP_LOAD_CONST, 11, OP_FINISH_STRING, 1, OP_LOAD_CONST, 12, OP_FINISH_STRING, 1, OP_ADD,
        OP_AS, 3, OP_FINISH_STRING, 4, OP_LOAD_CONST, 13, OP_FINISH_STRING, 1, OP_ADD, OP_POP,
        OP_END
    };
    ASSERT_BYTES_EQUAL(compiledFunc->bytecode.data, expected, compiledFunc->bytecode.length);
    free_zmx_program(&program);
}

/** Tests compiler.c. */
void test_compiler() {
    TEST(test_compile_expr);
}
