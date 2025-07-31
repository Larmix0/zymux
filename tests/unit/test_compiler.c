#include "lukip.h"

#include "cli_handler.h"
#include "lexer.h"
#include "parser.h"

#include "compiler.c"

/** Tests that math expressions compile properly.*/
PRIVATE_TEST_CASE(test_compile_expr) {
    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram *program = new_zmx_program("no_file", &cli, false);
    char *source = "3 + 4 * --9 * 2 / 7 ** 6 / -2;"
        "$\"start {1 + 2} middle {'inner' + ' string'}\" + ' end.';";
    FuncObj *func = compile_file_source(&program->gc.startupVulnObjs, source, true);
    ASSERT_NOT_NULL(func);

    const u8 expected[] = {
        OP_LOAD_CONST, 0, OP_LOAD_CONST, 1, OP_LOAD_CONST, 2, OP_MINUS, OP_MINUS, OP_MULTIPLY,
        OP_LOAD_CONST, 3, OP_MULTIPLY, OP_LOAD_CONST, 4, OP_LOAD_CONST, 5, OP_EXPONENT, OP_DIVIDE,
        OP_LOAD_CONST, 6, OP_MINUS, OP_DIVIDE, OP_ADD, OP_POP_LOCAL,
        OP_LOAD_CONST, 7, OP_LOAD_CONST, 8, OP_LOAD_CONST, 9, OP_ADD, OP_AS, 3, OP_LOAD_CONST, 10,
        OP_LOAD_CONST, 11, OP_LOAD_CONST, 12, OP_ADD, OP_AS, 3,
        OP_FINISH_STRING, 4, OP_LOAD_CONST, 13, OP_ADD, OP_POP_LOCAL, OP_EOF
    };
    ASSERT_BYTES_EQUAL(func->bytecode.data, expected, func->bytecode.length);
    free_zmx_program(program);
}

/** Tests compiler.c. */
void test_compiler() {
    TEST(test_compile_expr);
}
