#include "lukip.h"

#include "cli_handler.h"

#include "program.c"

/** Tests that we can properly create an initialized Zymux program from create_zmx_program(). */
PRIVATE_TEST_CASE(test_create_zmx_program) {
    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram program = create_zmx_program("zymux", &cli, false);
    ASSERT_STRING_EQUAL(program.currentFile->string, "zymux");
    ASSERT_FALSE(program.showErrors);
    free_zmx_program(&program);
}

/** Tests zmx_program.c. */
void test_program() {
    TEST(test_create_zmx_program);
}
