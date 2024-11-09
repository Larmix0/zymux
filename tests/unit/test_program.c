#include "lukip.h"

#include "program.c"

/** Tests that we can properly create an initialized Zymux program from create_zmx_program(). */
PRIVATE_TEST_CASE(test_create_zmx_program) {
    ZmxProgram program = create_zmx_program("zymux", false);
    ASSERT_STRING_EQUAL(program.currentFile->string, "zymux");
    ASSERT_FALSE(program.showErrors);
    free_zmx_program(&program);
}

/** Tests zmx_program.c. */
void test_program() {
    TEST(test_create_zmx_program);
}
