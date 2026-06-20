#include "lukip.h"

#include "cli_handler.c"

PRIVATE_TEST_CASE(test_is_arg) {
    ASSERT_TRUE(is_arg("-h", strlen("-h"), "-h"));
    ASSERT_TRUE(is_arg("--hELp", strlen("--hELp"), "--help")); // Case insensitive.

    ASSERT_FALSE(is_arg("--no", strlen("--no"), "-h")); // Simply different.
    ASSERT_FALSE(is_arg("help", strlen("help"), "--help")); // No dashes.
    ASSERT_FALSE(is_arg("--hel", strlen("--hel"), "--help")); // Short of the whole arg.
    ASSERT_FALSE(is_arg("--helpp", strlen("--helpp"), "--help")); // More than the whole arg.
}

PRIVATE_TEST_CASE(test_parse_cli) {
    CliHandler replCli = {0};
    char *replArgs[] = {"exe"};
    parse_cli(1, replArgs, &replCli);
    // No file and no debugging enabled.
    ASSERT_NULL(replCli.file);
    ASSERT_FALSE(replCli.debugTokens || replCli.debugAst || replCli.debugBytecode);

    CliHandler fileCli = {0};
    char *fileArgs[] = {"exe", "file.ext", "--db-ast"};
    parse_cli(3, fileArgs, &fileCli);
    ASSERT_STRING_EQUAL(fileCli.file, "file.ext");
    // Correct debugging enabled.
    ASSERT_FALSE(fileCli.debugTokens);
    ASSERT_FALSE(fileCli.debugBytecode);
    ASSERT_TRUE(fileCli.debugAst);
}

/** Tests the general creation of a command line handler. */
PRIVATE_TEST_CASE(test_create_cli_handler) {
    char *validArgs[] = {"exe", "--help"};
    CliHandler cli = create_cli_handler(2, validArgs);
    ASSERT_FALSE(cli.debugTokens || cli.debugAst || cli.debugBytecode);
    ASSERT_TRUE(cli.help);
}

/** Tests cli_handler.c/.h. */
void test_cli_handler() {
    TEST(test_is_arg);
    TEST(test_parse_cli);
    TEST(test_create_cli_handler);
}
