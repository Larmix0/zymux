#include "lukip.h"

#include "test_char_buffer.h"
#include "test_dynamic_array.h"
#include "test_file.h"
#include "test_lexer.h"
#include "test_node.h"
#include "test_parser.h"
#include "test_program.h"
#include "test_token.h"

// Since every source must be manually included somewhere for testing once and only once,
// all sources other than main.c that aren't used by other test modules are here for linking.
#include "debug_ast.c"
#include "debug_token.c"
#include "debug_bytecode.c"

#include "report_error.c"
#include "compiler.c"
#include "emitter.c"
#include "object.c"

/** Main function of the unit tests that calls all file testers. */
int main() {
#if OS == UNKNOWN_OS
    OS_ERROR("Your operating system is not supported for Zymux's tests.");
#endif

    LUKIP_INIT();
    test_dynamic_array();
    test_file();
    test_char_buffer();
    test_program();
    test_token();
    test_lexer();
    test_node();
    test_parser();
    return EXIT_SUCCESS;
}
