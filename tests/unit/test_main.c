#include "lukip.h"

#include "test_char_buffer.h"
#include "test_dynamic_array.h"
#include "test_file.h"
#include "test_lexer.h"
#include "test_program.h"

// Since every source must be manually included somewhere for testing once and only once,
// all sources that aren't used by other test modules (except main.c)
// are here for the sake of linking.
#include "report_error.c"
#include "debug_tokens.c"

/** Main function of the unit tests that calls all file testers. */
int main() {
#if OS == UNKNOWN_OS
    OS_ERROR("Your operating system is not supported in Zymux.");
#endif

    LUKIP_INIT();
    test_dynamic_array();
    test_file();
    test_char_buffer();
    test_program();
    test_lexer();
    LUKIP_END();
    
    return EXIT_SUCCESS;
}
