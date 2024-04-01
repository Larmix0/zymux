#include "lukip.h"

#include "test_char_buffer.h"
#include "test_file.h"
#include "test_lexer.h"
#include "test_zymux_program.h"

// Since every source must be manually included somewhere once and only once,
// all sources that aren't use by other test modules are here for the sake of linking.
#include "errors.c"
#include "debug_tokens.c"

/** Main function of the unit tests that calls all file testers. */
int main() {
    LUKIP_INIT();
    test_file();
    test_char_buffer();
    test_zymux_program();
    test_lexer();
    LUKIP_END();
    
    return 0;
}
