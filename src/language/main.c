#include <stdio.h>
#include <stdlib.h>

#include "debug_tokens.h"
#include "errors.h"
#include "file.h"
#include "lexer.h"
#include "zymux_program.h"

/** Runs Zymux on a REPL loop. */
static void repl() {
    // TODO: implement repl.
}

/** Reads the past file and makes it go through all the stages of Zymux to execute it. */
static void run_zmx_file(char *file) {
    char *source = alloc_source(file);
    ZymuxProgram program = create_zymux_program(file, true);
    Lexer lexer = create_lexer(&program, source);

    if (!lex(&lexer)) {
        printf("SYNTAX ERROR, EXITING.\n");
    }
    print_tokens(lexer.tokens);
    free_lexer(&lexer);
    free(source);
    free_zymux_program(&program);
}

/** 
 * Main entry part of Zymux.
 * 1 argument means to use REPL, 2 means to read file regular, any other number is an error.
 */
int main(const int argc, char **argv) {
#if OS == UNKNOWN_OS
    fprintf(
        stderr,
        RED "OS error:\n\t" DEFAULT_COLOR "Your operating system is not supported in Zymux.\n"
    );
    exit(1);
#endif

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        run_zmx_file(argv[1]);
    } else {
        FILE_ERROR("Invalid amount of arguments.");
    }
    return 0;
}
