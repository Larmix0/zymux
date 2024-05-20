#include <stdio.h>
#include <stdlib.h>

#include "debug_ast.h"
#include "debug_bytecode.h"
#include "debug_token.h"

#include "compiler.h"
#include "file.h"
#include "lexer.h"
#include "parser.h"
#include "program.h"
#include "report_error.h"
#include "vm.h"

/** Runs Zymux on a REPL loop. */
static void repl() {
    // TODO: implement repl.
}

/** Reads the passed file and makes it go through all the stages of Zymux to execute it. */
static void run_zmx_file(char *file) {
    char *source = alloc_source(file);
    ZmxProgram program = create_zmx_program(file, true);
    interpret_source(&program, source);
    free_zmx_program(&program);
    free(source);
}

/** 
 * Main entry part of Zymux.
 * 
 * 1 argument means to use REPL, 2 means to read file regular, any other number is an error for now.
 * We'll later hopefully add other options. Like one for displaying bytecode for example.
 */
int main(const int argc, char **argv) {
#if OS == UNKNOWN_OS
    OS_ERROR("Your operating system is not supported in Zymux.");
#endif

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        run_zmx_file(argv[1]);
    } else {
        FILE_ERROR("Invalid amount of arguments.");
    }
    return EXIT_SUCCESS;
}
