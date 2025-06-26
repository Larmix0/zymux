#include <stdio.h>
#include <stdlib.h>

#include "cli_handler.h"
#include "compiler.h"
#include "file.h"
#include "gc.h"
#include "lexer.h"
#include "parser.h"
#include "program.h"
#include "report_error.h"
#include "vm.h"

/** Runs Zymux on a REPL loop. */
static void run_repl() {
    // TODO: implement repl.
}

/** 
 * Reads the passed file and makes it go through all the stages of Zymux to execute it.
 * 
 * If any debugging flags of the CLI are on, then it only compiles the file without executing it
 * in order to display only the debug output.
 */
static void run_zmx_file(char *passedPath, CliHandler *cli) {
    char *absoluteFile = get_absolute_path(passedPath);
    char *source = get_file_source(absoluteFile);
    
    ZmxProgram program = create_zmx_program(absoluteFile, cli, true);
    if (cli->debugTokens || cli->debugAst || cli->debugBytecode) {
        compile_source(&program, source, true); // Only compile to print the debugging.
    } else {
        interpret_source(&program, source, true);
    }

    free_zmx_program(&program);
    free(source);
    free(absoluteFile);
}

/** Main entry part of a program. */
int main(const int argc, char **argv) {
#if OS == UNKNOWN_OS
    OS_ERROR("Your operating system is not supported in Zymux.");
#endif
    CliHandler cli = create_cli_handler(argc, argv);
    if (!cli.validArgs) {
        print_cli_help();
        return EXIT_FAILURE;
    } else if (cli.help) {
        print_cli_help();
        return EXIT_SUCCESS; // Assume the user just wanted the help menu.
    }

    if (cli.file) {
        run_zmx_file(cli.file, &cli);
    } else {
        run_repl();
    }
    return cli.exitCode;
}
