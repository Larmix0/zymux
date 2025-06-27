#include <stdio.h>
#include <stdlib.h>

#include "char_buffer.h"
#include "cli_handler.h"
#include "compiler.h"
#include "file.h"
#include "gc.h"
#include "lexer.h"
#include "parser.h"
#include "program.h"
#include "report_error.h"
#include "resolver.h"
#include "vm.h"

/** Frees the REPL lines string array and all its contents. */
static void free_repl_lines(StringArray *replLines) {
    for (u32 i = 0; i < replLines->length; i++) {
        free(replLines->data[i]);
    }
    FREE_DA(replLines);
}

/** Runs a program on a REPL loop. */
static void run_repl(CliHandler *cli) {
    StringArray replLines = CREATE_DA();
    ZmxProgram program = create_zmx_program("<REPL>", cli, false);
    NodeArray emptyAst = CREATE_DA();
    Resolver resolver = create_resolver(&program, emptyAst);
    Vm vm = create_vm(&program, new_func_obj(&program, NULL, 0, 0, NULL, true));
    program.gc.vm = &vm;

    while (!cli->exitedRepl) {
        APPEND_DA(&replLines, repl_line(cli, &program, &resolver, &vm));
    }
    free_resolver(&resolver);
    free_vm(&vm);
    free_repl_lines(&replLines);
    free_zmx_program(&program);
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
        compile_file_source(&program, source, true); // Only compile to print the debugging.
    } else {
        interpret_file_source(&program, source, true);
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
        run_repl(&cli);
    }
    return cli.exitCode;
}
