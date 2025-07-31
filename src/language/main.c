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
    ZmxProgram *program = new_zmx_program("<REPL>", cli, false);
    VulnerableObjs *vulnObjs = &program->gc.startupVulnObjs;
    NodeArray emptyAst = CREATE_DA();
    Resolver resolver = create_resolver(vulnObjs, emptyAst);
    Vm vm;
    init_vm(
        &vm, program, new_runtime_func_obj(vulnObjs, new_func_obj(vulnObjs, NULL, 0, 0, true), NULL)
    );

    while (!cli->exitedRepl) {
        PUSH_DA(&replLines, repl_line(cli, vulnObjs, &resolver, &vm));
    }
    free_resolver(&resolver);
    free_vm(&vm);
    free_repl_lines(&replLines);
    free_zmx_program(program);
}

/** 
 * Reads the passed file and makes it go through all the stages of Zymux to execute it.
 * 
 * If any debugging flags of the CLI are on, then it only compiles the file without executing it
 * in order to display only the debug output.
 */
static void run_file(char *passedPath, CliHandler *cli) {
    char *source = alloc_file_source(passedPath);
    
    char *absoluteFile = alloc_absolute_path(passedPath);
    ZmxProgram *program = new_zmx_program(absoluteFile, cli, true);
    interpret_file_source(program, source, true);

    free_zmx_program(program);
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
        run_file(cli.file, &cli);
    } else {
        run_repl(&cli);
    }
    return cli.exitCode;
}
