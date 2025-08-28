#ifndef CLI_HANDLER_H
#define CLI_HANDLER_H

#include <stdbool.h>

#include "constants.h"

typedef struct ZmxProgram ZmxProgram;
typedef struct Resolver Resolver;
typedef struct Vm Vm;
typedef struct VulnerableObjs VulnerableObjs;

/** Stores all information needed to handle the command line interface of the program. */
typedef struct {
    bool validArgs; /** Whether or not the args given to the CLI handler were valid. */
    bool exitedRepl; /** Whether or not REPL has finished. */
    char *file; /** Name of file being ran (NULL if REPL is running). */

    bool help;
    bool debugTokens;
    bool debugAst;
    bool debugBytecode;

    /** 
     * Whether the program exited via an exit statement (true), or EOF (false).
     * 
     * Useful for REPL to differentiate between a line exiting from EOF, or if the user
     * manually wrote an exit statement with an exit code.
     */
    bool manuallyExited;

    /** Current number for when we finish the program (0 by default). */
    ZmxInt exitCode;
} CliHandler;

/** Creates a CLI (command line interface) handler, which is returned after parsing the args. */
CliHandler create_cli_handler(const int argc, char **argv);

/** Prints the documentation of the CLI. */
void print_cli_help();

/** Returns an allocated string of a line that the user writes in stdin. */
char *get_stdin_line();

/** Prompts the user for one line and runs it in REPL once. */
char *repl_line(CliHandler *cli, VulnerableObjs *vulnObjs, Resolver *resolver, Vm *vm);

#endif
