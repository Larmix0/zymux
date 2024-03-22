#include <stdio.h>
#include <stdlib.h>

#include "debug_tokens.h"
#include "errors.h"
#include "file.h"
#include "lexer.h"
#include "zymux_program.h"

static void repl() {
    // TODO: implement repl.
}

static void run_zmx_file(char *file) {
    char *source = alloc_source(file);
    ZymuxProgram program = create_zymux_program(file, true);
    Lexer lexer = create_lexer(&program, source);

    if (!lex(&lexer)) {
        printf("SYNTAX ERROR, EXITING.\n");
        exit(1);
    }
    print_tokens(lexer.tokens);
    free_lexer(&lexer);
    free(source);
    free_zymux_program(&program);
}

int main(const int argc, char **argv) {
    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        run_zmx_file(argv[1]);
    } else {
        FILE_ERROR("Invalid amount of arguments.");
    }
    return 0;
}
