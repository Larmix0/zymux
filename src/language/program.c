#include <stdlib.h>
#include <string.h>

#include "built_in.h"
#include "lexer.h"
#include "node.h"
#include "object.h"
#include "program.h"

/** Returns an initialized, allocated zymux program with the parameters. */
ZmxProgram *new_zmx_program(char *file, CliHandler *cli, const bool showErrors) {
    ZmxProgram *program = TYPE_ALLOC(ZmxProgram);

    MUTEX_INIT(&program->printLock);
    program->isRuntime = false;
    program->hasErrored = false;
    program->showErrors = showErrors;
    program->cli = cli;
    INIT_DA(&program->allTokenStrings);

    program->allNodes = NULL;
    program->allObjs = NULL;
    program->currentFile = NULL;

    program->internedFalse = NULL;
    program->internedTrue = NULL;
    program->internedNull = NULL;
    program->internedStrings = create_table();
    program->builtIn = empty_built_ins(),
    program->gc = create_gc(program);

    // Now initialize the built-in objects after everything's been safely zeroed out.
    intern_objs(&program->gc.startupVulnObjs);
    load_built_ins(&program->gc.startupVulnObjs);
    program->currentFile = new_string_obj(&program->gc.startupVulnObjs, file, strlen(file));
    return program;
}

/** Frees all the memory the passed program owns which is freed after the VM's execution. */
void free_zmx_program(ZmxProgram *program) {
    free_all_token_strings(program);
    free_all_nodes(program);
    free_all_objs(program);

    free_table(&program->internedStrings);
    free_built_ins(&program->builtIn);
    free_gc(&program->gc);
    MUTEX_DESTROY(&program->printLock);

    free(program); // The program itself is allocated.
}
