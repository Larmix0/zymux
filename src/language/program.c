#include <stdlib.h>
#include <string.h>

#include "built_in.h"
#include "node.h"
#include "object.h"
#include "program.h"

/** Returns an initialized zymux program with the parameters. */
ZmxProgram create_zmx_program(char *file, const bool showErrors) {
    ZmxProgram program = {
        .hasErrored = false, .showErrors = showErrors, .exitCode = 0,
        .allNodes = NULL, .allObjs = NULL, .currentFile = NULL,
        .internedFalse = NULL, .internedTrue = NULL, .internedNull = NULL,
        .builtIn = create_table(), .internedStrings = create_table(), .gc = create_gc()
    };
    intern_objs(&program);
    load_built_ins(&program);
    program.currentFile = new_string_obj(&program, file, strlen(file));
    return program;
}

/** 
 * Frees all the memory the passed program owns which is freed after the VM's execution.
 * 
 * This means it doesn't free nodes or tokens, as they should be manually freed
 * after compiling and before executing the VM for runtime memory efficiency.
 */
void free_zmx_program(ZmxProgram *program) {
    free_all_objs(program);
    free_table(&program->internedStrings);
    free_table(&program->builtIn);
    free_gc(&program->gc);
}
