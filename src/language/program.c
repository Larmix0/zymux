#include <stdlib.h>
#include <string.h>

#include "node.h"
#include "object.h"
#include "program.h"

/** Returns an initialized zymux program with the parameters. */
ZmxProgram create_zmx_program(char *file, const bool showErrors) {
    ZmxProgram program = {
        .hasErrored = false, .showErrors = showErrors,
        .allNodes = NULL, .allObjs = NULL, .mainFile = NULL, .currentFile = NULL,
        .internedStrings = create_table(),
        .internedFalse = NULL, .internedTrue = NULL, .internedNull = NULL,
        .gc = create_empty_gc()
    };
    intern_objs(&program);
    program.mainFile = new_string_obj(&program, file);
    program.currentFile = program.mainFile;
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
    GC_CLEAR_PROTECTED(&program->gc);
}
