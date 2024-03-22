#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "zymux_program.h"

void user_error(
    ZymuxProgram *program, const int line, const char *errorName, const char *format, ...
) {
    program->hasErrored = true;

    if (!program->showErrors) {
        return;
    }
    va_list args;
    va_start(args, format);
    fprintf(stderr, "file \"%s\":\n\t%s [line=%d]: ", program->currentFile, errorName, line);
    vfprintf(stderr, format, args);
    fputc('\n', stderr);
    va_end(args);
}

void internal_error(
    const char *file, const char *func, const int line,
    const int exitCode, const char *errorName, const char *format, ...
) {
    fprintf(
        stderr, "Zymux implementation [file=\"%s\", func=%s(), line=%d]:\n\t", file, func, line
    );

    va_list args;
    va_start(args, format);
    fprintf(stderr, "%s", errorName);
    fprintf(stderr, ": ");
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, ".\n");
    exit(exitCode);
}
