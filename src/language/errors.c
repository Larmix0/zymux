#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <file.h>
#include "zymux_program.h"

static bool is_whitespace(const char ch) {
    switch (ch) {
    case ' ':
    case '\t':
    case '\r':
    case '\n':
        return true;
    default:
        return false;
    }
}

static void print_error_line(char *source, int idx, const int column, const int length) {
    int whitespaces = 0;
    char current = source[idx];
    while (is_whitespace(current)) {
        current = source[++idx];
        whitespaces++;
    }
    while (current != '\0' && current != '\n') {
        fputc(current, stderr);
        current = source[++idx];
    }
    fputc('\n', stderr);

    for (int i = 0; i < column - whitespaces - 1; i++) {
        fputc(' ', stderr);
    }
    for (int i = 0; i < length; i++) {
        fputc('^', stderr);
    }
    fputc('\n', stderr);
}

static void show_zmx_error_line(char *file, const int line, const int column, const int length) {
    char *source = alloc_source(file);
    const int sourceLength = strlen(source);
    int sourceLine = 1;
    for (int i = 0; i < sourceLength; i++) {
        if (sourceLine == line) {
            print_error_line(source, i, column, length);
            break;
        }
        if (source[i] == '\n') {
            sourceLine++;
        }
    }
    free(source);
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

    fputc('\n', stderr);
    exit(exitCode);
}

void user_error(
    ZymuxProgram *program, const int line, const int column, const int length,
    const char *errorName, const char *format, ...
) {
    bool hasErrored = program->hasErrored;
    program->hasErrored = true;

    if (!program->showErrors) {
        return;
    }
    if (hasErrored) {
        fputc('\n', stderr);
    }
    show_zmx_error_line(program->currentFile, line, column, length);
    va_list args;
    va_start(args, format);
    fprintf(stderr, "line %d in \"%s\":\n\t%s: ", line, program->currentFile, errorName);
    vfprintf(stderr, format, args);
    va_end(args);

    fputc('\n', stderr);
}
