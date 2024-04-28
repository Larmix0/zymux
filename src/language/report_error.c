#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "constants.h"
#include "file.h"
#include "program.h"
#include "report_error.h"

/** Returns a position in a source code from the parameters. */
SourcePosition create_src_pos(int line, int column, int length) {
    SourcePosition position = {.line = line, .column = column, .length = length};
    return position;
}

/** Returns whether ch is a whitespace character or not. */
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

/** 
 * To call after reaching the first character of the error/warning line in a source.
 * 
 * Using the passed pos, we skip all preceding whitespace of a line,
 * then start actually printing so indents are skipped.
 * We then proceed to print carets when reaching column of pos.
 * The amount of carets is length in pos.
 * 
 * Current is the first character in the source after the newline.
 */
static void print_error_line(char *current, const SourcePosition pos) {
    int whitespaces = 0, lineIdx = 0;
    while (is_whitespace(*current)) {
        current++;
        lineIdx++;
        whitespaces++;
    }
    while (*current != '\0' && *current != '\n') {
        if (lineIdx == pos.column - 1) {
            fprintf(stderr, "%s", RED);
        }
        fputc(*current++, stderr);
        lineIdx++;
        if (lineIdx == pos.column + pos.length - 1) {
            fprintf(stderr, DEFAULT_COLOR);
        }
    }
    fputc('\n', stderr);

    for (int i = 0; i < pos.column - whitespaces - 1; i++) {
        fputc(' ', stderr);
    }
    fprintf(stderr, RED);
    for (int i = 0; i < pos.length - 1; i++) {
        fputc('^', stderr);
    }
    fputc('^', stderr); // Always output at least one for things like something missing at EOL.
    fprintf(stderr, DEFAULT_COLOR);
    fputc('\n', stderr);
}

/** Iterates through the source of the passed file and then prints the errored line when reached. */
static void show_zmx_error_line(char *file, const SourcePosition pos) {
    char *source = alloc_source(file);
    const int sourceLength = strlen(source);
    int sourceLine = 1;
    for (int i = 0; i < sourceLength; i++) {
        if (sourceLine == pos.line) {
            print_error_line(&source[i], pos);
            break;
        }
        if (source[i] == '\n') {
            sourceLine++;
        }
    }
    free(source);
}

/** Prints an error for an issue regarding the user's operating system. */
void os_error(const char *format, ...) {
    fprintf(stderr, RED "OS error:\n\t" DEFAULT_COLOR);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fputc('\n', stderr);
    exit(EXIT_FAILURE);
}

/** An internal error occurred within the Zymux implementation in C itself like a memory error. */
void internal_error(SourceInfo info, const char *errorName, const char *format, ...) {
    fprintf(
        stderr,
        "Zymux implementation [file=\"%s\", func=%s(), line=%d]:\n\t" RED "%s: " DEFAULT_COLOR,
        info.file, info.func, info.line, errorName
    );

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fputc('\n', stderr);
    exit(EXIT_FAILURE);
}

/** 
 * An error occurred while attempting to perform some operation related to files.
 * 
 * This is specifically for file errors outside of *.zmx programs.
 * Like running "./zymux x.zmx" where "x.zmx" doesn't exist.
 */
void file_error(const char *format, ...) {
    fprintf(stderr, RED "File IO error:\n\t" DEFAULT_COLOR);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fputc('\n', stderr);
    exit(EXIT_FAILURE);
}

/** 
 * Prints the error in the currentFile of program in the line and column of the passed pos.
 * 
 * This is reserved for errors that are due to the user's own mistake in a *.zmx file.
 * Like a compiler error or a runtime error.
 */
void zmx_user_error(
    ZmxProgram *program, const SourcePosition pos, const char *errorName, const char *format, ...
) {
    bool hasErrored = program->hasErrored;
    program->hasErrored = true;
    if (!program->showErrors) {
        return;
    }
    if (hasErrored) {
        fputc('\n', stderr);
    }

    show_zmx_error_line(program->currentFile, pos);
    fprintf(
        stderr, "line %d in \"%s\":\n\t" RED "%s: " DEFAULT_COLOR,
        pos.line, program->currentFile, errorName
    );

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fputc('\n', stderr);
}
