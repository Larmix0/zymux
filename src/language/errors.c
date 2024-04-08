#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <file.h>
#include "zymux_program.h"

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
 * To call after iterating through the source and reaching the first character of the
 * error/warning line.
 * 
 * It skips all preceding whitespace of a line then starts actually printing so indents are skipped.
 * It then proceeds to print carets when reaching column. The amount of carets is length.
 * The color is what color the carets and what they're pointing to should be.
 */
static void print_line(
    char *current, const int column, const int length, const char *color
) {
    int whitespaces = 0, lineIdx = 0;
    while (is_whitespace(*current)) {
        current++;
        lineIdx++;
        whitespaces++;
    }
    while (*current != '\0' && *current != '\n') {
        if (lineIdx == column - 1) {
            fprintf(stderr, "%s", color);
        }
        fputc(*current++, stderr);
        lineIdx++;
        if (lineIdx == column + length - 1) {
            fprintf(stderr, DEFAULT_COLOR);
        }
    }
    fputc('\n', stderr);

    for (int i = 0; i < column - whitespaces - 1; i++) {
        fputc(' ', stderr);
    }
    fprintf(stderr, RED);
    for (int i = 0; i < length; i++) {
        fputc('^', stderr);
    }
    fprintf(stderr, DEFAULT_COLOR);
    fputc('\n', stderr);
}

/** Iterates through the source of the passed file and then prints the errored line when reached. */
static void show_zmx_error_line(char *file, const int line, const int column, const int length) {
    char *source = alloc_source(file);
    const int sourceLength = strlen(source);
    int sourceLine = 1;
    for (int i = 0; i < sourceLength; i++) {
        if (sourceLine == line) {
            print_line(&source[i], column, length, RED);
            break;
        }
        if (source[i] == '\n') {
            sourceLine++;
        }
    }
    free(source);
}

/**
 * Prints an error for an issue regarding the user's operating system.
 * Like an operating system that isn't supported in Zymux.
 */
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
void internal_error(
    const char *file, const char *func, const int line,
    const char *errorName, const char *format, ...
) {
    fprintf(
        stderr,
        "Zymux implementation [file=\"%s\", func=%s(), line=%d]:\n\t" RED "%s: " DEFAULT_COLOR,
        file, func, line, errorName
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
 * Prints the error in the currentFile of program in the line and column passed.
 * This is reserved for errors that are due to the user's own mistake in a *.zmx file,
 * like a compiler error or a runtime error.
 */
void zmx_user_error(
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
    fprintf(
        stderr, "line %d in \"%s\":\n\t" RED "%s: " DEFAULT_COLOR,
        line, program->currentFile, errorName
    );

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fputc('\n', stderr);
}
