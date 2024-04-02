#ifndef ERRORS_H
#define ERRORS_H

#include "zymux_program.h"

/** Reports an internal memory-related error with the C implementation info manually passed. */
#define MEMORY_ERROR_ARGS(file, func, line, ...) \
    (internal_error((file), (func), (line), 1, "Memory error", __VA_ARGS__))

/** Reports an internal memory-related error where it was called. */
#define MEMORY_ERROR(...) \
    (MEMORY_ERROR_ARGS(__FILE__, __func__, __LINE__, __VA_ARGS__))

/** Reports an internal file-related error where it was called. */
#define FILE_ERROR(...) \
    (internal_error(__FILE__, __func__, __LINE__, 1, "File IO error", __VA_ARGS__))

/** 
 * Reports an error where a part of the implementation that wasn't meant to be executed was reached.
 * An example would be reaching a default case on a switch that should've covered the whole enum.
 * Unlike file errors or memory errors which may be caused by something in the user's computer
 * (like running out of memory).
 * Unreachable errors are never meant to be executed, so when seen they should be fixed immediately.
 */
#define UNREACHABLE_ERROR() \
    (internal_error( \
        __FILE__, __func__, __LINE__, 1, "Unreachable error", \
        "Reached an unreachable part of Zymux's internal code " \
        "(Please report this error to the developer(s) of Zymux, as this should never appear)" \
    ))

/** A user syntax error. Typically means an error occurred in lexing or parsing. */
#define SYNTAX_ERROR(program, errorToken) \
    (user_error( \
        (program), (errorToken).line, (errorToken).column, \
        (errorToken).length, "Syntax error", (errorToken).errorMessage \
    ))

/** General function for internal errors that occur in the C implementation of Zymux. */
void internal_error(
    const char *file, const char *func, const int line,
    const int exitCode, const char *errorName, const char *format, ...
);

/** General function for errors that happened due to a mistake from the person using Zymux. */
void user_error(
    ZymuxProgram *program, const int line, const int column, const int length,
    const char *errorName, const char *format, ...
);

#endif
