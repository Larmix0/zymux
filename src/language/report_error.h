#ifndef REPORT_ERROR_H
#define REPORT_ERROR_H

#include "program.h"

/** Reports an error relating to the user's operating system.*/
#define OS_ERROR(...) (os_error(__VA_ARGS__))

/** Reports a memory-related error with the C implementation info manually passed. */
#define MEMORY_ERROR_ARGS(file, func, line, ...) \
    (internal_error((file), (func), (line), "Memory error", __VA_ARGS__))

/** Reports a memory-related error where it was called. */
#define MEMORY_ERROR(...) (MEMORY_ERROR_ARGS(__FILE__, __func__, __LINE__, __VA_ARGS__))

/** 
 * Reports an error where a part of the implementation that wasn't meant to be executed was reached.
 * 
 * An example would be reaching a default case on a switch that should've covered a whole enum.
 * 
 * Unlike file errors or memory errors which may be caused by something in the user's computer
 * (like running out of memory), unreachable errors are never ever meant to be executed,
 * so when seen they should be fixed immediately.
 */
#define UNREACHABLE_ERROR() \
    (internal_error( \
        __FILE__, __func__, __LINE__, "Unreachable error", \
        "Reached an unreachable part of Zymux's internal code " \
        "(Please report this error to the developer(s) of Zymux, as this should never appear)" \
    ))

/** Reports a file-related error. */
#define FILE_ERROR(...) (file_error(__VA_ARGS__))

/** A user syntax error. Typically means an error occurred in lexing or parsing. */
#define SYNTAX_ERROR(program, pos, message) \
    (zmx_user_error((program), (pos), "Syntax error", (message)))

/** Represents a position in some source code. */
typedef struct {
    int line; /** Which line the position starts. */
    int column; /** Which column of the given line the position starts. */
    int length; /** How many consecutive characters the position covers. */
} SourcePosition;

/** Returns a position in the source code created from the parameters */
SourcePosition create_src_pos(int line, int column, int length);

/** Displays an error relating to the user's operating system. */
void os_error(const char *format, ...);

/** Displays an internal error that occurred in the C implementation of Zymux. */
void internal_error(
    const char *file, const char *func, const int line,
    const char *errorName, const char *format, ...
);

/** Displays a file error. */
void file_error(const char *format, ...);

/** Displays an error that happened due to a mistake from the person using Zymux in a *.zmx file. */
void zmx_user_error(
    ZmxProgram *program, SourcePosition pos, const char *errorName, const char *format, ...
);

#endif
