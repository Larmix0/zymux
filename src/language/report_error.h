#ifndef REPORT_ERROR_H
#define REPORT_ERROR_H

/** Reports an error relating to the user's operating system.*/
#define OS_ERROR(...) (os_error(__VA_ARGS__))

/** Reports a memory-related error with the line info that was passed. */
#define MEMORY_ERROR_ARGS(info, ...) \
    (internal_error(info, "Memory error", __VA_ARGS__))

/** Reports a memory-related error where it was called. */
#define MEMORY_ERROR(...) (MEMORY_ERROR_ARGS(LINE_INFO, __VA_ARGS__))

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
        SOURCE_INFO, "Unreachable error", \
        "Reached an unreachable part of Zymux's internal code " \
        "(Please report this error to the developer(s) of Zymux, as this should never appear)" \
    ))

/** Reports a file-related error. */
#define FILE_ERROR(...) (file_error(__VA_ARGS__))

/** A user syntax error. Typically means an error occurred in lexing or parsing. */
#define SYNTAX_ERROR(program, pos, message) \
    (zmx_user_error((program), (pos), "Syntax error", (message)))

/** Places an instantiated struct that holds information of a line in C source code. */
#define SOURCE_INFO ((SourceInfo){.file = __FILE__, .func = __func__, .line = __LINE__})

/** Forward declaration to avoid circular include. */
typedef struct ZmxProgram ZmxProgram;

/** Represents some information for a position in some source (like the line and file it's on). */
typedef struct {
    const char *file;
    const char *func;
    const int line;
} SourceInfo;

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
void internal_error(const SourceInfo info, const char *errorName, const char *format, ...);

/** Displays a file error. */
void file_error(const char *format, ...);

/** Displays an error that happened due to a mistake from the person using Zymux in a *.zmx file. */
void zmx_user_error(
    ZmxProgram *program, SourcePosition pos, const char *errorName, const char *format, ...
);

#endif
