#ifndef ERRORS_H
#define ERRORS_H

#include "zymux_program.h"

#define MEMORY_ERROR_ARGS(file, func, line, ...) \
    (internal_error(file, func, line, 1, "Memory error", __VA_ARGS__))
#define MEMORY_ERROR(...) \
    (MEMORY_ERROR_ARGS(__FILE__, __func__, __LINE__, __VA_ARGS__))

#define FILE_ERROR(...) \
    (internal_error(__FILE__, __func__, __LINE__, 1, "File IO error", __VA_ARGS__))
#define UNREACHABLE_ERROR() \
    (internal_error( \
        __FILE__, __func__, __LINE__, 1, "Unreachable error", \
        "Reached an unreachable part of Zymux's internal code " \
        "(Please report this error to the developer(s) of Zymux, as this should never appear)" \
    ))

#define SYNTAX_ERROR(program, line, ...) \
    (user_error((program), line, "Syntax error", __VA_ARGS__))

void internal_error(
    const char *file, const char *func, const int line,
    const int exitCode, const char *errorName, const char *format, ...
);
void user_error(
    ZymuxProgram *program, const int line, const char *errorName, const char *format, ...
);

#endif
