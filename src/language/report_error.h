#ifndef REPORT_ERROR_H
#define REPORT_ERROR_H

#include <stdbool.h>
#include <stdarg.h>
#include <stdlib.h>

#include "constants.h"

/** Places an instantiated struct that holds information of a line in C source code. */
#define SOURCE_INFO ((SourceInfo){.file = __FILE__, .func = __func__, .line = __LINE__})

#if ASSERTS_ENABLED
    /** 
     * Asserts a passed condition and spits an assert error if it's falsy with a message.
     * Currently enabled.
     */
    #define ASSERT(condition, message) (zmx_assert(SOURCE_INFO, condition, message))
#else
    /** 
     * Asserts a passed condition and spits an assert error if it's falsy with a message.
     * Currently disabled, so no assertions are actually being executed (for performance).
     */
    #define ASSERT(condition, message)
#endif

/** Reports an error relating to the user's operating system.*/
#define OS_ERROR(...) (os_error(__VA_ARGS__))

/** Reports a memory-related error with the line info that was passed. */
#define MEMORY_ERROR_ARGS(info, ...) \
    (internal_error(info, "Memory error", __VA_ARGS__))

/** Reports a memory-related error where it was called. */
#define MEMORY_ERROR(...) (MEMORY_ERROR_ARGS(SOURCE_INFO, __VA_ARGS__))

/** 
 * Reports an error where a part of the implementation that wasn't meant to be executed was reached.
 * 
 * An example would be reaching a default case on a switch that should've covered a whole enum.
 * 
 * Unlike file errors or memory errors which may be caused by something in the user's computer
 * (like running out of memory), unreachable errors are never ever meant to be executed,
 * so when seen they should be fixed immediately.
 * 
 * This macro also aborts afterwards,
 * we do that so the compiler doesn't complain about not returning values.
 */
#define UNREACHABLE_ERROR() \
    (internal_error( \
        SOURCE_INFO, "Unreachable error", \
        "Reached an unreachable part of Zymux's internal code " \
        "(Please report this error to the developer(s) of Zymux, as this should never appear)" \
    ), abort())

#if UNREACHABLE_DEFAULT_ENABLED
    /** 
     * Allows enum-exhaustive switches to toggle the default case for compiler warnings.
     * 
     * Currently enabled, so switches that use this will have a default case
     * which spits an unreachable if it occurs.
     */
    #define TOGGLEABLE_DEFAULT_UNREACHABLE() default: UNREACHABLE_ERROR()
#else
    /** 
     * Allows enum-exhaustive switches to toggle on/off the default case for compiler warnings.
     * 
     * Currently disabled, so switches that use this will not change in any way.
     * This is mostly to allow the compiler to spit warnings on switches that are supposed to fully
     * exhaust an enum, so we don't forget to add any enums to a switch.
     */
    #define TOGGLEABLE_DEFAULT_UNREACHABLE()
#endif

/** Reports a file-related error. */
#define FILE_ERROR(...) (file_error(__VA_ARGS__))

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
SourcePosition create_src_pos(const int line, const int column, const int length);

/** Checks whether pos1 and pos2 are the same. */
bool equal_position(const SourcePosition pos1, const SourcePosition pos2);

/** Displays an error relating to the user's operating system. */
void os_error(const char *format, ...);

/** Displays an internal error that occurred in the C implementation of Zymux. */
void internal_error(const SourceInfo info, const char *errorName, const char *format, ...);

/** Displays a file error. */
void file_error(const char *format, ...);

/** 
 * Displays an error that happened due to a mistake from the person using Zymux in a *.zmx file.
 * 
 * Takes in the args directly as this is supposed to be a base error function,
 * which could be wrapped around for extra functionality regarding errors during other stages
 * of Zymux.
 */
void zmx_user_error(
    ZmxProgram *program, const SourcePosition pos, const char *errorName,
    const char *format, va_list *args
);

/** Asserts a condition and errors with the passed message if the condition is false. */
void zmx_assert(const SourceInfo info, const bool condition, const char *message);

#endif
