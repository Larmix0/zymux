#ifndef CONSTANTS_H
#define CONSTANTS_H

#include <inttypes.h>
#include <stdint.h>

#define UNKNOWN_OS 0
#define UNIX_OS 1
#define WINDOWS_OS 2

#if defined(__unix__) || defined(__unix) || defined(unix) || defined(__APPLE__)
    #define OS UNIX_OS
    #define PATH_SEPARATOR '/'
#elif defined(_WIN32) || defined(__CYGWIN__)
    #define OS WINDOWS_OS
    #define PATH_SEPARATOR '\\'
#else
    #define OS UNKNOWN_OS
    #define PATH_SEPARATOR '/'
#endif

#define DEFAULT_COLOR "\033[0m"
#define GREEN "\033[32"
#define RED "\033[31m"
#define YELLOW "\033[33m"

#define DEBUG_TOKENS 0
#define DEBUG_AST 0
#define DEBUG_BYTECODE 0
#define DEBUG_RUNTIME 0
#define DEBUG_LOG_GC 0 /** Logs GC actions during a garbage collection. */
#define DEBUG_ALWAYS_GC 1 /** Performs a garbage collection after every object allocation. */

/** Allows assertions or disables them from the code for performance. */
#define ASSERTS_ENABLED 1

/** Allows us to disable default unreachables on enum-exhaustive switches so they throw warnings. */
#define UNREACHABLE_DEFAULT_ENABLED 0

#define I8_MAX INT8_MAX
#define I16_MAX INT16_MAX
#define I32_MAX INT32_MAX
#define I64_MAX INT64_MAX

#define U8_MAX UINT8_MAX
#define U16_MAX UINT16_MAX
#define U32_MAX UINT32_MAX
#define U64_MAX UINT64_MAX

#define ZMX_INT_FMT "%" PRId64
#define ZMX_FLOAT_FMT "%lf"

#define MAX_PATH_LENGTH U8_MAX

/** Gets rid of a compiler warning for a variable which was declared but never used. */
#define UNUSED_VARIABLE(parameter) ((void)(parameter))

/** The string of characters (file extension) which denotes a language file. */
#define EXTENSION ".zmx"

/** Converts the passed pointer to a pointer of the passed type. */
#define AS_PTR(type, value) ((type *)value)

/** 
 * Turns into a boolean of whether or not the passed index is within the passed length.
 * The length is presumed to be that of some collection, where the highest index is length - 1.
 */
#define IS_WITHIN_LENGTH(length, index) ((index) >= 0 && (index) <= (length) - 1)

/** Used when indenting text during error reporting and debugging. */
#define INDENT "    "

/** How much should a data structure grow per allocation (to be used with multiplication). */
#define CAPACITY_GROWTH 2

/** Minimum capacity of a data structure. */
#define MINIMUM_CAPACITY 16

/** Increases the passed capacity field/variable's size. */
#define INCREASE_CAPACITY(capacity) \
    ((capacity) = (capacity) < MINIMUM_CAPACITY ? MINIMUM_CAPACITY : (capacity) * CAPACITY_GROWTH)

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef i64 ZmxInt;
typedef double ZmxFloat;

#endif
