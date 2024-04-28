#ifndef CONSTANTS_H
#define CONSTANTS_H

#include <inttypes.h>
#include <stdint.h>

#define UNKNOWN_OS 0
#define UNIX_OS 1
#define WINDOWS_OS 2

#if defined(__unix__) || defined(__unix) || defined(unix) || defined(__APPLE__)
#define OS UNIX_OS
#define PATH_DELIMITER '/'

#elif defined(_WIN32) || defined(__CYGWIN__)
#define OS WINDOWS_OS
#define PATH_DELIMITER '\\'

#else
#define OS UNKNOWN_OS
#define PATH_DELIMITER '\0'

#endif

#define DEFAULT_COLOR "\033[0m"
#define GREEN "\033[32"
#define RED "\033[31m"
#define YELLOW "\033[33m"

#define DEBUG_LEXER 1
#define DEBUG_PARSER 1
#define DEBUG_COMPILER 1
#define DEBUG_RUNTIME 1
#define DEBUG_GC 1

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

/** Converts the passed pointer to a pointer to the passed type. */
#define AS_PTR(value, type) ((type *)value)

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
