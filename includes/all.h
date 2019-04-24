#pragma once

#if defined(__unix__) || defined(__APPLE__)
#   define __unix__
#   define _GNU_SOURCE // We need realpath defined
#   include <unistd.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <wchar.h>

#if defined(__unix__)
#   include <fcntl.h>
#   include <sys/stat.h>
#   include <sys/mman.h>
#   include <execinfo.h>
#   include <limits.h>
#   include <sys/utsname.h> 
#   include <dirent.h>
#endif

#if defined(_WIN32) || defined(_WIN64)
#   include <windows.h>
#   include <crtdbg.h>
#   include <io.h>
#   include <errno.h>
#endif

#ifndef STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#endif

#ifndef PROFILER_IMPLEMENATION
#include "profiler.h"
#endif

typedef uint8_t  u8;  // Unsigned ints
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   i8;  // Signed ints
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef float    f32;
typedef double   f64;
typedef i8       b8;
typedef i32      b32;

#define VERSION "0.0.0 (prerelease)"

#if defined(__APPLE__)
#   define __unix__
#endif

#if defined(_WIN32) || defined(_WIN64)
#   define STATIC_ASSERT(cond) _STATIC_ASSERT(cond)
#else
#   define STATIC_ASSERT(cond) _Static_assert(cond, #cond " was false")
#endif

#ifndef MAX_PATH
#   define MAX_NAME 255
#   define MAX_PATH 4096
#endif

#if !defined(INLINE)
#   if defined(_MSC_VER)
#       if _MSC_VER < 1300
#           define INLINE
#       else
#           define INLINE __forceinline
#       endif
#   else
#       define INLINE __attribute__ ((__always_inline__))
#   endif
#endif

#define MIN(x, y) ((x) <= (y) ? (x) : (y))
#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define CLAMP_MAX(x, max) MIN(x, max)
#define CLAMP_MIN(x, min) MAX(x, min)
#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a) - 1, (a))
#define ALIGN_DOWN_PTR(p, a) ((void *)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void *)ALIGN_UP((uintptr_t)(p), (a)))

#define NONE 0
#define INVALID 0

void *xmalloc(size_t num_bytes);
void *xcalloc(size_t num_bytes);
void *xrealloc(void *ptr, size_t num_bytes);

#if defined(_MSC_VER)
#   if _MSC_VER < 1300
#       define DEBUG_TRAP() __asm int 3
#   else
#       define DEBUG_TRAP() __debugbreak()
#   endif
#else
#   define DEBUG_TRAP() __builtin_trap()
#endif

#ifndef TEST
#   ifdef DEBUG
#       define ASSERT_MSG_VA(cond, msg, ...) if (!(cond)) (assertHandler(__FILE__, (i32)__LINE__, msg, __VA_ARGS__), DEBUG_TRAP());
#       define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, "(" #cond ") " msg, 0)
#       define ASSERT(cond) ASSERT_MSG_VA(cond, 0, 0)
#       define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented", 0);
#   else
#       define ASSERT_MSG_VA(cond, msg, ...)
#       define ASSERT_MSG(cond, msg)
#       define ASSERT(cond)
#       define UNIMPLEMENTED()
#   endif
#endif

// From debug.h
INLINE void verbose(const char *fmt, ...);
void Backtrace(void);
_Noreturn void fatal(const char *fmt, ...);
void warn(const char *fmt, ...);
_Noreturn void fatal(const char *fmt, ...);
void assertHandler(char const *file, i32 line, char const *msg, ...);
