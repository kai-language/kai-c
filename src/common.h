#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_WIN32) || defined(_WIN64)
	#ifndef SYSTEM_WINDOWS
	#define SYSTEM_WINDOWS 1
	#endif
#elif defined(__APPLE__) && defined(__MACH__)
    #ifndef SYSTEM_POSIX
    #define SYSTEM_POSIX 1
    #endif
	
    #ifndef SYSTEM_OSX
	#define SYSTEM_OSX 1
	#endif
#elif defined(__unix__)
	#ifndef SYSTEM_POSIX
    #define SYSTEM_POSIX 1
    #endif

    #ifndef SYSTEM_UNIX
	#define SYSTEM_UNIX 1
	#endif

	#if defined(__linux__)
		#ifndef SYSTEM_LINUX
		#define SYSTEM_LINUX 1
		#endif
	#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
		#ifndef SYSTEM_FREEBSD
		#define SYSTEM_FREEBSD 1
		#endif
	#else
		#error This UNIX operating system is not supported
	#endif
#else
	#error This operating system is not supported
#endif

#if SYSTEM_POSIX
    #include <fcntl.h>
    #include <sys/stat.h>
    #include <sys/mman.h>
    #include <execinfo.h>
    #include <limits.h>
    #include <unistd.h>
    #include <sys/utsname.h> // uname to default arch & os to current
#elif SYSTEM_WINDOWS
    #include <windows.h>
#endif

#ifndef MAX_PATH
    #if defined _MAX_PATH
        #define MAX_PATH _MAX_PATH
    #elif defined PATH_MAX
        #define MAX_PATH PATH_MAX
    #else
        #error "No suitable MAX_PATH surrogate"
    #endif
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

#define CONCAT(x,y) x##y

#define KB(x) (  (x)*1024LL)
#define MB(x) (KB(x)*1024LL)
#define GB(x) (MB(x)*1024LL)
#define TB(x) (GB(x)*1024LL)

#define For(agg) for (size_t i = 0; i < ArrayLen(agg); i++)

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;

typedef float    f32;
typedef double   f64;

typedef i8       b8;
typedef i32      b32;

void assertHandler(char const *file, i32 line, char const *msg, ...);

#if defined(_MSC_VER)
    #if _MSC_VER < 1300
    #define DEBUG_TRAP() __asm int 3
    #else
    #define DEBUG_TRAP() __debugbreak()
    #endif
#else
    #define DEBUG_TRAP() __builtin_trap()
#endif

#ifndef TEST
#if !defined(RELEASE) && !defined(ASSERTS)
    #define ASSERT_MSG_VA(cond, msg, ...) do { \
        if (!(cond)) { \
            assertHandler(__FILE__, (i32)__LINE__, msg, __VA_ARGS__); \
            DEBUG_TRAP(); \
        } \
    } while(0)

    #define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, "(" #cond ") " msg, 0)

    #define ASSERT(cond) ASSERT_MSG_VA(cond, 0, 0)
    #define PANIC(msg) ASSERT_MSG_VA(0, msg, 0)
    #define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented", 0);
#else
    #define ASSERT_MSG_VA(cond, msg, ...)
    #define ASSERT_MSG(cond, msg)
    #define ASSERT(cond)
    #define PANIC(msg)
    #define UNIMPLEMENTED()
#endif
#endif

#if !defined(Inline)
    #if defined(_MSC_VER)
        #if _MSC_VER < 1300
        #define Inline
        #else
        #define Inline __forceinline
        #endif
    #else
        #define Inline __attribute__ ((__always_inline__))
    #endif
#endif


extern bool FlagVerbose;
// Use this to print a message when in verbose mode and returning NULL
#define NullWithLoggedReason(msg, ...) \
    (FlagVerbose ? ((void)printf("return NULL from %s: " msg "\n", __FUNCTION__, __VA_ARGS__), NULL) : NULL)

typedef struct Position Position;
struct Position {
    const char *name;
    u32 offset, line, column;
};

/// Allocators
typedef enum AllocType {
    AT_Alloc,
    AT_Calloc,
    AT_Realloc,
    AT_Free,
    AT_FreeAll,
} AllocType;


// NOTE: count is the target bytes always, size is either the size in bytes of each entry (for calloc) or the old size for realloc.
#define ALLOC_FUNC(name) void *name(void *payload, enum AllocType alType, size_t count, size_t size, void *old)
typedef void *allocFunc(void *payload, enum AllocType alType, size_t count, size_t size, void *old);
//typedef ALLOC_FUNC(allocFunc);

typedef struct Allocator Allocator;
struct Allocator {
    allocFunc *func;
    void *payload;
};

// Arena Allocator

typedef struct Arena Arena;
struct Arena {
    u8 *ptr;
    u8 *end;
    u8 **blocks;
};

void *ArenaAlloc(Arena *arena, size_t size);
void *ArenaCalloc(Arena *arena, size_t size);
void ArenaFree(Arena *arena);

#include "array.h"
#include "map.h"

typedef struct DiagnosticError DiagnosticError;
typedef struct DiagnosticEngine DiagnosticEngine;
struct DiagnosticEngine {
    Arena arena;
    DynamicArray(DiagnosticError) errors;
};

typedef struct Symbol Symbol;
typedef struct Stmt Stmt;
typedef struct CheckerInfo CheckerInfo;

typedef struct Scope Scope;
struct Scope {
    Scope *parent;
    Map members;
};

typedef struct Package Package;
struct Package {
    const char *path;
    char fullPath[MAX_PATH];
    const char *externalName;
    DiagnosticEngine diagnostics;
    Arena arena;
    DynamicArray(Stmt *) stmts;
    Map symbolMap;
    DynamicArray(Symbol *) symbols;

    u64 astIdCount;
    u64 declCount;
    DynamicArray(CheckerInfo) checkerInfo;

    Scope *globalScope;
};

typedef union Val {
    b32 b32;
    i8 i8;
    u8 u8;
    i16 i16;
    u16 u16;
    i32 i32;
    u32 u32;
    i64 i64;
    u64 u64;
    f32 f32;
    f64 f64;
    uintptr_t ptr;
} Val;

char *GetFileName(const char *path, char *res, char **dir);

#ifdef __cplusplus
}
#endif
