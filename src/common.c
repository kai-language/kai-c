
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>
#include <unistd.h>
#include <stdbool.h>
#include <wchar.h>

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
#elif SYSTEM_WINDOWS

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

    #define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, msg, 0)

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

void Backtrace() {
#define BACKTRACE_MAX_STACK_DEPTH 50
#if SYSTEM_POSIX
    void* callstack[BACKTRACE_MAX_STACK_DEPTH];
    int i, frames = backtrace(callstack, BACKTRACE_MAX_STACK_DEPTH);
    char** strs = backtrace_symbols(callstack, frames);
    for (i = 0; i < frames; ++i) {
        fprintf(stderr, "%s\n", strs[i]);
    }
    free(strs);
#elif SYSTEM_WINDOWS
    UNIMPLEMENTED();
#endif
}

void assertHandler(char const *file, i32 line, char const *msg, ...) {
    va_list args;
    va_start(args, msg);
    Backtrace();
    
    if (msg) {
        fprintf(stderr, "Assert failure: %s:%d: ", file, line);
        vfprintf(stderr, msg, args);
        fprintf(stderr, "\n");
    } else {
        fprintf(stderr, "Assert failure: %s:%d\n", file, line);
    }
    va_end(args);
}

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

void *checkedCalloc(size_t num_elems, size_t elem_size) {
    void *ptr = calloc(num_elems, elem_size);
    if (!ptr) {
        perror("calloc failed");
        exit(1);
    }
    return ptr;
}

void *checkedRealloc(void *ptr, size_t num_bytes) {
    ptr = realloc(ptr, num_bytes);
    if (!ptr) {
        perror("realloc failed");
        exit(1);
    }
    return ptr;
}

void *checkedMalloc(size_t num_bytes) {
    void *ptr = malloc(num_bytes);
    if (!ptr) {
        perror("malloc failed");
        exit(1);
    }
    return ptr;
}

void *heapAllocFunc(void *payload, enum AllocType alType, size_t count, size_t size, void *old) {
    switch (alType) {
        case AT_Alloc:
            return checkedMalloc(count);
        case AT_Calloc:
            return checkedCalloc(count, size);
        case AT_Free:
        case AT_FreeAll:
            free(old);
            return NULL;
        case AT_Realloc:
            return checkedRealloc(old, count);
    }
    return NULL;
}

Allocator DefaultAllocator = { .func = heapAllocFunc, .payload = 0 };

void *Alloc(Allocator al, size_t count) {
    return al.func(al.payload, AT_Alloc, count, 0, NULL);
}

void *Calloc(Allocator al, size_t count, size_t size) {
    return al.func(al.payload, AT_Calloc, count, size, NULL);
}

void *Free(Allocator al, void* ptr) {
    if (ptr)
        al.func(al.payload, AT_Free, 0, 0, ptr);
    return NULL;
}

void *FreeAll(Allocator al) {
    al.func(al.payload, AT_FreeAll, 0, 0, NULL);
    return NULL;
}

void *Realloc(Allocator al, void *ptr, size_t size, size_t oldsize) {
    return al.func(al.payload, AT_Realloc, size, oldsize, ptr);
}

#include "flags.c"
#include "map.c"
#include "array.c"
#include "utf.c"
#include "error.c"

// Arena Allocator

typedef struct Arena Arena;
struct Arena {
    u8 *ptr;
    u8 *end;
    u8 **blocks;
};

#include "string.c"

#define ARENA_BLOCK_SIZE MB(1)
#define ARENA_ALIGNMENT 8

void ArenaGrow(Arena *arena, size_t minSize, b32 clear) {
    size_t size = ALIGN_UP(CLAMP_MIN(minSize, ARENA_BLOCK_SIZE), ARENA_ALIGNMENT);
    arena->ptr = clear ? Calloc(DefaultAllocator, 1, size) : Alloc(DefaultAllocator, size);
    ASSERT(arena->ptr == ALIGN_DOWN_PTR(arena->ptr, ARENA_ALIGNMENT));
    arena->end = arena->ptr + size;
    ArrayPush(arena->blocks, arena->ptr);
}

void *ArenaAlloc(Arena *arena, size_t size) {
    if (size > (size_t)(arena->end - arena->ptr)) {
        ArenaGrow(arena, size, false);
        ASSERT(size <= (size_t)(arena->end - arena->ptr));
    }
    void *allocation = arena->ptr;
    arena->ptr = (u8*) ALIGN_UP_PTR(arena->ptr + size, ARENA_ALIGNMENT);
    ASSERT(arena->ptr <= arena->end);
    ASSERT_MSG_VA(allocation == ALIGN_DOWN_PTR(allocation, ARENA_ALIGNMENT), "The pointer %p should be aligned to %d", allocation, ARENA_ALIGNMENT);
    return allocation;
}

void *ArenaCalloc(Arena *arena, size_t size) {
    b32 needMemset = true;
    if (size > (size_t)(arena->end - arena->ptr)) {
        ArenaGrow(arena, size, true);
        ASSERT(size <= (size_t)(arena->end - arena->ptr));
        needMemset = false;
    }
    void *allocation = arena->ptr;
    arena->ptr = (u8*) ALIGN_UP_PTR(arena->ptr + size, ARENA_ALIGNMENT);
    ASSERT(arena->ptr <= arena->end);
    ASSERT_MSG_VA(allocation == ALIGN_DOWN_PTR(allocation, ARENA_ALIGNMENT), "The pointer %p should be aligned to %d", allocation, ARENA_ALIGNMENT);
    if (needMemset) memset(allocation, 0, size);
    return allocation;
}

void ArenaFree(Arena *arena) {
    arena->ptr = NULL;
    arena->end = NULL;
    for (u8 **block = arena->blocks; block != ArrayEnd(arena->blocks); block++) {
        Free(DefaultAllocator, *block);
    }
    ArrayFree(arena->blocks);
}

#if TEST
void test_arena() {
    u64 bytes = MB(1);
    Arena arena = {0};

    u64 N = 1024;
    u64* mem = (u64*) ArenaAlloc(&arena, bytes);
    for (int i = 0; i < N; i++) {
        mem[i] = i;
    }

    ArenaFree(&arena);

    TEST_ASSERT(arena.ptr == NULL);
    TEST_ASSERT(arena.end == NULL);
    TEST_ASSERT(arena.blocks == NULL);
}
#endif



void PrintBits(u64 const size, void const * const ptr) {
    u8 *b = (u8*) ptr;
    u8 byte;
    i64 i, j;
    
    for (i=size-1;i>=0;i--)
    {
        for (j=7;j>=0;j--)
        {
            byte = (b[i] >> j) & 1;
            printf("%u", byte);
        }
    }
    puts("");
}

char *AbsolutePath(const char *fileName, char *resolved) {
    return realpath(fileName, resolved);
}

#define NullWithLoggedReason(msg, ...) \
    (FlagVerbose ? ((void)printf("return NULL from %s: " msg "\n", __FUNCTION__, __VA_ARGS__), NULL) : NULL)

// FIXME: We are mmap()'ing this with no way to munmap it currently
char *ReadFile(const char *path) {
    i32 fd = open(path, O_RDONLY);
    if (fd == -1) return NullWithLoggedReason("failed to open file %s", path);

    struct stat st;
    if (stat(path, &st) == -1) return NullWithLoggedReason("Failed to stat already opened file %s with file descriptor %d", path, fd);
    size_t len = st.st_size;

    char *address = (char*) mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    if (address == MAP_FAILED) return NullWithLoggedReason("Failed to mmap opened file %s", path);

    return address;
}

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
    uintptr_t ptr;
} Val;
