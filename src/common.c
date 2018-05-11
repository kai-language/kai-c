
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>

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
#elif SYSTEM_WINDOWS

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
    Backtrace();
    
    if (msg) {
        fprintf(stderr, "Assert failure: %s:%d: %s\n", file, line, msg);
    } else {
        fprintf(stderr, "Assert failure: %s:%d\n", file, line);
    }
}


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


// Arena Allocator

typedef struct Arena Arena;
struct Arena {
    Allocator allocator;
    u8  *raw;
    u64 cap;
    u64 len;
};

void *arenaAllocFunc(void *payload, enum AllocType alType, size_t count, size_t size, void *old) {
    Arena *arena = (Arena *) payload;

    switch (alType) {
        case AT_Alloc: {
            if (arena->len + count > arena->cap) return NULL;
            u8 *ptr = &arena->raw[arena->len];
            arena->len += count;
            return ptr;
        }
        case AT_Calloc: {
            size_t bytes = count * size;
            ASSERT(bytes >= count || size == 0);
            if (arena->len + bytes > arena->cap) return NULL;
            u8 *ptr = &arena->raw[arena->len];
            arena->len += bytes;
            memset(ptr, 0, bytes);
            return ptr;
        }
        case AT_Free:
        case AT_FreeAll: {
            arena->len = 0;
            break;
        }
        case AT_Realloc: {
            u8 *buff = (u8 *) Realloc(arena->allocator, arena->raw, count, size);
            arena->raw = buff;
            arena->cap = size;
            return buff;
        }
    }
    
    return NULL;
}

Allocator MakeArenaAllocator(Arena *arena) {
    Allocator al;
    al.func    = arenaAllocFunc;
    al.payload = arena;
    return al;
}


void InitArenaCustomAllocator(Arena *arena, Allocator al, u64 size) {
    arena->allocator = al;
    arena->raw = (u8 *) Alloc(al, size);
    arena->cap = size;
    arena->len = 0;
}

void InitArena(Arena *arena, u64 size) {
    InitArenaCustomAllocator(arena, DefaultAllocator, size);
}

void DestroyArena(Arena *arena) {
    ASSERT(arena->raw);
    arena->raw = (typeof arena->raw) Free(arena->allocator, arena->raw);
    arena->len = 0;
    arena->cap = 0;
}

#if TEST
void test_arena() {
    u64 bytes = MB(1);
    Arena arena;
    InitArena(&arena, bytes);
    TEST_ASSERT(arena.cap == bytes);

    Allocator al = MakeArenaAllocator(&arena);

    u64 N = 1024;
    u64* mem = (u64*) Alloc(al, bytes);
    TEST_ASSERT(arena.len == bytes);
    for (int i = 0; i < N; i++) {
        mem[i] = i;
    }

    arena.len = 0;
    u64 *ptr = (u64*) Calloc(al, N, sizeof(u64));

    TEST_ASSERT(arena.len == N * sizeof(u64));
    for (int i = 0; i < N; i++) {
        TEST_ASSERT(ptr[i] == 0);
    }

    DestroyArena(&arena);

    TEST_ASSERT(arena.len == 0);
    TEST_ASSERT(arena.cap == 0);
    TEST_ASSERT(arena.raw == NULL);
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

void testAssertHandler(char const *file, i32 line, char const *msg, ...) {
    Backtrace();

    if (msg) {
        fprintf(stderr, "Assert failure: %s:%d: %s\n", file, line, msg);
    } else {
        fprintf(stderr, "Assert failure: %s:%d\n", file, line);
    }
}



#include "string.cpp"
#include "utf.c"
#include "array.c"
#include "map.c"

b32 ReadFile(String *data, String path) {
    i32 file;
    if ((file = open((const char *)path.data, O_RDONLY)) < -1) {
        return false;
    }

    struct stat buf;
    if (fstat(file, &buf) < 0 ) {
        return false;
    }

    u64 len = buf.st_size;
    void *address = mmap(NULL, len, PROT_READ, MAP_PRIVATE, file, 0);
    if (address == MAP_FAILED)
        return false;

    *data = MakeString((u8 *) address, (u32)len);

    return true;
}
