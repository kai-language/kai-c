#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <strings.h>

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

#define ArrayCount(Array) (sizeof(Array) / sizeof((Array)[0]))
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


#define MAXF(a,b) ((a) > (b) ? (a) : (b))
#define MINF(a,b) ((a) < (b) ? (a) : (b))


void Backtrace() {
#if SYSTEM_POSIX
    void* callstack[25];
    int i, frames = backtrace(callstack, ArrayCount(callstack));
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
    }
    else {
        fprintf(stderr, "Assert failure: %s:%d\n", file, line);
    }
}


typedef enum AllocType {
    AT_Alloc,
    AT_Free,
    AT_FreeAll,
    AT_Realloc
} AllocType;


struct Allocator;
struct Arena;

void * Alloc(Allocator, u64);
void Free(Allocator, void *);
void FreeAll(Allocator);
void * Realloc(Allocator, void *, u64, u64);


#define ALLOC_FUNC(name) void *name(void *payload, enum AllocType alType, u64 size, u64 oldSize, void *old)
typedef ALLOC_FUNC(allocFunc);


struct Allocator {
    allocFunc *func;
    void *payload;
};

struct Arena {
    Allocator allocator;
    u8  *raw;
    u64 cap;
    u64 len;
};


ALLOC_FUNC(arenaAllocFunc) {
    Arena *arena = (Arena *) payload;
    
    switch (alType) {
        case AT_Alloc: {
            if (arena->len + size > arena->cap) {
                return NULL;
            }
            u8 *ptr = &arena->raw[arena->len];
            arena->len += size;
            return ptr;
        }
        case AT_Free:
        case AT_FreeAll: {
            arena->len = 0;
            break;
        }
        case AT_Realloc: {
            u8 *buff = (u8 *) Realloc(arena->allocator, arena->raw, oldSize, size);
            arena->raw = buff;
            arena->cap = size;
            return buff;
        }
    }
    
    return NULL;
}


ALLOC_FUNC(heapAllocFunc) {
    
    switch (alType) {
        case AT_Alloc:
            return malloc(size);
        case AT_Free: {
            free(old);
            break;
        }
        case AT_FreeAll: {
            break;
        }
        case AT_Realloc:
            return realloc(old, size);
    }
    
    return NULL;
}


Allocator InitArenaAllocator(Arena *arena) {
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


Allocator MakeDefaultAllocator(void) {
    Allocator al = {0};
    al.func = heapAllocFunc;
    return al;
}


void InitArena(Arena *arena, u64 size) {
    InitArenaCustomAllocator(arena, MakeDefaultAllocator(), size);
}


void DestroyArena(Arena *arena) {
    ASSERT(arena->raw);
    Free(arena->allocator, arena->raw);
    arena->len = 0;
    arena->cap = 0;
}


void * Alloc(Allocator al, u64 size) {
    return al.func(al.payload, AT_Alloc, size, 0, NULL);
}


void Free(Allocator al, void *ptr) {
    if ( ptr )
        al.func(al.payload, AT_Free, 0, 0, ptr);
}


void FreeAll(Allocator al) {
    al.func(al.payload, AT_FreeAll, 0, 0, NULL);
}


void * Realloc(Allocator al, void *ptr, u64 oldsize, u64 size) {
    return al.func(al.payload, AT_Realloc, size, oldsize, ptr);
}


void PrintBits(u64 const size, void const * const ptr) {
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
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


#include "string.cpp"
#include "utf.cpp"
#include "array.cpp"
#include "hash.cpp"
#include "map.cpp"


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

