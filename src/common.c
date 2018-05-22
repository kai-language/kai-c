#include "common.h"

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

Allocator DefaultAllocator = { heapAllocFunc, 0 };

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

#include "targets.c"
#include "os.c"
#include "map.c"
#include "array.h"
#include "queue.c"
#include "flags.c"
#include "utf.c"
#include "error.c"
#include "string.c"

#define ARENA_BLOCK_SIZE MB(1)
#define ARENA_ALIGNMENT 8

void ArenaGrow(Arena *arena, size_t minSize, b32 clear) {
    size_t size = ALIGN_UP(CLAMP_MIN(minSize, ARENA_BLOCK_SIZE), ARENA_ALIGNMENT);
    arena->ptr = (u8 *)(clear ? Calloc(DefaultAllocator, 1, size) : Alloc(DefaultAllocator, size));
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

    u64* mem = (u64*) ArenaAlloc(&arena, bytes);
    for (int i = 0; i < 1024; i++) {
        mem[i] = i;
    }

    ArenaFree(&arena);

    ASSERT(arena.ptr == NULL);
    ASSERT(arena.end == NULL);
    ASSERT(arena.blocks == NULL);
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

