
#include "all.h"
#include "arena.h"

#define ARENA_ALIGNMENT 8
#define ARENA_BLOCK_SIZE (1024 * 1024)

void arena_grow(Arena *arena, size_t min_size) {
    size_t size = ALIGN_UP(CLAMP_MIN(min_size, ARENA_BLOCK_SIZE), ARENA_ALIGNMENT);
    arena->size += size;
    arena->ptr = xmalloc(size);
    ASSERT(arena->ptr == ALIGN_DOWN_PTR(arena->ptr, ARENA_ALIGNMENT));
    arena->end = arena->ptr + size;
    arrput(arena->blocks, arena->ptr);
}

void *arena_alloc(Arena *arena, size_t size) {
    if (size > (size_t)(arena->end - arena->ptr)) {
        arena_grow(arena, size);
        ASSERT(size <= (size_t)(arena->end - arena->ptr));
    }
    void *ptr = arena->ptr;
    arena->ptr = ALIGN_UP_PTR(arena->ptr + size, ARENA_ALIGNMENT);
    ASSERT(arena->ptr <= arena->end);
    ASSERT(ptr == ALIGN_DOWN_PTR(ptr, ARENA_ALIGNMENT));
    return ptr;
}

void *arena_calloc(Arena *arena, size_t size) {
    void *mem = arena_alloc(arena, size);
    memset(mem, 0, size);
    return mem;
}

void arena_free(Arena *arena) {
    for (char **it = arena->blocks; it != arrlen(arena->blocks) + arena->blocks; it++) {
        free(*it);
    }
    arrfree(arena->blocks);
}
