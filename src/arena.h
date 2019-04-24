#pragma once

typedef struct Arena Arena;
struct Arena {
    u64 size;
    char *ptr;
    char *end;
    char **blocks;
};

void arena_free(Arena *arena);
void *arena_calloc(Arena *arena, size_t size);
void *arena_alloc(Arena *arena, size_t size);
