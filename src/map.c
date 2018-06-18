Inline
u64 HashU64(u64 x) {
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;
    return x;
}

Inline
u64 HashPtr(const void *ptr) {
    return HashU64((uintptr_t)ptr);
}


// FIXME: When 2 hashes are the same, they cancel out
Inline
u64 HashMix(u64 x, u64 y) {
    x ^= y;
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;
    return x;
}

Inline
u64 HashBytes(const void *ptr, size_t len) {
    u64 x = 0xcbf29ce484222325;
    const char *buf = (const char *)ptr;
    for (size_t i = 0; i < len; i++) {
        x ^= buf[i];
        x *= 0x100000001b3;
        x ^= x >> 32;
    }
    return x;
}

void MapFree(Map *map) {
    // TODO(Brett): figure out who's being naughty here
//    if (map->allocator) {
//        Free(*map->allocator, map->keys);
//        Free(*map->allocator, map->vals);
//    }

    map->len = 0;
    map->cap = 0;
}

void MapCopy(Allocator *alloc, Map *dest, Map *source) {
    Allocator *a = alloc ? alloc : source->allocator;
    Map map;

    map.allocator = a;
    map.len = source->len;
    map.cap = source->cap;

    u64 *keys = Alloc(*a, sizeof(u64) * map.cap);
    if (!keys) return;

    u64 *vals = Alloc(*a, sizeof(u64) * map.cap);
    if (!vals) {
        Free(*a, keys);
    }

    memcpy(keys, source->keys, sizeof(u64) * map.cap);
    memcpy(vals, source->vals, sizeof(u64) * map.cap);

    map.keys = keys;
    map.vals = vals;

    *dest = map;
}

void MapGrow(Map *map, size_t newCap) {
    newCap = CLAMP_MIN(newCap, 16);
    Allocator *al = map->allocator ? map->allocator : &DefaultAllocator;
    Map new_map = {
        al,
        checkedCalloc(newCap, sizeof(u64)),
        checkedMalloc(newCap * sizeof(u64)),
        0,
        newCap,
    };
    for (size_t i = 0; i < map->cap; i++) {
        if (map->keys[i]) {
            MapSetU64(&new_map, map->keys[i], map->vals[i]);
        }
    }

    Free(*al, map->keys);
    Free(*al, map->vals);

    *map = new_map;
}

void MapCombine(Map *dest, Map *source) {
    if (2*(dest->len+source->len) >= dest->cap) {
        MapGrow(dest, 2 * dest->cap);
    }

    for (size_t i = 0; i < source->cap; i++) {
        if (source->keys[i]) {
            MapSetU64(dest, source->keys[i], source->vals[i]);
        }
    }
}

void *MapGetU64(Map *map, u64 key) {
    if (map->len == 0) return 0;

    ASSERT(IS_POW2(map->cap));
    size_t i = (size_t)HashU64(key);
    ASSERT(map->len < map->cap);
    for (;;) {
        i &= map->cap - 1;
        if (map->keys[i] == key) {
            return (void *) map->vals[i];
        } else if (!map->keys[i]) {
            return 0;
        }
        i++;
    }
    return 0;
}

void MapSetU64(Map *map, u64 key, u64 val) {
    ASSERT_MSG(key, "Storing values with Zero keys is unsupported");
    ASSERT_MSG(val, "Storing Zero Values is unsupported");
    if (2*map->len >= map->cap) {
        MapGrow(map, 2 * map->cap);
    }
    ASSERT(2*map->len < map->cap);
    ASSERT(IS_POW2(map->cap));
    size_t i = (size_t)HashU64(key);
    for (;;) {
        i &= map->cap - 1;
        if (!map->keys[i]) {
            map->len++;
            map->keys[i] = key;
            map->vals[i] = val;
            return;
        } else if (map->keys[i] == key) {
            map->vals[i] = val;
            return;
        }
        i++;
    }
}

void *MapGet(Map *map, const void *key) {
    return (void *)(uintptr_t)MapGetU64(map, (u64)(uintptr_t)key);
}

void MapSet(Map *map, const void *key, const void *val) {
    MapSetU64(map, (u64)(uintptr_t)key, (u64)(uintptr_t)val);
}

#if TEST
void test_map(void) {
    Map map = {0};
    enum { N = 1024 };
    for (size_t i = 1; i < N; i++) {
        MapSet(&map, (void *) i, (void *) (i + 1));
    }
    for (size_t i = 1; i < N; i++) {
        u64 val = (u64) MapGet(&map, (void *) i);
        ASSERT(val == (i + 1));
    }
    // Override values

    for (size_t i = 2; i < N; i++) {
        MapSet(&map, (void *) i, (void *) (i - 1));
    }
    for (size_t i = 2; i < N; i++) {
        u64 val = (u64) MapGet(&map, (void *) i);
        ASSERT(val == (i - 1));
    }
}

void test_mapCopy() {
    Map map = {0};
    enum { N = 1024 };
    for (size_t i = 1; i < N; i++) {
        MapSet(&map, (void *) i, (void *) (i + 1));
    }

    Map mapCopy;
    MapCopy(NULL, &mapCopy, &map);

    ASSERT(mapCopy.len == map.len);
    ASSERT(mapCopy.cap == map.cap);
    ASSERT(mapCopy.keys != map.keys);
    ASSERT(mapCopy.vals != map.vals);

    for (size_t i = 1; i < N; i++) {
        u64 val = (u64) MapGet(&mapCopy, (void *) i);
        ASSERT(val == (i + 1));
    }
}
#endif
