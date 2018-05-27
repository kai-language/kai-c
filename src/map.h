typedef struct Map Map;
struct Map {
    Allocator *allocator;
    u64 *keys;
    u64 *vals;
    size_t len;
    size_t cap;
};

void MapFree(Map *map);
void MapCopy(Allocator *alloc, Map *dest, Map *source);
void MapGrow(Map *map, size_t newCap);

void *MapGetU64(Map *map, u64 key);
void *MapGet(Map *map, const void *key);

void MapSet(Map *map, const void *key, const void *val);
void MapSetU64(Map *map, u64 key, u64 val);

