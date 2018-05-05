// a struct to ensure someone doesn't accidentally pass in a u64 that's not a hash
struct Hash {
    u64 key;
};

Inline
Hash hash(const u8 *data, u64 len) {
    u64 hash = FNV64a(data, len);
    ASSERT_MSG(hash, "It looks like your data hashed to 0. Please report this");
    return (Hash){hash};
}

Inline
Hash HashString(String s) {
    return hash(s.data, s.len);
}

Inline
Hash HashPtr(void *ptr) {
    ASSERT(ptr);
    return (Hash){(u64)ptr};
}

template <typename T>
struct Map {
    Hash *hashes;
    T    *values;
    u64 len,
        cap;
};

template <typename T>
i32 mapFind(Map<T> *m, Hash hash) {
    if (m->len > 0) {
        i32 index = hash.key % m->len;
        while (m->hashes[index].key != 0) {
            if (m->hashes[index].key == hash.key) {
                return index;
            }

            index = (index+1) % m->len;
        }
    }

    return -1;
}

template <typename T>
i32 mapFindFreeSlot(Map<T> *m, Hash hash) {
    ASSERT_MSG(m->len != m->cap, "Map should never be full. Please report this");

    i32 index = -1;

    if (m->len > 0) {
        index = hash.key % m->len;
        // NOTE: this loop will never exit if the map is full
        while (m->hashes[index].key != 0) {
            index = (index+1) % m->len;
        }
    }

    return index;
}

template <typename T>
Inline
T *MapGet(Map<T> *m, Hash key) {
    i32 index = mapFind(m, key);
    if (index < 0)
        return 0;

    return &m->values[index];
}

template <typename T>
Inline
void MapSet(Map<T> *m, Hash key, T value) {

}
