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
b32 MakeMap(Map<T> *m, u32 cap) {
    // TODO(Brett, Jonas): custom allocators
    m->hashes = (Hash *)calloc(cap, sizeof(Hash));
    m->values = (T *)malloc(cap * sizeof(T));
    m->len = 0;
    m->cap = cap;
    return true;
}

template <typename T>
i32 mapFind(Map<T> *m, Hash hash) {
    if (m->len > 0) {
        i32 index = (i32) hash.key % m->cap;
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

    if (m->cap > 0) {
        index = hash.key % m->cap;
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
b32 MapSet(Map<T> *m, Hash key, T value) {
    if (m->len >= m->cap*0.7) {
        // TODO(Brett): reallocate and rehash
        UNIMPLEMENTED();
    }

    i32 index = mapFindFreeSlot(m, key);
    if (index >= 0) {
        m->hashes[index] = key;
        m->values[index] = value;
        m->len += 1;
        return true;
    }

    return false;
}

