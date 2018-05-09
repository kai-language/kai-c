
u64 FNV64a(const u8 *key, u64 len) {
    u64 h = 0xcbf29ce484222325;
    for (u32 c = 0; c<len; ++c) {
        h = (h ^ (u64) key[c]) * 0x100000001b3;
    }
    return h;
}
