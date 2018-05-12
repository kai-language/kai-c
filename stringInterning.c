

typedef struct Intern Intern;
struct Intern {
    Intern *next;
    size_t len;
    char data[];
};

Allocator internAllocator;
Arena internArena;
Map interns;

const char *strInternRange(const char *start, const char *end) {
    size_t len = end - start;
    u64 hash = HashBytes(start, len);
    u64 key = hash ? hash : 1; // 0 is a sentinal
    Intern *intern = (Intern *) MapGetU64(&interns, key);
    for (Intern *it = intern; it; it = it->next) {
        if (it->len == len && strncmp(it->data, start, len) == 0) {
            return it->data;
        }
    }
    Intern *newIntern = (typeof newIntern) Alloc(internAllocator, offsetof(Intern, data) + len + 1);
    newIntern->len = len;
    newIntern->next = intern;
    memcpy(newIntern->data, start, len);
    newIntern->data[len] = 0; // Zero terminate
    MapSet(&interns, (void*) key, (void*) newIntern);
    return newIntern->data;
}

const char *strIntern(const char *str) {
    return strInternRange(str, str + strlen(str));
}

#define LIT(str) ((int)(str).len), (str).data
#define STR(c) (String) { (u8 *)c, sizeof(c)-1}

enum StringEscapeError {
    SEE_Error,
    SEE_NoAllocations,
    SEE_AllocatedMem
};

/*
// TODO(Brett): custom allocator
StringEscapeError EscapeString(String *str) {
    String s = *str;

    if (s.len < 2) {
        return SEE_Error;
    }

    u8 quote = s[0];
    if (s[s.len-1] != quote && quote != '"') {
        return SEE_Error;
    }

    s = Slice(s, 1, s.len - 1);

    b32 requiresEscape = false;
    for (u32 i = 0; i < s.len; i += 1) {
        if (s[i] == '\\') {
            requiresEscape = true;
            break;
        }
    }

    if (!requiresEscape) {
        *str = s;
        return SEE_NoAllocations;
    }

    // TODO(Brett): actually escape string
    *str = s;
    return SEE_AllocatedMem;
}
*/
