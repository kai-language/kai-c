#pragma once

typedef struct InternedString InternedString;
struct InternedString {
    size_t key;
    char *value;
    u32 len;
};

extern const char *intern_in;
extern const char *intern_ptr;
extern const char *intern_len;
extern const char *intern_cap;

const char *str_intern(const char *str);
const char *str_intern_range(const char *start,const char *end);
const char *str_join(const char *a, const char *b);
