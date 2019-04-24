
#include "all.h"
#include "string.h"
#include "arena.h"
#include "package.h"
#include "queue.h"
#include "compiler.h"

// If end is NULL strlen will be called on start
const char *str_intern_range(const char *start, const char *end) {
    TRACE(INTERN);
    ASSERT(end - start < UINT32_MAX);
    size_t len = end - start;
    size_t hash = stbds_hash_bytes((void *) start, len, 0);

    InternedString intern = hmgets(compiler.interns, hash);
    if (intern.len) return intern.value;
    intern.value = arena_alloc(&compiler.strings, len + 1);
    memcpy(intern.value, start, len);
    intern.value[len] = 0;
    intern.key = hash;
    intern.len = (u32) len;
    stbds_hmputs(compiler.interns, intern);
    COUNTER1(INTERN, "num_interns", INT("num", (int) hmlen(compiler.interns)));
    return intern.value;
}

const char *str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

const char *str_join(const char *a, const char *b) {
    char mem[4 * 1024];
    strcpy(mem, a);
    strcat(mem, b);
    return str_intern(mem);
}

#if TEST
void test_stringInterning() {
    char mem[4 * 1024];

#define ALPHA "alpha"
#define BETA "beta"
    strncpy(mem, ALPHA, sizeof(ALPHA));
    mem[sizeof(ALPHA)] = 0;

    ASSERT(strncmp(ALPHA, mem, sizeof(ALPHA)) == 0);

    const char *stored = str_intern(ALPHA);
    ASSERT(strncmp(stored, mem, sizeof(ALPHA)) == 0);

    // Push ourselves over allocated space so we allocate a new block

    const char *retrieved = str_intern(ALPHA);
    ASSERT(stored == retrieved);

    stored = str_intern(BETA);
    retrieved = str_intern(BETA);
    ASSERT(stored == retrieved);

    char stackAlpha[] = ALPHA;
    ASSERT(str_intern(ALPHA) == str_intern(stackAlpha));

    char stackBeta[] = BETA;
    ASSERT(str_intern(BETA) == str_intern(stackBeta));

#undef ALPHA
#undef BETA
}
#endif
