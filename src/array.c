

typedef struct ArrayHdr {
    Allocator *allocator;
    size_t cap;
    size_t len;
    u8 data[];
} ArrayHdr;

#define DynamicArray(Type) Type *

#define _array_hdr(b) ((ArrayHdr *)((u8 *)(b) - offsetof(ArrayHdr, data)))

#define ArrayLen(b) ((b) ? _array_hdr(b)->len : 0)
#define ArrayCap(b) ((b) ? _array_hdr(b)->cap : 0)
#define ArrayEnd(b) ((b) ? (b) + ArrayLen(b)  : 0)

#define ArrayFree(b) ((b) ? ((b) = Free(*_array_hdr(b)->allocator, _array_hdr(b))) : 0);
#define ArrayFit(b, n) ((n) <= ArrayCap(b) ? 0 : ((b) = _arrayGrow((b), (n), sizeof(*(b)))))
#define ArrayPush(b, ...) (ArrayFit((b), 1 + ArrayLen(b)), (b)[_array_hdr(b)->len++] = (__VA_ARGS__))
#define ArrayPrintf(b, ...) ((b) = _arrayPrintf((b), __VA_ARGS__))
#define ArrayClear(b) ((b) ? _array_hdr(b)->len = 0 : 0)
#define ArrayAllocator(b) ((b) ? _array_hdr(b)->allocator : &DefaultAllocator)

void *_arrayFree(void *array, Allocator al) {
    return Free(al, _array_hdr(array));
}

void *_arrayGrow(const void *array, size_t newLen, size_t elemSize) {
    ASSERT(ArrayCap(array) <= (SIZE_MAX - 1) / 2);
    size_t newCap = CLAMP_MIN(2 * ArrayCap(array), MAX(newLen, 16));
    ASSERT(newLen <= newCap);
    ASSERT(newCap <= (SIZE_MAX - offsetof(ArrayHdr, data)) / elemSize);
    size_t newSize = offsetof(ArrayHdr, data) + newCap * elemSize;
    ArrayHdr *newHdr;
    if (array) {
        newHdr = ArrayAllocator(array)->func(ArrayAllocator(array)->payload, AT_Realloc, newSize, ArrayCap(array), _array_hdr(array));
    } else {
        newHdr = Alloc(DefaultAllocator, newSize);
        newHdr->allocator = &DefaultAllocator;
        newHdr->len = 0;
    }
    newHdr->cap = newCap;
    return newHdr->data;
}

u8 *_arrayPrintf(u8 *array, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t cap = ArrayCap(array) - ArrayLen(array);
    size_t n = 1 + vsnprintf((char*) ArrayEnd(array), cap, fmt, args);
    va_end(args);
    if (n > cap) {
        ArrayFit(array, n + ArrayLen(array));
        va_start(args, fmt);
        size_t new_cap = ArrayCap(array) - ArrayLen(array);
        n = 1 + vsnprintf((char*) ArrayEnd(array), new_cap, fmt, args);
        ASSERT(n <= new_cap);
        va_end(args);
    }
    _array_hdr(array)->len += n - 1;
    return array;
}

#if TEST
void test_array() {
    DynamicArray(int) buf = NULL;
    ASSERT(ArrayCap(buf) == 0);
    ASSERT(ArrayLen(buf) == 0);
    int n = 1024;
    for (int i = 0; i < n; i++) {
        ArrayPush(buf, i);
    }
    ASSERT(ArrayLen(buf) == n);
    for (size_t i = 0; i < ArrayLen(buf); i++) {
        ASSERT(buf[i] == i);
    }

    ArrayFree(buf);
    ASSERT(buf == NULL);
    ASSERT(ArrayLen(buf) == 0);
    DynamicArray(u8) str = NULL;
    ArrayPrintf(str, "One: %d\n", 1);
    ASSERT(strcmp((char*) str, "One: 1\n") == 0);
    ArrayPrintf(str, "Hex: 0x%x\n", 0x12345678);
    ASSERT(strcmp((char*) str, "One: 1\nHex: 0x12345678\n") == 0);
}
#endif
