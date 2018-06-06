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
#define ArraysEqual(a, b) (a == b || (ArrayLen(a) == ArrayLen(b) && memcmp(a, b, ArrayLen(a)) == 0))

#ifndef __cplusplus

#define ForEachWithIndex(agg, _indexName) \
size_t _indexName = 0; \
for (__auto_type it = agg ? agg[_indexName] : NULL; _indexName < ArrayLen(agg); _indexName++, it = agg[_indexName])

#define CONCATENATE_DIRECT(x, y) x##y
#define CONCATENATE(x, y) CONCATENATE_DIRECT(x, y)
#define ANONYMOUS_VARIABLE(x) CONCATENATE(x, __LINE__)

#define ForEach(agg) ForEachWithIndex(agg, ANONYMOUS_VARIABLE(_index))

#endif

#ifdef __cplusplus
extern "C" {
#endif

extern void *_arrayFree(void *array, Allocator al);
extern void *_arrayGrow(const void *array, size_t newLen, size_t elemSize);
extern u8 *_arrayPrintf(u8 *array, const char *fmt, ...);

#ifdef __cplusplus
}
#endif
