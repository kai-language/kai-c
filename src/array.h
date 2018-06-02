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

#ifdef __cplusplus
extern "C" {
#endif

extern void *_arrayFree(void *array, Allocator al);
extern void *_arrayGrow(const void *array, size_t newLen, size_t elemSize);
extern u8 *_arrayPrintf(u8 *array, const char *fmt, ...);

#ifdef __cplusplus
}
#endif
