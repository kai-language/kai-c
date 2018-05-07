
template <typename T>
struct Array {
    T *       data;
    u32       len,
              cap;


    T &operator[](u32 i) {
        #if !defined(NO_BOUNDS_CHECK)
            ASSERT_MSG_VA(i >= 0 && i < len, "Index %td is out of bounds 0..<%td", i, len);
        #endif
        return data[i];
    }

    T const &operator[](u32 i) const {
        #if !defined(NO_BOUNDS_CHECK)
            ASSERT_MSG_VA(i >= 0 && i < len, "Index %td is out of bounds 0..<%td", i, len);
        #endif
        return data[i];
    }
};

#define ARRAY_GROWTH 1.5

#define MAX_CAP    0x7FFFFFFF
#define FLAG_SLICE 0x80000000

#define ARRAY_IS_SLICE(x) (x->cap & FLAG_SLICE)


// NOTE(jonas): this arena can never be freed
template <typename T>
struct ArrayArena {
    Allocator  baseAllocator;
    
    // initial array size
    u32        defaultLength;
    // max array num per memory chunk
    u32        maxCount;
    // number of arrays remaining
    u32        remaining;
    
    T          *nextArray;
};

#define ARRAY_ARENA_GROWTH 1.5


template <typename T>
void initArrayArena(ArrayArena<T> *aa, u32 defaultLength, u32 maxCount) {
#if DEBUG
    if ( defaultLength > MAX_CAP ) {
        printf("Requested array length too large!\n");
        defaultLength = MAX_CAP;
    }
#endif
    aa->baseAllocator = makeDefaultAllocator();
    
    aa->defaultLength = defaultLength;
    aa->maxCount      = maxCount;
    aa->remaining     = maxCount;
    
    u64 size      = sizeof(T) * defaultLength * maxCount;
    aa->nextArray = (T *) alloc(aa->baseAllocator, size);
}


template <typename T>
T * allocArray(ArrayArena<T> *aa) {
    
    if ( aa->remaining <= 0 ) {
        aa->maxCount  *= ARRAY_ARENA_GROWTH;
        aa->remaining = aa->maxCount;
        u64 size      = sizeof(T) * aa->defaultLength * aa->remaining;
        aa->nextArray = (T *) alloc(aa->baseAllocator, size);
        if ( ! aa->nextArray )
            return NULL;
#if DEBUG
        printf("requesting more memory for array arena at %p\n", aa->nextArray);
#endif
    }
    
    T *ptr = aa->nextArray;
    aa->remaining -= 1;
    aa->nextArray  = ptr + (aa->defaultLength * sizeof(T));
    return ptr;
}


template <typename T>
void dumpArray(Array<T> ar) {
    printf("Array = {\n");
    printf("\tdata = %p\n", ar.data);
    printf("\tlen  = %u\n", ar.len);
    printf("\tcap  = %u\n", ARRAY_IS_SLICE((&ar)) ? ar.cap & MAX_CAP : ar.cap );
    printf("}\n");
}


template <typename T>
void reallocArray(ArrayArena<T> aa, Array<T> *ar, u32 newCap) {
#if DEBUG
    if ( newCap > MAX_CAP )
        newCap = MAX_CAP;
#endif
    if ( ARRAY_IS_SLICE(ar) ) {
        T * oldData = ar->data;
        ar->data = (T *) alloc(aa.baseAllocator, newCap);
        memcpy( &(ar->data), &oldData, ar->len * sizeof(T));
        ar->cap  = ar->cap & MAX_CAP;
#if DEBUG
        printf("Reallocating a slice!\n");
#endif
    }
    else {
        ar->data = (T *) realloc(aa.baseAllocator, ar->data, ar->cap * sizeof(T), newCap * sizeof(T));
#if DEBUG
        printf("Reallocating!\n");
#endif

    }
    ar->cap  = newCap;
}


template <typename T>
void initArray(ArrayArena<T> *aa, Array<T> *ar) {
    ar->data = (T *) allocArray(aa);
    ar->len  = 0;
    ar->cap  = aa->defaultLength | FLAG_SLICE;
}

