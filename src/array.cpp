
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


// NOTE(jonas): this arena can never be freed
template <typename T>
struct ArrayArena {
    Allocator  baseAllocator;
    
    // initial array size
    u32               defaultLength;
    // max array num per memory chunk
    u32               maxCount;
    // number of arrays remaining
    u32               remaining;
    
    T                 *nextArray;
};

template <typename T>
void initArrayArena(ArrayArena<T> *aa, u32 defaultLength, u32 maxCount) {
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
        aa->remaining = aa->maxCount;
        u64 size      = sizeof(T) * aa->defaultLength * aa->maxCount;
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
void reallocArray(ArrayArena<T> aa, Array<T> *ar) {
    
}

template <typename T>
void initArray(ArrayArena<T> *aa, Array<T> *ar) {
    ar->data = (T *) allocArray(aa);
    ar->len  = 0;
    ar->cap  = aa->defaultLength;
}

