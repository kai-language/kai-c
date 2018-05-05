
template <typename T>
struct Array {
    Allocator allocator;
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
