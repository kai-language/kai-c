
template <typename T>
struct Array {
    Allocator allocator;
    T *       data;
    u32       len,
              cap;


    T &operator[](u32 index) {
        #if !defined(NO_BOUNDS_CHECK)
            ASSERT_MSG_VA(index >= 0 && index < len, "Index %td is out of bounds 0..<%td", index, len);
        #endif
        return data[index];
    }

    T const &operator[](u32 index) const {
        #if !defined(NO_BOUNDS_CHECK)
            ASSERT_MSG_VA(index >= 0 && index < len, "Index %td is out of bounds 0..<%td", index, len);
        #endif
        return data[index];
    }
};
