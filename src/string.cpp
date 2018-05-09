
struct String {
    u8 *data;
    u32 len;

    u8 &operator[](u32 i) {
        #ifndef NO_BOUNDS_CHECK
        ASSERT_MSG_VA(i >= 0 && i < len, "Index %td is out of bounds 0..<%td", i, len);
        #endif
        return data[i];
    }

    u8 const &operator[](u32 i) const {
        #ifndef NO_BOUNDS_CHECK
        ASSERT_MSG_VA(i >= 0 && i < len, "Index %td is out of bounds 0..<%td", i, len);
        #endif
        return data[i];
    }
};

Inline
String MakeString(u8 *data, u32 len) {
    return (String){
        .data = data,
        .len = len
    };
}

Inline
String MakeCString(char *data) {
    return MakeString((u8 *)data, (u32)strlen(data));
}

Inline
String Slice(String string, u32 lo, u32 hi) {
    #ifndef NO_BOUNDS_CHECK
    u32 max = string.len;
    ASSERT(lo <= hi && hi <= max);
    #endif
    return MakeString(string.data+lo, hi-lo);
}

Inline
b32 StringEqual(String const &a, String const &b) {
    if (a.len != b.len)
        return false;

    for (u32 i = 0; i < a.len; i += 1) {
        if (a[i] != b[i])
            return false;
    }

    return true;
}

Inline b32 operator ==(String const &a, String const &b) { return StringEqual(a, b); }
Inline b32 operator !=(String const &a, String const &b) { return !StringEqual(a, b); }

#define LIT(str) ((int)(str).len), (str).data
#define STR(c) { (u8 *)c, sizeof(c)-1}

enum StringEscapeError {
    SEE_Error,
    SEE_NoAllocations,
    SEE_AllocatedMem
};

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
