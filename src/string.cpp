struct String {
    u8 *data;
    u32 len;

    u8 &operator[](u32 i) {
        return data[i];
    }

    u8 const &operator[](u32 i) const {
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
    u32 max = string.len;
    ASSERT(lo <= hi && hi <= max);
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
#define STR(c) { (u8 *)c, ArrayCount(c)-1}
