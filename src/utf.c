
// NOTE: The data passed to the Lexer must be nul terminated
#define FileEnd 0

u32 DecodeCodePoint(u32 *cpLen, const char *str) {
    static const u32 FIRST_LEN[] = {
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
        4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1
    };

    static const u8 MASK[] = {
        0xFF, 0xFF, 0x1F, 0xF, 0x7
    };

    u8 b0 = str[0];
    i32 l = FIRST_LEN[b0];
    i32 val = (i32)(b0 & MASK[l]);

    for (i32 i=1; i < l; i += 1) {
        val = (val << 6) | (i32)(str[i] & 0x3f);
    }

    if (cpLen)
        *cpLen = l;
    return val;
}

u32 EncodeCodePoint(char *buffer, const u32 cp) {
    if (cp <= 0x7F) {
        buffer[0] = cp;
        return 1;
    }

    if (cp <= 0x7FF) {
        buffer[0] = 0xC0 | (cp >> 6);
        buffer[1] = 0x80 | (cp & 0x3F);
        return 2;
    }

    if (cp <= 0xFFFF) {
        buffer[0] = 0xE0 | (cp >> 12);
        buffer[1] = 0x80 | ((cp >> 6) & 0x3F);
        buffer[2] = 0x80 | (cp & 0x3F);
        return 3;
    }

    if (cp <= 0x10FFFF) {
        buffer[0] = 0xF0 | (cp >> 18);
        buffer[1] = 0x80 | ((cp >> 12) & 0x3F);
        buffer[2] = 0x80 | ((cp >> 6) & 0x3F);
        buffer[3] = 0x80 | (cp & 0x3F);
        return 4;
    }

    return 0;
}

b32 IsAlpha(u32 cp) {
    if ((cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z')) {
        return true;
    }

    // TODO(Brett): valid unicode identifier range
    // Swift actually has good documentation for this
    // See: https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410
    if (cp >= 0x100 && cp != FileEnd) {
        return true;
    }

    return false;
}

b32 IsNumeric(u32 cp) {
    return cp >= '0' && cp <= '9';
}

b32 IsValidIdentifierHead(u32 cp) {
    return IsAlpha(cp);
}

b32 IsValidIdentifierBody(u32 cp) {
    return IsAlpha(cp) || IsNumeric(cp) || cp == '_';
}
