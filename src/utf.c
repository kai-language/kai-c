
// NOTE: The data passed to the Lexer must be nul terminated
#define FileEnd 0

// “
#define LeftDoubleQuote 0x201C

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

b32 IsIdentifierHead(u32 cp) {
    if ((cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z') || cp == '_')
        return true;


    switch (cp) {
    case 0x00A8: { return true; }
    case 0x00AA: { return true; }
    case 0x00AD: { return true; }
    case 0x00AF: { return true; }
    case 0x2054: { return true; }
    }


    if (cp >= 0x00B2 && cp <= 0x00B5)   { return true; }
    if (cp >= 0x00B7 && cp <= 0x00BA)   { return true; }
    if (cp >= 0x00BC && cp <= 0x00BE)   { return true; }
    if (cp >= 0x00C0 && cp <= 0x00D6)   { return true; }
    if (cp >= 0x00D8 && cp <= 0x00F6)   { return true; }
    if (cp >= 0x00F8 && cp <= 0x00FF)   { return true; }
    if (cp >= 0x0100 && cp <= 0x02FF)   { return true; }
    if (cp >= 0x0370 && cp <= 0x167F)   { return true; }
    if (cp >= 0x1681 && cp <= 0x180D)   { return true; }
    if (cp >= 0x180F && cp <= 0x1DBF)   { return true; }
    if (cp >= 0x1E00 && cp <= 0x1FFF)   { return true; }
    if (cp >= 0x200B && cp <= 0x200D)   { return true; }
    if (cp >= 0x202A && cp <= 0x202E)   { return true; }
    if (cp >= 0x203F && cp <= 0x2040)   { return true; }
    if (cp >= 0x2060 && cp <= 0x206F)   { return true; }
    if (cp >= 0x2070 && cp <= 0x20CF)   { return true; }
    if (cp >= 0x2100 && cp <= 0x218F)   { return true; }
    if (cp >= 0x2460 && cp <= 0x24FF)   { return true; }
    if (cp >= 0x2776 && cp <= 0x2793)   { return true; }
    if (cp >= 0x2C00 && cp <= 0x2DFF)   { return true; }
    if (cp >= 0x2E80 && cp <= 0x2FFF)   { return true; }
    if (cp >= 0x3004 && cp <= 0x3007)   { return true; }
    if (cp >= 0x3021 && cp <= 0x302F)   { return true; }
    if (cp >= 0x3031 && cp <= 0x303F)   { return true; }
    if (cp >= 0x3040 && cp <= 0xD7FF)   { return true; }
    if (cp >= 0xF900 && cp <= 0xFD3D)   { return true; }
    if (cp >= 0xFD40 && cp <= 0xFDCF)   { return true; }
    if (cp >= 0xFDF0 && cp <= 0xFE1F)   { return true; }
    if (cp >= 0xFE30 && cp <= 0xFE44)   { return true; }
    if (cp >= 0xFE47 && cp <= 0xFFFD)   { return true; }
    if (cp >= 0x10000 && cp <= 0x1FFFD) { return true; }
    if (cp >= 0x20000 && cp <= 0x2FFFD) { return true; }
    if (cp >= 0x30000 && cp <= 0x3FFFD) { return true; }
    if (cp >= 0x40000 && cp <= 0x4FFFD) { return true; }
    if (cp >= 0x50000 && cp <= 0x5FFFD) { return true; }
    if (cp >= 0x60000 && cp <= 0x6FFFD) { return true; }
    if (cp >= 0x70000 && cp <= 0x7FFFD) { return true; }
    if (cp >= 0x80000 && cp <= 0x8FFFD) { return true; }
    if (cp >= 0x90000 && cp <= 0x9FFFD) { return true; }
    if (cp >= 0xA0000 && cp <= 0xAFFFD) { return true; }
    if (cp >= 0xB0000 && cp <= 0xBFFFD) { return true; }
    if (cp >= 0xC0000 && cp <= 0xCFFFD) { return true; }
    if (cp >= 0xD0000 && cp <= 0xDFFFD) { return true; }
    if (cp >= 0xE0000 && cp <= 0xEFFFD) { return true; }

    return false;
}

b32 IsIdentifierCharacter(u32 cp) {
    if (IsIdentifierHead(cp) || (cp >= '0' && cp <= '9'))
        return true;

    if (cp >= 0x0300 && cp <= 0x036F) { return true; }
    if (cp >= 0x1DC0 && cp <= 0x1DFF) { return true; }
    if (cp >= 0x20D0 && cp <= 0x20FF) { return true; }
    if (cp >= 0xFE20 && cp <= 0xFE2F) { return true; }

    return false;
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
