
b32 IsAlpha(u32 cp);
b32 IsIdentifierCharacter(u32 cp);
b32 IsIdentifierHead(u32 cp);
u32 EncodeCodePoint(char *buffer,const u32 cp);
u32 DecodeCodePoint(u32 *cpLen,const char *str);
