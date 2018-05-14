#include "../src/common.c"


i32 startsWith(char *str, const char *cmp) {
    i32 idx = 0;
    while ( str[idx] == cmp[idx] )
        idx++;

    if ( cmp[idx] == '\0' )
        return idx;
    else
        return -1;
}


typedef struct DeferBlock DeferBlock;
struct DeferBlock {

    DeferBlock *prev;
    i32        scopeLevel;

    char *data;
    u32  len;

};


DeferBlock * makeDeferBlock(char *data, DeferBlock *prev, i32 scopeLevel) {
    DeferBlock *db = (DeferBlock *) Alloc(DefaultAllocator, sizeof(DeferBlock));
    db->data = data;
    db->prev = prev;
    db->scopeLevel = scopeLevel;
    return db;
}


typedef struct Lexer Lexer;
struct Lexer {

    const char *file;

    char *at;

    DeferBlock *block;
    i32 scopeLevel;
    b8  seenReturn;

    char *newFile;
    u32  len;
    u32  cap;

};


Lexer makeLexer(const char *file) {

    u32 cap = KB(1);

    Lexer l;

    l.file = file;
    l.at   = (char *) file;

    l.block = NULL;
    l.scopeLevel = 0;
    l.seenReturn = false;

    l.newFile = (char *) Alloc(DefaultAllocator, cap);
    l.len = 0;
    l.cap = cap;

    return l;
}


Inline
void advance(Lexer *l) {

    if ( l->len >= l->cap ) {
        u32 newCap = 1.5 * l->cap;
        l->newFile = (char *) Realloc(DefaultAllocator, l->newFile, newCap, l->cap);
        l->cap     = newCap;
    }

    l->newFile[l->len] = l->at[0];
    l->len++;
    l->at++;
}


void writeToNewFile(Lexer *l, char *text, u32 len) {

    if ( l->len + len >= l->cap ) {
        u32 newCap = 1.5 * (l->cap + len);
        l->newFile = (char *) Realloc(DefaultAllocator, l->newFile, newCap, l->cap);
        l->cap     = newCap;
    }

    for ( u32 i=0; i<len; i++ ) {
        l->newFile[l->len] = text[i];
        l->len++;
    }

}


void writeDeferBlock(Lexer *l) {
    if ( l->block ) {
        writeToNewFile(l, l->block->data, l->block->len);
    }
}


Inline
b32 isWhiteSpace(char c) {
    return( c == ' ' || c == '\t' || c == '\n' || c == '\r' );
}


void eatWhiteSpace(Lexer *l) {
    while ( isWhiteSpace(l->at[0]) )
        l->at++; 
}


DeferBlock * grabDeferBlock(Lexer *l, DeferBlock *prev, i32 scopeLevel) {
    DeferBlock *db = makeDeferBlock(l->at, prev, scopeLevel);

    u32 len = 0;
    while ( l->at[0] != '}' ) {
        len++;
        l->at++;
    }
    len++;
    l->at++;

    db->len = len;

    return db;
}


void parseDefer(Lexer *l) {

    i32 isDefer = startsWith(l->at, "defer");
    if ( isDefer > 0 ) {
        l->at += isDefer;
        eatWhiteSpace(l);
        if ( l->at[0] == '{' ) {
            DeferBlock *db = grabDeferBlock(l, l->block, l->scopeLevel);
            printf("DeferBlock:\n%.*s\n", db->len, db->data);
            db->prev = l->block;
            l->block = db;
        }
    }
}


void addDeferBlocks(Lexer *l) {

    if ( ! l->block ) {
        return;
    }

    if ( l->scopeLevel > l->block->scopeLevel ) {
        writeDeferBlock(l); 
    }
    else {

    }

}


void processFile(Lexer *l) {

    u32 idx = 0;

    while( l->at[0] != 0 ) {

        switch (l->at[0]) {

            case 'd': {
                parseDefer(l);
            } break;

            case '{': {
                l->scopeLevel += 1;
            } break;

            case '}': {
                if ( ! l->seenReturn ) {
                    addDeferBlocks(l);
                }
                l->scopeLevel -= 1;
            } break;

            case 'r': {
                i32 isReturn = startsWith(l->at, "return");
                if ( isReturn > 0 ) {
                    l->seenReturn = true;
                    addDeferBlocks(l);
                }
            } break;

            default: {}
        }
        advance(l);
    }

}


int main(int argnum, char **args) {

    if ( argnum < 2 ) {
        printf("Specify input!\n");
        return 1;
    }

    const char *path = args[1];
    const char *file = ReadFile(path);

    printf("path = %s\n", path);
    printf("file:\n%s\n", file);

    Lexer l = makeLexer(file);
    
    processFile(&l);

    printf("newFile:\n%s\n", l.newFile);

    return 0;
}

