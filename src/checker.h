#define CHECKER_INFO_KINDS \
    FOR_EACH(Decl)         \
    FOR_EACH(DeclList)     \
    FOR_EACH(Selector)     \
    FOR_EACH(Ident)        \
    FOR_EACH(BasicLit)     \

typedef enum CheckerInfoKind {
#define FOR_EACH(kind) CheckerInfoKind_##kind,
    CHECKER_INFO_KINDS
#undef FOR_EACH
} CheckerInfoKind;

struct CheckerInfo_Decl {
    Symbol *symbol;
    b8 isGlobal;
};

struct CheckerInfo_DeclList {
    b8 isGlobal;
    DynamicArray(Symbol *) symbols;
};

struct CheckerInfo_Ident {
    Symbol *symbol;
};

struct CheckerInfo_Selector {
    u32 levelsOfIndirection;
    Val constant;
};

struct CheckerInfo_BasicLit {
    Type *type;
};

#define FOR_EACH(kind) typedef struct CheckerInfo_##kind CheckerInfo_##kind;
    CHECKER_INFO_KINDS
#undef  FOR_EACH

typedef struct CheckerInfo CheckerInfo;
struct CheckerInfo {
    CheckerInfoKind kind;
    union {
#define FOR_EACH(kind) CheckerInfo_##kind kind;
        CHECKER_INFO_KINDS
#undef FOR_EACH
    };
};

Symbol *Lookup(Scope *scope, const char *name);
