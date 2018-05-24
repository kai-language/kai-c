#define TYPE_INFO_KINDS \
    FOR_EACH(Decl)      \
    FOR_EACH(Selector)  \

typedef enum TypeInfoKind {
#define FOR_EACH(kind) TypeInfoKind_##kind,
    TYPE_INFO_KINDS
#undef FOR_EACH
} TypeInfoKind;


typedef struct TypeInfo_Decl TypeInfo_Decl;
struct TypeInfo_Decl {
    Symbol *symbol;
};

typedef struct TypeInfo_Selector TypeInfo_Selector;
struct TypeInfo_Selector {

};

typedef struct TypeInfo TypeInfo;
struct TypeInfo {
    TypeInfoKind kind;
    union {
        TypeInfo_Decl Decl;
    };
};
