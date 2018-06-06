
typedef enum CheckerInfoKind {
    CheckerInfoKind_Decl,
    CheckerInfoKind_DeclList,
    CheckerInfoKind_Ident,
    CheckerInfoKind_Selector,
    CheckerInfoKind_BasicExpr,
} CheckerInfoKind;

typedef struct CheckerInfo_Decl CheckerInfo_Decl;
struct CheckerInfo_Decl {
    Symbol *symbol;
    b8 isGlobal;
};

typedef struct CheckerInfo_DeclList CheckerInfo_DeclList;
struct CheckerInfo_DeclList {
    b8 isGlobal;
    DynamicArray(Symbol *) symbols;
};

typedef struct CheckerInfo_Ident CheckerInfo_Ident;
struct CheckerInfo_Ident {
    Symbol *symbol;
};

typedef struct CheckerInfo_Selector CheckerInfo_Selector;
struct CheckerInfo_Selector {
    u32 levelsOfIndirection;
    Val constant;
};

typedef struct CheckerInfo_BasicExpr CheckerInfo_BasicExpr;
struct CheckerInfo_BasicExpr {
    Type *type;
    Val val;
};

typedef struct CheckerInfo CheckerInfo;
struct CheckerInfo {
    CheckerInfoKind kind;
    union {
        CheckerInfo_Decl Decl;
        CheckerInfo_DeclList DeclList;
        CheckerInfo_Selector Selector;
        CheckerInfo_Ident Ident;
        CheckerInfo_BasicExpr BasicExpr;
    };
};

Symbol *Lookup(Scope *scope, const char *name);
