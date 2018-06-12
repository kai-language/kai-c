
typedef enum CheckerInfoKind {
    CheckerInfoKind_Constant,
    CheckerInfoKind_Variable,
    CheckerInfoKind_Ident,
    CheckerInfoKind_Selector,
    CheckerInfoKind_BasicExpr,
} CheckerInfoKind;

typedef struct CheckerInfo_Constant CheckerInfo_Constant;
struct CheckerInfo_Constant {
    Symbol *symbol;
};

typedef struct CheckerInfo_Variable CheckerInfo_Variable;
struct CheckerInfo_Variable {
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
    b8 isConstant;
    Val val;
};

typedef struct CheckerInfo CheckerInfo;
struct CheckerInfo {
    CheckerInfoKind kind;
    union {
        CheckerInfo_Constant Constant;
        CheckerInfo_Variable Variable;
        CheckerInfo_Selector Selector;
        CheckerInfo_Ident Ident;
        CheckerInfo_BasicExpr BasicExpr;
    };
};

Symbol *Lookup(Scope *scope, const char *name);
