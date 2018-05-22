
typedef enum SymbolKind {
    SymbolKind_Invalid,
    SymbolKind_Type,
    SymbolKind_Package,
    SymbolKind_Variable,
    SymbolKind_Constant,
} SymbolKind;

typedef enum SymbolState {
    SymbolState_Unresolved,
    SymbolState_Resolving,
    SymbolState_Resolved,
} SymbolState;

typedef struct Decl Decl;
typedef struct Type Type;

typedef struct Symbol Symbol;
struct Symbol {
    const char *name;
    const char *externalName;
    SymbolKind kind;
    SymbolState state;
    Decl *decl;
    Type *type;
    Val val;
};
