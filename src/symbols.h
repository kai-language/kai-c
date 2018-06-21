
typedef enum SymbolKind {
    SymbolKind_Invalid,

    SymbolKind_Import,
    SymbolKind_Library,

    SymbolKind_Label,

    SymbolKind_Type,

    SymbolKind_Constant,
    SymbolKind_Variable,
} SymbolKind;

typedef enum SymbolState {
    SymbolState_Unresolved,
    SymbolState_Resolving,
    SymbolState_Resolved,
} SymbolState;

typedef struct Decl Decl;
typedef struct Type Type;

struct Symbol {
    const char *name;
    const char *externalName;
    SymbolKind kind;
    SymbolState state;
    Decl *decl;

    // This pointer can be used by any backend to store symbol-related info for quick lookup
    void *backendUserdata;

    Type *type;
    b8 used;
    Val val;
};
