
typedef u8 SymbolKind;
enum Enum_SymbolKind {
    SymbolKindInvalid = 0x0,

    SymbolKindImport  = 0x1,
    SymbolKindLibrary = 0x2,

    SymbolKindLabel   = 0x3,

    SymbolKindType    = 0x4,

    SymbolKindConstant = 0x5,
    SymbolKindVariable = 0x6,

    SYMBOL_KIND_COUNT,
};

typedef u8 SymbolState;
enum Enum_SymbolState {
    SymbolState_Unresolved = 0x0,
    SymbolState_Resolving  = 0x1,
    SymbolState_Resolved   = 0x2,
};

typedef struct Decl Decl;
typedef struct Type Type;

typedef u8 SymbolFlag;
enum Enum_SymbolFlag {
    SymbolFlag_Global = 0x01,
};

struct Symbol {
    const char *name;
    const char *externalName;
    SymbolKind kind;
    SymbolFlag flags;
    SymbolState state; // NOTE: when we add @Multithreading we will need a thread ID to check for loops
    Decl *decl;

    // This pointer can be used by any backend to store symbol-related info for quick lookup
    void *backendUserdata;

    Type *type;
    b8 used;
    Val val;
};
