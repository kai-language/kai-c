
typedef enum SymbolKind SymbolKind;
enum SymbolKind {
    SymbolKind_Invalid,
    SymbolKind_Type,
    SymbolKind_Package,
    SymbolKind_Variable,
    SymbolKind_Constant,
};

typedef enum SymbolState SymbolState;
enum SymbolState {
    SymbolState_Unresolved,
    SymbolState_Resolving,
    SymbolState_Resolved,
};

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

void DeclarePackageSymbol(Package *package, const char *name, Symbol *sym) {
    Symbol *old = MapGet(&package->symbolMap, name);
    if (old) {
        Position pos = sym->decl ? sym->decl->start : (Position){0};
        ReportError(package, RedefinitionError, pos, "Duplicate definition of global symbol '%s'", name);
        if (old->decl) {
            ReportNote(package, old->decl->start, "Previous definition of '%s'", name);
        }
    }

    MapSet(&package->symbolMap, name, sym);
    ArrayPush(package->symbols, sym);
}
