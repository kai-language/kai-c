
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

typedef struct Package Package;
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

typedef struct Stmt Stmt;
struct Package {
    const char *path;
    char fullPath[MAX_PATH];
    const char *externalName;
    Arena arena;
    DynamicArray(Stmt *) stmts;
    Map symbolMap;
    DynamicArray(Symbol*) symbols;
};

void InitCompiler() {
    InitErrorBuffers();
    InitKeywords();
    InitDetailsForCurrentSystem();
    InitUnsetFlagsToDefaults();
}

Map packageMap;
DynamicArray(Package*) packages;

Queue parsingQueue;

void addPackage(Package *package) {
    Package *old = MapGet(&packageMap, package->path);
    if (old != package) {
        ASSERT(!old);
        MapSet(&packageMap, package->path, package);
        ArrayPush(packages, package);
    }
}

Package *ImportPackage(const char *path) {
    path = StrIntern(path);
    Package *package = MapGet(&packageMap, path);
    if (!package) { // First time we have seen this package
        package = Calloc(DefaultAllocator, 1, sizeof(Package));
        package->path = path;
        if (FlagVerbose) printf("Importing %s\n", path);

        char fullPath[MAX_PATH];
        if (!AbsolutePath(path, fullPath)) return NullWithLoggedReason("Failed to resolve absolute path for %s", path); // Unable to resolve file path

        strcpy(package->fullPath, fullPath);
        addPackage(package);
        QueueEnqueue(&parsingQueue, package);
    }
    return package;
}
