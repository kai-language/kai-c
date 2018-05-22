typedef struct Symbol Symbol;
typedef struct Stmt Stmt;

typedef struct Package Package;
struct Package {
    const char *path;
    char fullPath[MAX_PATH];
    const char *externalName;
    Arena arena;
    DynamicArray(Stmt *) stmts;
    Map symbolMap;
    DynamicArray(Symbol *) symbols;
};

void InitCompiler();
Package *ImportPackage(const char *path);
