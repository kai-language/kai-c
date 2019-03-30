
typedef struct CheckerWork CheckerWork;
struct CheckerWork {
    Package *package;
    Stmt *stmt;
};

Package *ImportPackage(const char *path, Package *importer);

extern Package builtinPackage;
