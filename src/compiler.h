typedef struct CheckerWork CheckerWork;
struct CheckerWork {
    Package *package;
    Stmt *stmt;
};

void InitCompiler();
Package *ImportPackage(const char *path);
