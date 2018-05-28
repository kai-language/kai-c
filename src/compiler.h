typedef struct CheckerWork CheckerWork;
struct CheckerWork {
    Package *package;
    Stmt *stmt;
};

void InitCompiler(void);
Package *ImportPackage(const char *path);
