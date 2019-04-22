#pragma once

// requires nothing

// package.h
typedef struct Package Package;

// ast.h
typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Decl Decl;

// types.h
typedef struct Type Type;

typedef union Val Val;
union Val {
    i64 i;
    u64 u;
    f64 f;
    void *p;
};

typedef enum SymKind SymKind;
enum SymKind {
    SYM_NONE = 0,
    SYM_VAR,
    SYM_VAL,
    SYM_PACKAGE,
    SYM_LIBRARY,
};

typedef enum SymState SymState;
enum SymState {
    SYM_UNRESOLVED = 0,
    SYM_RESOLVING,
    SYM_RESOLVED,
};

typedef enum Reachable Reachable;
enum Reachable {
    REACHABLE_NONE = 0,
    REACHABLE_NATURAL,
    REACHABLE_FORCED,
};

typedef struct Sym Sym;
struct Sym {
    const char *name;
    Package *owning_package;
    SymKind kind;
    SymState state;
    Reachable reachable : 8;
    Decl *decl;
    const char *external_name;
    union {
        struct {
            Type *type;
            Val val;
        };
        Package *package;
    };
};

typedef struct SymMapEntry SymMapEntry;
struct SymMapEntry {
    const char *key;
    Sym *value;
};

typedef struct Scope Scope;
struct Scope {
    Scope *parent;
    SymMapEntry *members;
};

typedef enum OperandFlags OperandFlags;
enum OperandFlags {
    OPERAND_FLAGS_NONE = 0,
    LVALUE    = 0x01,
    CONST     = 0x02,
    TYPE      = 0x04,
    PACKAGE   = 0x08,
    LIBRARY   = 0x10,
    SPECIAL   = 0x20, // Used to indicate something about the Operand is special
    BAD_VALUE = 0x80,
};

typedef struct Operand Operand;
struct Operand {
    Type *type;
    OperandFlags flags : 8;
    Val val;
};

typedef struct OperandMapEntry OperandMapEntry;
struct OperandMapEntry {
    Expr *key;
    Operand value;
};

void check(Package *package, Stmt *stmt);
Val resolve_value(Package *package, Expr *expr);
void scope_declare(Scope *scope, Sym *sym);
Scope *scope_push(Package *package, Scope *parent);
