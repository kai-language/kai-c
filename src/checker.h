#pragma once

// requires nothing

// package.h
typedef struct Package Package;
typedef struct SymMapEntry SymMapEntry;

// ast.h
typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Decl Decl;

// types.h
typedef struct Ty Ty;

typedef union Val Val;
union Val {
    i64 i;
    u64 u;
    f64 f;
    void *p;
};

typedef enum SymKind {
    SYM_NONE = 0,
    SYM_TYPE,
    SYM_VAR,
    SYM_VAL,
    SYM_ARG,
    SYM_PKG,
    SYM_LIB,
    SYM_LABEL,
} SymKind;

typedef enum SymState {
    SYM_UNCHECKED = 0,
    SYM_CHECKING,
    SYM_CHECKED,
} SymState;

typedef enum Reachable {
    REACHABLE_NONE = 0,
    REACHABLE_NATURAL,
    REACHABLE_FORCED,
} Reachable;

typedef struct Sym Sym;
struct Sym {
    const char *name;
    Package *owning_package;
    SymKind kind : 8;
    SymState state : 8; // TODO: For multithreading we will need a threadid for cycle detection
    Reachable reachable : 8;
    Decl *decl;
    const char *external_name;
    void *userdata; // backend data
    union {
        struct {
            Ty *type;
            Val val;
        };
        Package *package;
    };
};

typedef struct Scope Scope;
struct Scope {
    Scope *parent;
    SymMapEntry *members;
};

typedef enum OperandFlags { // lower 4 bits are flags upper are kind
    OPERAND_FLAGS_NONE = 0,
    LVALUE     = 0x01,
    CONST      = 0x02,

    OPERAND_OK = 0x10,
    PACKAGE    = 0x20,
    LIBRARY    = 0x30,
    TYPE       = 0x40,
    LABEL      = 0x50,
    UNCHECKED  = 0xFE,
    BAD_VALUE  = 0xFF,
} OperandFlags;

typedef struct Operand Operand;
struct Operand {
    Ty *type;
    OperandFlags flags : 8;
    Val val;
};

typedef struct OperandMapEntry OperandMapEntry;
struct OperandMapEntry {
    void *key;
    Operand value;
};

bool check(Package *package, Stmt *stmt);
Val resolve_value(Package *package, Expr *expr);
void scope_declare(Scope *scope, Sym *sym);
Scope *scope_push(Package *package, Scope *parent);
