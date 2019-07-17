#pragma once

// package.h
typedef struct Package Package;
typedef struct Source Source;

#define EXPR_KIND_BASE 0x20
#define STMT_KIND_BASE 0x40
#define DECL_KIND_BASE 0x80

#define ISEXPR(ast) (((ast)->kind & EXPR_KIND_BASE) != 0)
#define ISSTMT(ast) (((ast)->kind & STMT_KIND_BASE) != 0)
#define ISDECL(ast) (((ast)->kind & DECL_KIND_BASE) != 0)

typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;

typedef struct Range Range;
struct Range {
    u32 start;
    u32 end;
};

typedef struct Ast Ast;
struct Ast {
    u8 kind : 8;
    u8 flags : 8;
    Range range;
};

typedef struct ExprString ExprString;
struct ExprString {
    char *str;
    u32 len;
    bool mapped;
};

typedef enum CompoundFieldKind {
    COMPOUND_FIELD_NONE  = 0x00,
    FIELD_NAME  = 0x01,
    FIELD_INDEX = 0x02,
} CompoundFieldKind;

typedef struct CompoundField CompoundField;
struct CompoundField {
    CompoundFieldKind kind;
    Expr *key;
    Expr *val;
};

typedef struct ExprCompound ExprCompound;
struct ExprCompound {
    Expr *type;
    CompoundField *fields;
};

typedef struct ExprCast ExprCast;
struct ExprCast {
    Expr *type;
    Expr *expr;
};

typedef struct ExprBinary ExprBinary;
struct ExprBinary {
    Expr *elhs;
    Expr *erhs;
};

typedef struct ExprTernary ExprTernary;
struct ExprTernary {
    Expr *econd;
    Expr *epass;
    Expr *efail;
};

typedef struct CallArg CallArg;
struct CallArg {
    Expr *name; // Ident
    Expr *expr;
};

typedef struct ExprCall ExprCall;
struct ExprCall {
    Expr *expr;
    CallArg *args; // arr
};

typedef struct ExprField ExprField;
struct ExprField {
    Expr *expr;
    Expr *name;
};

typedef struct ExprIndex ExprIndex;
struct ExprIndex {
    Expr *expr;
    Expr *index;
};

typedef struct ExprSlice ExprSlice;
struct ExprSlice {
    Expr *base;
    Expr *lo;
    Expr *hi;
};

typedef enum FuncFlags {
    FUNC_NONE = 0,
    FUNC_VARGS = 1 << 0,
    FUNC_CVARGS = 1 << 1
} FuncFlags;

typedef struct ExprFunc ExprFunc;
struct ExprFunc {
    Expr *type; // FuncType
    Stmt *body;
};

typedef struct FuncParam FuncParam;
struct FuncParam {
    Expr *name;
    Expr *type;
};

typedef struct ExprFuncType ExprFuncType;
struct ExprFuncType {
    FuncParam *params; // arr
    FuncParam *result; // arr
};

typedef struct ExprArray ExprArray;
struct ExprArray {
    Expr *base;
    Expr *len;
};

typedef struct ExprPointer ExprPointer;
struct ExprPointer {
    Expr *base;
};

typedef struct AggregateField AggregateField;
struct AggregateField {
    Expr **names; // arr ExprName
    Expr *type;
};

typedef struct ExprAggregate ExprAggregate;
struct ExprAggregate {
    AggregateField *fields; // arr
};

typedef enum EnumFlags {
    ENUM_NONE = 0,
    ENUM_FLAGS = 1 << 0,
} EnumFlags;

typedef struct EnumItem EnumItem;
struct EnumItem {
    Expr *name; // ExprName
    Expr *init;
};

typedef struct ExprEnum ExprEnum;
struct ExprEnum {
    Expr *type;
    EnumItem *items; // arr EnumItem
};

typedef struct DeclVal DeclVal;
struct DeclVal {
    Expr *name; // ExprName
    Expr *type; // opt
    Expr *val;
};

typedef struct DeclVar DeclVar;
struct DeclVar {
    Expr **names; // arr ExprName
    Expr *type; // opt
    Expr **vals; // arr
};

typedef struct ImportItem ImportItem;
struct ImportItem {
    Expr *name; // ExprName
    Expr *alias; // opt ExprName
};

typedef struct DeclImport DeclImport;
struct DeclImport {
    Expr *path;
    Expr *alias; // opt ExprName
    ImportItem *items; // arr
};

typedef struct DeclLibrary DeclLibrary;
struct DeclLibrary {
    Expr *path;
    Expr *alias; // opt ExprName
};

typedef struct DeclForeign DeclForeign;
struct DeclForeign {
    Expr *name;
    Expr *library;
    Expr *type;
    Expr *linkname; // opt
    Expr *callconv; // opt
    Decl *block; // opt
};

typedef struct DeclForeignBlock DeclForeignBlock;
struct DeclForeignBlock {
    Expr *linkprefix; // opt
    Expr *callconv; // opt
    Decl **decls; // arr DeclForeign
};

typedef struct StmtAssign StmtAssign;
struct StmtAssign {
    Expr **lhs; // arr
    Expr **rhs; // arr
};

typedef enum GotoKind {
    GOTO_NONE,
    GOTO_CONTINUE,
    GOTO_FALLTHROUGH,
    GOTO_BREAK,
    GOTO_GOTO,
} GotoKind;

typedef struct StmtIf StmtIf;
struct StmtIf {
    Expr *cond;
    Stmt *pass;
    Stmt *fail;
};

typedef enum ForKind {
    FOR_NONE,
    FOR_REGULAR,
    FOR_AGGREGATE,
} ForKind;

typedef struct StmtFor StmtFor;
struct StmtFor {
    union {
        Stmt *init;
        Expr *value_name; // ExprName
    };
    union {
        Expr *cond;
        Expr *index_name; // ExprName
    };
    union {
        Stmt *step;
        Expr *aggregate;
    };
    Stmt *body;
};

typedef struct SwitchCase SwitchCase;
struct SwitchCase {
    Expr **matches; // arr
    Stmt *body; // StmtBlock
};

typedef struct StmtSwitch StmtSwitch;
struct StmtSwitch {
    Expr *subject;
    SwitchCase *cases; // arr
};

typedef enum ExprKind {
    EXPR_NIL       = EXPR_KIND_BASE + 0x00,
    EXPR_INT       = EXPR_KIND_BASE + 0x01,
    EXPR_FLOAT     = EXPR_KIND_BASE + 0x02,
    EXPR_STR       = EXPR_KIND_BASE + 0x03,
    EXPR_NAME      = EXPR_KIND_BASE + 0x04,
    EXPR_COMPOUND  = EXPR_KIND_BASE + 0x05,
    EXPR_CAST      = EXPR_KIND_BASE + 0x06,
    EXPR_PAREN     = EXPR_KIND_BASE + 0x07,
    EXPR_UNARY     = EXPR_KIND_BASE + 0x08,
    EXPR_BINARY    = EXPR_KIND_BASE + 0x09,
    EXPR_TERNARY   = EXPR_KIND_BASE + 0x0A,
    EXPR_CALL      = EXPR_KIND_BASE + 0x0B,
    EXPR_FIELD     = EXPR_KIND_BASE + 0x0C,
    EXPR_INDEX     = EXPR_KIND_BASE + 0x0D,
    EXPR_SLICE     = EXPR_KIND_BASE + 0x0E,
    EXPR_FUNC      = EXPR_KIND_BASE + 0x0F,
    EXPR_FUNCTYPE  = EXPR_KIND_BASE + 0x11,
    EXPR_SLICETYPE = EXPR_KIND_BASE + 0x12,
    EXPR_ARRAY     = EXPR_KIND_BASE + 0x13,
    EXPR_POINTER   = EXPR_KIND_BASE + 0x14,
    EXPR_STRUCT    = EXPR_KIND_BASE + 0x15,
    EXPR_UNION     = EXPR_KIND_BASE + 0x16,
    EXPR_ENUM      = EXPR_KIND_BASE + 0x17,
} ExprKind;

typedef enum Op {
    OP_NONE = 0,
    OP_ADD  = '+',
    OP_SUB  = '-',
    OP_MUL  = '*',
    OP_DIV  = '/',
    OP_REM  = '%',
    OP_AND  = '&',
    OP_OR   = '|',
    OP_XOR  = '^',
    OP_SHL  = '_',
    OP_SHR  = '`',
    OP_NOT  = '!',
    OP_BNOT = '~',
    OP_LAND = 'a',
    OP_LOR  = 'o',
    OP_LSS  = '<',
    OP_GTR  = '>',
    OP_EQL  = '=' | 0x80,
    OP_NEQ  = '!' | 0x80,
    OP_LEQ  = '<' | 0x80,
    OP_GEQ  = '>' | 0x80,
    NUM_OPS,
} Op;

typedef struct Expr Expr;
struct Expr {
    ExprKind kind : 8;
    u8 flags;
    Range range;
    union {
        u8 enil[0];
        u64 eint;
        f64 efloat;
        const char *ename;
        ExprString estr;
        ExprCompound ecompound;
        ExprCast ecast;
        Expr *eparen;
        Expr *eunary;
        ExprBinary ebinary;
        ExprTernary eternary;
        ExprCall ecall;
        ExprField efield;
        ExprIndex eindex;
        ExprSlice eslice;
        ExprFunc efunc;

        ExprFuncType efunctype;
        ExprArray earray;
        Expr *eslicetype;
        ExprPointer epointer;
        ExprAggregate estruct;
        ExprAggregate eunion;
        ExprEnum eenum;
    };
};

typedef enum DeclKind {
    DECL_VAR          = DECL_KIND_BASE + 0x0,
    DECL_VAL          = DECL_KIND_BASE + 0x1,
    DECL_IMPORT       = DECL_KIND_BASE + 0x2,
    DECL_LIBRARY      = DECL_KIND_BASE + 0x3,
    DECL_FOREIGN      = DECL_KIND_BASE + 0x4,
    DECL_FOREIGNBLOCK = DECL_KIND_BASE + 0x5,
    DECL_FILE         = DECL_KIND_BASE + 0x6,
} DeclKind;

typedef enum DeclFlags {
    DECL_NONE = 0,
    DECL_CONSTANT = 1 << 0, // used to indicate constant foreign decls
} DeclFlags;

typedef struct Decl Decl;
struct Decl {
    DeclKind kind : 8;
    u8 flags;
    Range range;
    union {
        Source *dfile;
        DeclVal dval;
        DeclVar dvar;
        DeclImport dimport;
        DeclLibrary dlibrary;
        DeclForeign dforeign;
        DeclForeignBlock dforeign_block;
    };
};

typedef enum StmtKind {
    STMT_LABEL  = STMT_KIND_BASE + 0x0,
    STMT_ASSIGN = STMT_KIND_BASE + 0x1,
    STMT_RETURN = STMT_KIND_BASE + 0x2,
    STMT_DEFER  = STMT_KIND_BASE + 0x3,
    STMT_USING  = STMT_KIND_BASE + 0x4,
    STMT_GOTO   = STMT_KIND_BASE + 0x5,
    STMT_BLOCK  = STMT_KIND_BASE + 0x6,
    STMT_IF     = STMT_KIND_BASE + 0x7,
    STMT_FOR    = STMT_KIND_BASE + 0x8,
    STMT_SWITCH = STMT_KIND_BASE + 0x9,
    STMT_NAMES  = STMT_KIND_BASE + 0xA,
} StmtKind;

typedef struct Stmt Stmt;
struct Stmt {
    StmtKind kind : 8;
    u8 flags;
    Range range;
    union {
        Expr *slabel;
        StmtAssign sassign;
        Expr **sreturn; // arr
        Stmt *sdefer;
        Expr **susing; // arr
        Expr *sgoto;
        Stmt **sblock; // arr
        StmtIf sif;
        StmtFor sfor;
        StmtSwitch sswitch;

        Expr **snames; // arr
    };
};

void *new_ast_invalid(Package *package, Range range);
Expr *new_expr_nil(Package *package, Range range);
Expr *new_expr_paren(Package *package, Range range, Expr *expr);
Expr *new_expr_int(Package *package, Range range, u64 val);
Expr *new_expr_float(Package *package, Range range, f64 val);
Expr *new_expr_str(Package *package, Range range, const char *str, u32 len, bool mapped);
Expr *new_expr_name(Package *package, Range range, const char *name);
Expr *new_expr_compound(Package *package, Range range, Expr *type, CompoundField *fields);
Expr *new_expr_cast(Package *package, Range range, Expr *type, Expr *expr);
Expr *new_expr_unary(Package *package, Range range, Op op, Expr *expr);
Expr *new_expr_binary(Package *package, Range range, Op op, Expr *left, Expr *right);
Expr *new_expr_ternary(Package *package, Range range, Expr *cond, Expr *pass, Expr *fail);
Expr *new_expr_call(Package *package, Range range, Expr *expr, CallArg *args);
Expr *new_expr_field(Package *package, Range range, Expr *expr, Expr *name);
Expr *new_expr_index(Package *package, Range range, Expr *expr, Expr *index);
Expr *new_expr_slice(Package *package, Range range, Expr *base, Expr *lo, Expr *hi);
Expr *new_expr_func(Package *package, Range range, FuncFlags flags, Expr *type, Stmt *body);
Expr *new_expr_functype(Package *package, Range range,
                        FuncFlags flags, FuncParam *params, FuncParam *result);
Expr *new_expr_array(Package *package, Range range, Expr *base, Expr *len);
Expr *new_expr_slicetype(Package *package, Range range, Expr *base);
Expr *new_expr_pointer(Package *package, Range range, Expr *base);
Expr *new_expr_struct(Package *package, Range range, AggregateField *fields);
Expr *new_expr_union(Package *package, Range range, AggregateField *fields);
Expr *new_expr_enum(Package *package, Range range, EnumFlags flags, Expr *type, EnumItem *items);
Decl *new_decl_file(Package *package, Source *file);
Decl *new_decl_val(Package *package, Range range, Expr *name, Expr *type, Expr *val);
Decl *new_decl_var(Package *package, Range range, Expr **names, Expr *type, Expr **vals);
Decl *new_decl_import(Package *package, Range range, Expr *path, Expr *alias, ImportItem *items);
Decl *new_decl_library(Package *package, Range range, Expr *path, Expr *alias);
Decl *new_decl_foreign(Package *package, Range range, DeclFlags flags,
                       Expr *name, Expr *library, Expr *type,
                       Expr *linkname, Expr *callconv, Decl *block);
Decl *new_decl_foreign_block(Package *package, Range range, Decl **decls,
                             Expr *linkprefix, Expr *callconv);
Stmt *new_stmt_label(Package *package, Range range, Expr *label);
Stmt *new_stmt_assign(Package *package, Range range, Expr **lhs, Expr **rhs);
Stmt *new_stmt_return(Package *package, Range range, Expr **exprs);
Stmt *new_stmt_defer(Package *package, Range range, Stmt *stmt);
Stmt *new_stmt_using(Package *package, Range range, Expr **exprs);
Stmt *new_stmt_goto(Package *package, Range range, GotoKind kind, Expr *expr);
Stmt *new_stmt_block(Package *package, Range range, Stmt **stmts);
Stmt *new_stmt_if(Package *package, Range range, Expr *cond, Stmt *pass, Stmt *fail);
Stmt *new_stmt_for(Package *package, Range range, Stmt *init, Expr *cond, Stmt *step, Stmt *body);
Stmt *new_stmt_for_aggregate(Package *package, Range range,
                             Expr *value, Expr *index, Expr *aggregate, Stmt *body);
Stmt *new_stmt_switch(Package *package, Range range, Expr *match, SwitchCase *cases);
// Temp node! Needs free!
Stmt *new_stmt_names(Package *package, Range range, Expr **names);
const char *describe_goto_kind(int kind);
const char *describe_ast_kind(int kind);
const char *describe_ast(Package *package, void *p);
const char *describe_op(Op op);
