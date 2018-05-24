#define EXPR_KIND_START       0x100
#define STMT_KIND_START       0x200
#define DECL_KIND_START       0x300
#define AST_KIND_START        0x400

#define EXPR_KINDS                                           \
    FOR_EACH(Ident, "identifier", true)                      \
    FOR_EACH(Paren, "parenthesis", false)                    \
    FOR_EACH(Call, "call", false)                            \
    FOR_EACH(Selector, "selector", true)                     \
    FOR_EACH(Subscript, "subscript", false)                  \
    FOR_EACH(Slice, "slice", false)                          \
    FOR_EACH(Unary, "unary", false)                          \
    FOR_EACH(Binary, "binary", false)                        \
    FOR_EACH(Ternary, "ternary", false)                      \
    FOR_EACH(Cast, "cast", false)                            \
    FOR_EACH(Autocast, "autocast", false)                    \
    FOR_EACH(KeyValue, "key value", false)                   \
    FOR_EACH(LocationDirective, "location directive", false) \
    FOR_EACH(LitNil, "nil literal", false)                   \
    FOR_EACH(LitInt, "integer literal", false)               \
    FOR_EACH(LitFloat, "float literal", false)               \
    FOR_EACH(LitString, "string literal", false)             \
    FOR_EACH(LitCompound, "compound literal", false)         \
    FOR_EACH(LitFunction, "function literal", false)         \
    FOR_EACH(TypePointer, "pointer type", false)             \
    FOR_EACH(TypeArray, "array type", false)                 \
    FOR_EACH(TypeSlice, "slice type", false)                 \
    FOR_EACH(TypeStruct, "struct type", false)               \
    FOR_EACH(TypeEnum, "enum type", false)                   \
    FOR_EACH(TypeUnion, "union type", false)                 \
    FOR_EACH(TypePolymorphic, "polymorphic type", false)     \
    FOR_EACH(TypeVariadic, "variadic type", false)           \
    FOR_EACH(TypeFunction, "function type", false)

#define STMT_KINDS                        \
    FOR_EACH(Empty, "empty", false)       \
    FOR_EACH(Label, "label", false)       \
    FOR_EACH(Assign, "assignment", false) \
    FOR_EACH(Return, "return", false)     \
    FOR_EACH(Defer, "defer", false)       \
    FOR_EACH(Using, "using", false)       \
    FOR_EACH(Goto, "goto", false)         \
    FOR_EACH(Block, "block", false)       \
    FOR_EACH(If, "if", false)             \
    FOR_EACH(For, "for", false)           \
    FOR_EACH(ForIn, "for in", false)      \
    FOR_EACH(Switch, "switch", false)

// TODO: This needs to include some more directives (for example static asserts `#assert` should be supported at top level)
#define DECL_KINDS                        \
    FOR_EACH(Variable, "variable", false) \
    FOR_EACH(Constant, "constant", false) \
    FOR_EACH(Import, "import", false)

typedef enum ExprKind {
    ExprKind_Invalid = 0,

    _ExprKind_Start = EXPR_KIND_START,
#define FOR_EACH(kindName, ...) ExprKind_##kindName,
    EXPR_KINDS
#undef FOR_EACH
    _ExprKind_End,
} ExprKind;

typedef enum StmtKind {
    StmtKind_Invalid = 0,

    _StmtKind_Start = STMT_KIND_START,
#define FOR_EACH(kindName, ...) StmtKind_##kindName,
    STMT_KINDS
#undef FOR_EACH
    _StmtKind_End,

    _StmtExprKind_Start = EXPR_KIND_START,
#define FOR_EACH(kindName, ...) StmtExprKind_##kindName,
    EXPR_KINDS
#undef FOR_EACH
    _StmtExprKind_End,

    _StmtDeclKind_Start = DECL_KIND_START,
#define FOR_EACH(kindName, ...) StmtDeclKind_##kindName,
    DECL_KINDS
#undef FOR_EACH
    _StmtDeclKind_End
} StmtKind;

typedef enum DeclKind {
    DeclKind_Invalid = 0,

    _DeclKind_Start = DECL_KIND_START,
#define FOR_EACH(kindName, ...) DeclKind_##kindName,
    DECL_KINDS
#undef FOR_EACH
    _DeclKind_End,
} DeclKind;


typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;

// predefine all Ast Structs
#define FOR_EACH(kindName, s, ...) typedef struct Expr_##kindName Expr_##kindName;
EXPR_KINDS
#undef FOR_EACH

#define FOR_EACH(kindName, s, ...) typedef struct Stmt_##kindName Stmt_##kindName;
STMT_KINDS
#undef FOR_EACH

#define FOR_EACH(kindName, s, ...) typedef struct Decl_##kindName Decl_##kindName;
DECL_KINDS
#undef FOR_EACH

typedef struct AstInvalid AstInvalid;
struct AstInvalid {
    Position start;
    Position end;
};

struct Expr_Ident {
    Position start;
    const char *name;
};

struct Expr_Paren {
    Position start;
    Expr *expr;
    Position end;
};

struct Expr_Call {
    Position start;
    Expr *expr;
    DynamicArray(Expr_KeyValue *) args;
    Position end;
};

struct Expr_Selector {
    Position start;
    Expr *expr;
    const char *name;
    Position end;
};

struct Expr_Subscript {
    Position start;
    Expr *expr;
    Expr *index;
    Position end;
};

struct Expr_Slice {
    Position start;
    Expr *expr;
    Expr *lo;
    Expr *hi;
    Position end;
};

struct Expr_Unary {
    Position start;
    TokenKind op;
    Expr *expr;
};

struct Expr_Binary {
    Position start;
    TokenKind op;
    Position pos;
    Expr *lhs;
    Expr *rhs;
};

struct Expr_Ternary {
    Position start;
    Expr *cond;
    Expr *pass;
    Expr *fail;
};

struct Expr_Cast {
    Position start;
    Expr *type;
    Expr *expr;
};

struct Expr_Autocast {
    Position start;
    Expr *expr;
};

typedef enum KeyValueFlags {
    KeyValueFlagIndex = 1,
} KeyValueFlags;
struct Expr_KeyValue {
    Position start;
    // TODO: We should add support for C style {['0'] = 0; ['1'] = 1; } which may will require a different key type
    Expr *key; 
    Expr *value;
    u8 flags;
};

struct Expr_LocationDirective {
    Position start;
    const char *name;
};

struct Expr_LitNil {
    Position start;
};

struct Expr_LitInt {
    Position start;
    u64 val;
};

struct Expr_LitFloat {
    Position start;
    f64 val;
};

struct Expr_LitString {
    Position start;
    const char *val;
};

struct Expr_LitCompound {
    Position start;
    Expr *type;
    DynamicArray(Expr_KeyValue *) elements;
    Position end;
};

struct Expr_LitFunction {
    Position start;
    Expr *type;
    Stmt_Block *body;
    u8 flags;
};

struct Expr_TypePointer {
    Position start;
    Expr *type;
};

struct Expr_TypeArray {
    Position start;
    Expr *length;
    Expr *type;
};

struct Expr_TypeSlice {
    Position start;
    Expr *type;
};

typedef struct AggregateItem AggregateItem;
struct AggregateItem {
    Position start;
    DynamicArray(const char *) names;
    Expr *type;
};

struct Expr_TypeStruct {
    Position start;
    DynamicArray(AggregateItem) items;
};

struct Expr_TypeUnion {
    Position start;
    DynamicArray(AggregateItem) items;
};

typedef struct EnumItem EnumItem;
struct EnumItem {
    Position start;
    const char *name;
    Expr *init;
};

struct Expr_TypeEnum {
    Position start;
    Expr *explicitType;
    DynamicArray(EnumItem) items;
};

struct Expr_TypePolymorphic {
    Position start;
    const char *name;
};

typedef enum TypeVariadicFlags {
    TypeVariadicFlagCVargs = 1,
} TypeVariadicFlags;
struct Expr_TypeVariadic {
    Position start;
    Expr *type;
    b8 flags;
};

struct Expr_TypeFunction {
    Position start;
    DynamicArray(Expr *) result;
    DynamicArray(Expr_KeyValue *) params;
};

struct Stmt_Empty {
    Position start;
};

struct Stmt_Label {
    Position start;
    const char *name;
};

struct Stmt_Assign {
    Position start;
    DynamicArray(Expr *) lhs;
    DynamicArray(Expr *) rhs;
};

struct Stmt_Return {
    Position start;
    DynamicArray(Expr *) exprs;
};

struct Stmt_Defer {
    Position start;
    Stmt *stmt;
};

struct Stmt_Using {
    Position start;
    Expr *expr;
};

struct Stmt_Goto {
    Position start;
    const char *keyword;
    Expr_Ident *label;
};

struct Stmt_Block {
    Position start;
    DynamicArray(Stmt *) stmts;
    Position end;
};

struct Stmt_If {
    Position start;
    Expr *cond;
    Stmt *pass;
    Stmt *fail;
};

struct Stmt_For {
    Position start;
    Stmt *init;
    Expr *cond;
    Stmt *step;
    Stmt_Block *body;
};

struct Stmt_ForIn {
    Position start;
    Expr_Ident *valueName;
    Expr_Ident *indexName;
    Expr *aggregate;
    Stmt_Block *body;
};

typedef struct SwitchCase SwitchCase;
struct SwitchCase {
    Token token;
    DynamicArray(Expr *) matches;
    Stmt_Block *block;
};

struct Stmt_Switch {
    Position start;
    Expr *match;
    DynamicArray(SwitchCase *) cases;
};

struct Decl_Variable {
    Position start;
    DynamicArray(Expr_Ident *) names;
    Expr *type;
    DynamicArray(Expr *) values;
};

struct Decl_Constant {
    Position start;
    DynamicArray(Expr_Ident *) names;
    Expr *type;
    DynamicArray(Expr *) values;
};

struct Decl_Import {
    Position start;
    const char *path;
    const char *alias;
};

#define FOR_EACH(kindName, s, ...) Expr_##kindName kindName;
typedef union ExprValue ExprValue;
union ExprValue {
    EXPR_KINDS
};
#undef FOR_EACH

#define FOR_EACH(kindName, s, ...) Stmt_##kindName kindName;
typedef union StmtValue StmtValue;
union StmtValue {
    STMT_KINDS
};
#undef FOR_EACH

#define FOR_EACH(kindName, s, ...) Decl_##kindName kindName;
typedef union DeclValue DeclValue;
union DeclValue {
    DECL_KINDS
};
#undef FOR_EACH

struct Stmt {
    u64 id;
    StmtKind kind;
    union {
        Position start;
        StmtValue stmt;
        ExprValue expr;
        DeclValue decl;
        AstInvalid Invalid;

#define FOR_EACH(kindName, ...) Stmt_##kindName kindName;
        STMT_KINDS
#undef FOR_EACH

#define FOR_EACH(kindName, ...) Expr_##kindName kindName;
        EXPR_KINDS
#undef FOR_EACH

#define FOR_EACH(kindName, ...) Decl_##kindName kindName;
        DECL_KINDS
#undef FOR_EACH
    };
};

struct Expr {
    u64 id;
    ExprKind kind;
    union {
        Position start;
        ExprValue expr;
        AstInvalid Invalid;

#define FOR_EACH(kindName, ...) Expr_##kindName kindName;
        EXPR_KINDS
#undef FOR_EACH
    };
};

struct Decl {
    u64 id;
    DeclKind kind;
    union {
        Position start;
        DeclValue decl;
        AstInvalid Invalid;

#define FOR_EACH(kindName, ...) Decl_##kindName kindName;
        DECL_KINDS
#undef FOR_EACH
    };
};

b32 isExpr(Stmt *stmt);

b32 isDecl(Stmt *stmt);

void *AllocAst(Package *package, size_t size);

// - MARK: Ends
Position EndForStmt(Stmt *stmt);
Position EndForExpr(Expr *expr);
Position EndForDecl(Decl *decl);

// - MARK: Exprs
Expr *NewExpr(Package *package, ExprKind kind, Position start);
Stmt *NewStmt(Package *package, StmtKind kind, Position start);
Decl *NewDecl(Package *package, DeclKind kind, Position start);
Expr *NewExprInvalid(Package *package, Position start, Position end);
Stmt *NewStmtInvalid(Package *package, Position start, Position end);
Decl *NewDeclInvalid(Package *package, Position start, Position end);
Expr *NewExprIdent(Package *package, Position start, const char *name);
Expr *NewExprParen(Package *package, Expr *expr, Position start, Position end);
Expr *NewExprCall(Package *package, Expr *expr, DynamicArray(Expr_KeyValue *) args, Position end);
Expr *NewExprSelector(Package *package, Expr *expr, const char *name, Position end);
Expr *NewExprSubscript(Package *package, Expr *expr, Expr *index, Position end);
Expr *NewExprSlice(Package *package, Expr *expr, Expr *lo, Expr *hi, Position end);
Expr *NewExprUnary(Package *package, Position start, TokenKind op, Expr *expr);
Expr *NewExprBinary(Package *package, TokenKind op, Position pos, Expr *lhs, Expr *rhs);
Expr *NewExprTernary(Package *package, Expr *cond, Expr *pass, Expr *fail);
Expr *NewExprCast(Package *package, Position start, Expr *type, Expr *expr);
Expr *NewExprAutocast(Package *package, Position start, Expr *expr);
Expr *NewExprKeyValue(Package *package, Expr *key, Expr *value);
Expr *NewExprLocationDirective(Package *package, Position start, const char *name);
Expr *NewExprLitNil(Package *package, Position start);
Expr *NewExprLitInt(Package *package, Position start, u64 val);
Expr *NewExprLitFloat(Package *package, Position start, f64 val);
Expr *NewExprLitString(Package *package, Position start, const char *val);
Expr *NewExprLitCompound(Package *package, Position start, Expr *type, DynamicArray(Expr_KeyValue *) elements, Position end);
Expr *NewExprLitFunction(Package *package, Expr *type, Stmt_Block *body, u8 flags);
Expr *NewExprTypePointer(Package *package, Position start, Expr *type);
Expr *NewExprTypeArray(Package *package, Position start, Expr *length, Expr *type);
Expr *NewExprTypeSlice(Package *package, Position start, Expr *type);
Expr *NewExprTypeStruct(Package *package, Position start, DynamicArray(AggregateItem) items);
Expr *NewExprTypeEnum(Package *package, Position start, Expr *explicitType, DynamicArray(EnumItem) items);
Expr *NewExprTypeUnion(Package *package, Position start, DynamicArray(AggregateItem) items);
Expr *NewExprTypePolymorphic(Package *package, Position start, const char *name);
Expr *NewExprTypeVariadic(Package *package, Position start, Expr *type, u8 flags);
Expr *NewExprTypeFunction(Package *package, Position start, DynamicArray(Expr_KeyValue *) params, DynamicArray(Expr *)result);

// - MARK: Stmts
Stmt *NewStmtEmpty(Package *package, Position start);
Stmt *NewStmtLabel(Package *package, Position start, const char *name);
Stmt *NewStmtAssign(Package *package, Position start, DynamicArray(Expr *) lhs, DynamicArray(Expr*) rhs);
Stmt *NewStmtReturn(Package *package, Position start, DynamicArray(Expr *) exprs);
Stmt *NewStmtDefer(Package *package, Position start, Stmt *stmt);
Stmt *NewStmtUsing(Package *package, Position start, Expr *expr);
Stmt *NewStmtGoto(Package *package, Position start, const char *keyword, Expr_Ident *label);
Stmt *NewStmtBlock(Package *package, Position start, DynamicArray(Stmt *) stmts, Position end);
Stmt *NewStmtIf(Package *package, Position start, Expr *cond, Stmt *pass, Stmt *fail);
Stmt *NewStmtFor(Package *package, Position start, Stmt *init, Expr *cond, Stmt *step, Stmt_Block *body);
Stmt *NewStmtForIn(Package *package, Position start, Expr_Ident *valueName, Expr_Ident *indexName, Expr *aggregate, Stmt_Block *body);
Stmt *NewStmtSwitch(Package *package, Position start, Expr *match, DynamicArray(SwitchCase *) cases);

// - MARK: Decls
Decl *NewDeclVariable(Package *package, Position start, DynamicArray(Expr_Ident *) names, Expr *type, DynamicArray(Expr *) values);
Decl *NewDeclConstant(Package *package, Position start, DynamicArray(Expr_Ident *) names, Expr *type, DynamicArray(Expr *) values);
Decl *NewDeclImport(Package *package, Position start, const char *path, const char *alias);
