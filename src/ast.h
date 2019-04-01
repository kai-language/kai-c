
#define EXPR_KIND_START       0x20
#define STMT_KIND_START       0x40
#define DECL_KIND_START       0x80

#define EXPR_KINDS                                           \
    FOR_EACH(Ident, "identifier", true)                      \
    FOR_EACH(Paren, "parenthesis", false)                    \
    FOR_EACH(Call, "call", true)                             \
    FOR_EACH(Cast, "cast", true)                             \
    FOR_EACH(Selector, "selector", true)                     \
    FOR_EACH(Subscript, "subscript", true)                   \
    FOR_EACH(Slice, "slice", true)                           \
    FOR_EACH(Unary, "unary", true)                           \
    FOR_EACH(Binary, "binary", true)                         \
    FOR_EACH(Ternary, "ternary", true)                       \
    FOR_EACH(Autocast, "autocast", false)                    \
    FOR_EACH(LocationDirective, "location directive", true)  \
    FOR_EACH(LitNil, "nil literal", true)                    \
    FOR_EACH(LitInt, "integer literal", true)                \
    FOR_EACH(LitFloat, "float literal", true)                \
    FOR_EACH(LitString, "string literal", true)              \
    FOR_EACH(LitCompound, "compound literal", true)          \
    FOR_EACH(LitFunction, "function literal", true)          \
    FOR_EACH(TypePointer, "pointer type", true)              \
    FOR_EACH(TypeArray, "array type", true)                  \
    FOR_EACH(TypeSlice, "slice type", true)                  \
    FOR_EACH(TypeStruct, "struct type", true)                \
    FOR_EACH(TypeEnum, "enum type", true)                    \
    FOR_EACH(TypeUnion, "union type", true)                  \
    FOR_EACH(TypePolymorphic, "polymorphic type", true)      \
    FOR_EACH(TypeVariadic, "variadic type", true)            \
    FOR_EACH(TypeFunction, "function type", true)

#define STMT_KINDS                        \
    FOR_EACH(Empty, "empty", false)       \
    FOR_EACH(Label, "label", true)        \
    FOR_EACH(Assign, "assignment", false) \
    FOR_EACH(Return, "return", false)     \
    FOR_EACH(Defer, "defer", false)       \
    FOR_EACH(Using, "using", false)       \
    FOR_EACH(Goto, "goto", true)          \
    FOR_EACH(Block, "block", false)       \
    FOR_EACH(If, "if", false)             \
    FOR_EACH(For, "for", true)            \
    FOR_EACH(ForIn, "for in", true)       \
    FOR_EACH(Switch, "switch", true)      \
    FOR_EACH(SwitchCase, "case", true)

// TODO: This needs to include some more directives (for example static asserts `#assert` should be supported at top level)
#define DECL_KINDS                       \
    FOR_EACH(Variable, "variable", true) \
    FOR_EACH(Constant, "constant", true) \
    FOR_EACH(Foreign, "foreign", true)   \
    FOR_EACH(ForeignBlock, "foreign block", false) \
    FOR_EACH(Import, "import", true)

typedef u8 ExprKind;
enum Enum_ExprKind {
    ExprKind_Invalid = 0,
    
    _ExprKind_Start = EXPR_KIND_START,
#define FOR_EACH(kindName, ...) ExprKind_##kindName,
    EXPR_KINDS
#undef FOR_EACH
    _ExprKind_End,
};

typedef u8 StmtKind;
enum Enum_StmtKind {
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
};

typedef u8 DeclKind;
enum Enum_DeclKind {
    DeclKind_Invalid = 0,
    
    _DeclKind_Start = DECL_KIND_START,
#define FOR_EACH(kindName, ...) DeclKind_##kindName,
    DECL_KINDS
#undef FOR_EACH
    _DeclKind_End,
};

STATIC_ASSERT(_ExprKind_End <= UINT8_MAX, "enum values overflow storage type");
STATIC_ASSERT(_StmtKind_End <= UINT8_MAX, "enum values overflow storage type");
STATIC_ASSERT(_DeclKind_End <= UINT8_MAX, "enum values overflow storage type");

typedef void * AstNode;
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

typedef u8 KeyValueFlag;
enum Enum_KeyValueFlag {
    KeyValueFlag_Index = 1,
};
typedef struct KeyValue {
    SourceRange pos;
    Expr *key;
    Expr *value;
    KeyValueFlag flags;

    // info is set by the checker. This is the only node to have checker info
    // inlined onto the node instead of using an Expr id. The reason for this
    // is that KeyValues are inlined directly for both composite literals and
    // function literals. This will actually save space because the id is 64
    // bits and there is also a union tag that this doesn't have.
    void *info;
    // if LitCompound is Array -> u64
    // if LitCompound is Struct -> TypeField*
} KeyValue;

typedef struct AggregateItem {
    SourceRange pos;
    DynamicArray(const char *) names;
    Expr *type;
} AggregateItem;

typedef struct EnumItem {
    SourceRange pos;
    const char *name;
    Expr *init;
} EnumItem;

typedef struct AstInvalid AstInvalid;
struct AstInvalid {
    SourceRange pos;
};

struct Expr_Ident {
    SourceRange pos;
    const char *name;
};

struct Expr_Paren {
    SourceRange pos;
    Expr *expr;
};

struct Expr_Call {
    SourceRange pos;
    Expr *expr;
    DynamicArray(KeyValue) args;
};

struct Expr_Selector {
    SourceRange pos;
    Expr *expr;
    const char *name;
};

struct Expr_Subscript {
    SourceRange pos;
    Expr *expr;
    Expr *index;
};

struct Expr_Slice {
    SourceRange pos;
    Expr *expr;
    Expr *lo;
    Expr *hi;
};

struct Expr_Unary {
    SourceRange pos;
    TokenKind op;
    Expr *expr;
};

struct Expr_Binary {
    SourceRange pos;
    Token op;
    Expr *lhs;
    Expr *rhs;
};

struct Expr_Ternary {
    SourceRange pos;
    Expr *cond;
    Expr *pass;
    Expr *fail;
};

struct Expr_Cast {
    SourceRange pos;
    Expr *type;
    Expr *expr;
};

struct Expr_Autocast {
    SourceRange pos;
    Expr *expr;
};

struct Expr_LocationDirective {
    SourceRange pos;
    const char *name;
};

struct Expr_LitNil {
    SourceRange pos;
};

struct Expr_LitInt {
    SourceRange pos;
    u64 val;
};

struct Expr_LitFloat {
    SourceRange pos;
    f64 val;
};

struct Expr_LitString {
    SourceRange pos;
    const char *val;
};

struct Expr_LitCompound {
    SourceRange pos;
    Expr *type;
    DynamicArray(KeyValue) elements;
};

struct Expr_LitFunction {
    SourceRange pos;
    Expr *type;
    Stmt *body; // Stmt_Block
    u8 flags;
};

struct Expr_TypePointer {
    SourceRange pos;
    Expr *type;
};

struct Expr_TypeArray {
    SourceRange pos;
    Expr *length;
    Expr *type;
};

struct Expr_TypeSlice {
    SourceRange pos;
    Expr *type;
};

struct Expr_TypeStruct {
    SourceRange pos;
    DynamicArray(AggregateItem) items;
};

struct Expr_TypeUnion {
    SourceRange pos;
    DynamicArray(AggregateItem) items;
};

struct Expr_TypeEnum {
    SourceRange pos;
    Expr *explicitType;
    DynamicArray(EnumItem) items;
};

struct Expr_TypePolymorphic {
    SourceRange pos;
    const char *name;
};

typedef u8 TypeVariadicFlag;
enum Enum_TypeVariadicFlag {
    TypeVariadicFlag_CVargs = 1,
};
struct Expr_TypeVariadic {
    SourceRange pos;
    Expr *type;
    TypeVariadicFlag flags;
};

struct Expr_TypeFunction {
    SourceRange pos;
    DynamicArray(Expr *) result;
    DynamicArray(KeyValue) params;
};

struct Stmt_Empty {
    SourceRange pos;
};

struct Stmt_Label {
    SourceRange pos;
    const char *name;
};

struct Stmt_Assign {
    SourceRange pos;
    DynamicArray(Expr *) lhs;
    DynamicArray(Expr *) rhs;
};

struct Stmt_Return {
    SourceRange pos;
    DynamicArray(Expr *) exprs;
};

struct Stmt_Defer {
    SourceRange pos;
    Stmt *stmt;
};

struct Stmt_Using {
    SourceRange pos;
    Expr *expr;
};

struct Stmt_Goto {
    SourceRange pos;
    const char *keyword;
    Expr *target;
};

struct Stmt_Block {
    SourceRange pos;
    DynamicArray(Stmt *) stmts;
};

struct Stmt_If {
    SourceRange pos;
    Expr *cond;
    Stmt *pass;
    Stmt *fail;
};

struct Stmt_For {
    SourceRange pos;
    Stmt *init;
    Expr *cond;
    Stmt *step;
    Stmt *body; // Stmt_Block
};

struct Stmt_ForIn {
    SourceRange pos;
    Expr *valueName; // Expr_Ident
    Expr *indexName; // Expr_Ident
    Expr *aggregate;
    Stmt *body; // Stmt_Block
};

struct Stmt_Switch {
    SourceRange pos;
    Expr *match;
    DynamicArray(Stmt *) cases;
};

struct Stmt_SwitchCase {
    SourceRange pos;
    DynamicArray(Expr *) matches;
    Stmt *body; // Stmt_Block
};

struct Decl_Variable {
    SourceRange pos;
    DynamicArray(Expr *) names; // DynamicArray(Expr_Ident *)
    Expr *type;
    DynamicArray(Expr *) values;
};

struct Decl_Constant {
    SourceRange pos;
    DynamicArray(Expr *) names; // DynamicArray(Expr_Ident *)
    Expr *type;
    DynamicArray(Expr *) values;
};

struct Decl_Foreign {
    SourceRange pos;
    Expr *library;
    bool isConstant;
    const char *name;
    Expr *type;
    const char *linkname;
    const char *callingConvention;
};

typedef struct Decl_ForeignBlockMember Decl_ForeignBlockMember;
struct Decl_ForeignBlockMember {
    SourceRange pos;
    const char *name;
    bool isConstant;
    Expr *type;
    const char *linkname;
    Symbol *symbol;
};

struct Decl_ForeignBlock {
    SourceRange pos;
    Expr *library;
    const char *callingConvention;
    DynamicArray(Decl_ForeignBlockMember) members;
};

struct Decl_Import {
    SourceRange pos;
    Expr *path;
    const char *alias;
    Symbol *symbol;
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
        SourceRange pos;
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
        SourceRange pos;
        ExprValue expr;
        AstInvalid Invalid;
        
#define FOR_EACH(kindName, ...) Expr_##kindName kindName;
        EXPR_KINDS
#undef FOR_EACH
    };
};

// TODO: All decls should have space for a calling convention
struct Decl {
    u64 id;
    DeclKind kind;
    union {
        SourceRange pos;
        DeclValue decl;
        AstInvalid Invalid;
        
#define FOR_EACH(kindName, ...) Decl_##kindName kindName;
        DECL_KINDS
#undef FOR_EACH
    };

    // This is used to switch context for file level declarations. Only expect this to be set for top level declarations
    Scope *owningScope;

    // The following members exist in DEBUG builds to aid in debugging the compiler itself.
    // Anything they enable should be achievable in another way, they are here purely for convenience.
#if DEBUG
    Package *owningPackage;
#endif
};

b32 isExpr(Stmt *stmt);

b32 isDecl(Stmt *stmt);

const char *DescribeStmt(Stmt *stmt);
const char *DescribeExpr(Expr *expr);
const char *DescribeDecl(Decl *decl);

void *AllocAst(Package *package, size_t size);

// - MARK: Exprs
Expr *NewExpr(Package *package, ExprKind kind, SourceRange pos);
Stmt *NewStmt(Package *package, StmtKind kind, SourceRange pos);
Decl *NewDecl(Package *package, DeclKind kind, SourceRange pos);
Expr *NewExprInvalid(Package *package, SourceRange pos);
Stmt *NewStmtInvalid(Package *package, SourceRange pos);
Decl *NewDeclInvalid(Package *package, SourceRange pos);
Expr *NewExprIdent(Package *package, SourceRange pos, const char *name);
Expr *NewExprParen(Package *package, SourceRange pos, Expr *expr);
Expr *NewExprCall(Package *package, SourceRange pos, Expr *expr, DynamicArray(KeyValue) args);
Expr *NewExprSelector(Package *package, SourceRange pos, Expr *expr, const char *name);
Expr *NewExprSubscript(Package *package, SourceRange pos, Expr *expr, Expr *index);
Expr *NewExprSlice(Package *package, SourceRange pos, Expr *expr, Expr *lo, Expr *hi);
Expr *NewExprUnary(Package *package, SourceRange pos, TokenKind op, Expr *expr);
Expr *NewExprBinary(Package *package, SourceRange pos, Token op, Expr *lhs, Expr *rhs);
Expr *NewExprTernary(Package *package, SourceRange pos, Expr *cond, Expr *pass, Expr *fail);
Expr *NewExprCast(Package *package, SourceRange pos, Expr *type, Expr *expr);
Expr *NewExprAutocast(Package *package, SourceRange pos, Expr *expr);
Expr *NewExprLocationDirective(Package *package, SourceRange pos, const char *name);
Expr *NewExprLitNil(Package *package, SourceRange pos);
Expr *NewExprLitInt(Package *package, SourceRange pos, u64 val);
Expr *NewExprLitFloat(Package *package, SourceRange pos, f64 val);
Expr *NewExprLitString(Package *package, SourceRange pos, const char *val);
Expr *NewExprLitCompound(Package *package, SourceRange pos, Expr *type, DynamicArray(KeyValue) elements);
Expr *NewExprLitFunction(Package *package, SourceRange pos, Expr *type, Stmt *body, u8 flags);
Expr *NewExprTypePointer(Package *package, SourceRange pos, Expr *type);
Expr *NewExprTypeArray(Package *package, SourceRange pos, Expr *length, Expr *type);
Expr *NewExprTypeSlice(Package *package, SourceRange pos, Expr *type);
Expr *NewExprTypeStruct(Package *package, SourceRange pos, DynamicArray(AggregateItem) items);
Expr *NewExprTypeEnum(Package *package, SourceRange pos, Expr *explicitType, DynamicArray(EnumItem) items);
Expr *NewExprTypeUnion(Package *package, SourceRange pos, DynamicArray(AggregateItem) items);
Expr *NewExprTypePolymorphic(Package *package, SourceRange pos, const char *name);
Expr *NewExprTypeVariadic(Package *package, SourceRange pos, Expr *type, u8 flags);
Expr *NewExprTypeFunction(Package *package, SourceRange pos, DynamicArray(KeyValue) params, DynamicArray(Expr *)result);

// - MARK: Stmts
Stmt *NewStmtEmpty(Package *package, SourceRange pos);
Stmt *NewStmtLabel(Package *package, SourceRange pos, const char *name);
Stmt *NewStmtAssign(Package *package, SourceRange pos, DynamicArray(Expr *) lhs, DynamicArray(Expr*) rhs);
Stmt *NewStmtReturn(Package *package, SourceRange pos, DynamicArray(Expr *) exprs);
Stmt *NewStmtDefer(Package *package, SourceRange pos, Stmt *stmt);
Stmt *NewStmtUsing(Package *package, SourceRange pos, Expr *expr);
Stmt *NewStmtGoto(Package *package, SourceRange pos, const char *keyword, Expr *target);
Stmt *NewStmtBlock(Package *package, SourceRange pos, DynamicArray(Stmt *) stmts);
Stmt *NewStmtIf(Package *package, SourceRange pos, Expr *cond, Stmt *pass, Stmt *fail);
Stmt *NewStmtFor(Package *package, SourceRange pos, Stmt *init, Expr *cond, Stmt *step, Stmt *body);
Stmt *NewStmtForIn(Package *package, SourceRange pos, Expr *valueName, Expr *indexName, Expr *aggregate, Stmt *body);
Stmt *NewStmtSwitch(Package *package, SourceRange pos, Expr *match, DynamicArray(Stmt *) cases);
Stmt *NewStmtSwitchCase(Package *package, SourceRange pos, DynamicArray(Expr *) matches, Stmt *block);

// - MARK: Decls
Decl *NewDeclVariable(Package *package, SourceRange pos, DynamicArray(Expr *) names, Expr *type, DynamicArray(Expr *) values);
Decl *NewDeclConstant(Package *package, SourceRange pos, DynamicArray(Expr *) names, Expr *type, DynamicArray(Expr *) values);
Decl *NewDeclForeign(Package *package, SourceRange pos, Expr *library, bool isConstant, const char *name, Expr *type, const char *linkname, const char *callingConvention);
Decl *NewDeclForeignBlock(Package *package, SourceRange pos, Expr *library, const char *callingConvention, DynamicArray(Decl_ForeignBlockMember) members);
Decl *NewDeclImport(Package *package, SourceRange pos, Expr *path, const char *alias);
