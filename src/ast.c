#include "ast.h"

i8 stmtDeclaresSymbol[_StmtDeclKind_End] = {
    [StmtDeclKind_Constant] = 1,
    [StmtDeclKind_Variable] = 1,
    [StmtDeclKind_Foreign] = 1,
    [StmtDeclKind_ForeignBlock] = 1,
};

const char *AstDescriptions[] = {
#define FOR_EACH(kindName, s, ...) "" s "",
    [EXPR_KIND_START] = "invalid",
    EXPR_KINDS
        "invalid",
    [STMT_KIND_START] = "invalid",
    STMT_KINDS
        "invalid",
    [DECL_KIND_START] = "invalid",
    DECL_KINDS
        "invalid",
#undef FOR_EACH
};

b8 DoesStmtKindAllocateTypeInfo[] = {
    [StmtKind_Invalid] =  0,
    
    [_StmtKind_Start] = 0,
#define FOR_EACH(kindName, __ignored__, doesAllocate) [StmtKind_##kindName] = doesAllocate,
    STMT_KINDS
#undef FOR_EACH
        [_StmtKind_End] = 0,
    
    [_StmtExprKind_Start] = 0,
#define FOR_EACH(kindName, __ignored__, doesAllocate) [StmtExprKind_##kindName] = doesAllocate,
    EXPR_KINDS
#undef FOR_EACH
        [_StmtExprKind_End] = 0,
    
    [_StmtDeclKind_Start] = 0,
#define FOR_EACH(kindName, __ignored__, doesAllocate) [StmtDeclKind_##kindName] = doesAllocate,
    DECL_KINDS
#undef FOR_EACH
        [_StmtDeclKind_End] = 0
};


b32 isExpr(Stmt *stmt) {
    return stmt->kind > _ExprKind_Start && stmt->kind < _ExprKind_End;
}

b32 isDecl(Stmt *stmt) {
    return stmt->kind > _DeclKind_Start && stmt->kind < _DeclKind_End;
}

// @ErrorQuality
// FIXME: Better description
const char *DescribeStmt(Stmt *stmt) {
    return AstDescriptions[stmt->kind];
}

const char *DescribeExpr(Expr *expr) {
    return AstDescriptions[expr->kind];
}

const char *DescribeDecl(Decl *decl) {
    return AstDescriptions[decl->kind];
}

void *AllocAst(Package *package, size_t size) {
    ASSERT(size != 0);
    void *mem = ArenaCalloc(&package->arena, size);
    return mem;
}

Expr *NewExpr(Package *package, ExprKind kind, SourceRange pos) {
    Expr *e = AllocAst(package, sizeof(Expr));
    e->kind = kind;
    e->pos = pos;
    e->id = DoesStmtKindAllocateTypeInfo[kind] ? ++package->astIdCount: 0;
    return e;
}

Stmt *NewStmt(Package *package, StmtKind kind, SourceRange pos) {
    Stmt *s = AllocAst(package, sizeof(Stmt));
    s->kind = kind;
    s->pos = pos;
    s->id = DoesStmtKindAllocateTypeInfo[kind] ? ++package->astIdCount: 0;
    return s;
}

Decl *NewDecl(Package *package, DeclKind kind, SourceRange pos) {
    Decl *d = AllocAst(package, sizeof(Decl));
    d->kind = kind;
    d->pos = pos;
    d->id = DoesStmtKindAllocateTypeInfo[kind] ? ++package->astIdCount: 0;

#if DEBUG
    d->owningPackage = package;
#endif
    return d;
}

Expr *NewExprInvalid(Package *package, SourceRange pos) {
    Expr *e = NewExpr(package, ExprKind_Invalid, pos);
    return e;
}

Stmt *NewStmtInvalid(Package *package, SourceRange pos) {
    Stmt *s = NewStmt(package, StmtKind_Invalid, pos);
    return s;
}

Decl *NewDeclInvalid(Package *package, SourceRange pos) {
    Decl *d = NewDecl(package, DeclKind_Invalid, pos);
    return d;
}

Expr *NewExprIdent(Package *package, SourceRange pos, const char *name) {
    Expr *e = NewExpr(package, ExprKind_Ident, pos);
    e->Ident.name = name;
    return e;
}

Expr *NewExprParen(Package *package, SourceRange pos, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Paren, pos);
    e->Paren.expr = expr;
    return e;
}

Expr *NewExprCall(Package *package, SourceRange pos, Expr *expr, DynamicArray(Expr_KeyValue *) args) {
    Expr *e = NewExpr(package, ExprKind_Call, pos);
    e->Call.expr = expr;
    e->Call.args = args;
    return e;
}

Expr *NewExprSelector(Package *package, SourceRange pos, Expr *expr, const char *name) {
    Expr *e = NewExpr(package, ExprKind_Selector, pos);
    e->Selector.expr = expr;
    e->Selector.name = name;
    return e;
}

Expr *NewExprSubscript(Package *package, SourceRange pos, Expr *expr, Expr *index) {
    Expr *e = NewExpr(package, ExprKind_Subscript, pos);
    e->Subscript.expr = expr;
    e->Subscript.index = index;
    return e;
}

Expr *NewExprSlice(Package *package, SourceRange pos, Expr *expr, Expr *lo, Expr *hi) {
    Expr *e = NewExpr(package, ExprKind_Slice, pos);
    e->Slice.expr = expr;
    e->Slice.lo = lo;
    e->Slice.hi = hi;
    return e;
}

Expr *NewExprUnary(Package *package, SourceRange pos, TokenKind op, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Unary, pos);
    e->Unary.op = op;
    e->Unary.expr = expr;
    return e;
}

Expr *NewExprBinary(Package *package, SourceRange pos, Token op, Expr *lhs, Expr *rhs) {
    Expr *e = NewExpr(package, ExprKind_Binary, pos);
    e->Binary.op = op;
    e->Binary.lhs = lhs;
    e->Binary.rhs = rhs;
    return e;
}

Expr *NewExprTernary(Package *package, SourceRange pos, Expr *cond, Expr *pass, Expr *fail) {
    Expr *e = NewExpr(package, ExprKind_Ternary, pos);
    e->Ternary.cond = cond;
    e->Ternary.pass = pass;
    e->Ternary.fail = fail;
    return e;
}

Expr *NewExprCast(Package *package, SourceRange pos, Expr *type, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Cast, pos);
    e->Cast.type = type;
    e->Cast.expr = expr;
    return e;
}

Expr *NewExprAutocast(Package *package, SourceRange pos, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Autocast, pos);
    e->Autocast.expr = expr;
    return e;
}

Expr *NewExprKeyValue(Package *package, Expr *key, Expr *value) {
    Expr *e = NewExpr(package, ExprKind_KeyValue, key->pos);
    e->KeyValue.key = key;
    e->KeyValue.value = value;
    return e;
}

Expr *NewExprLocationDirective(Package *package, SourceRange pos, const char *name) {
    Expr *e = NewExpr(package, ExprKind_LocationDirective, pos);
    e->LocationDirective.name = name;
    return e;
}

Expr *NewExprLitNil(Package *package, SourceRange pos) {
    Expr *e = NewExpr(package, ExprKind_LitNil, pos);
    return e;
}

Expr *NewExprLitInt(Package *package, SourceRange pos, u64 val) {
    Expr *e = NewExpr(package, ExprKind_LitInt, pos);
    e->LitInt.val = val;
    return e;
}

Expr *NewExprLitFloat(Package *package, SourceRange pos, f64 val) {
    Expr *e = NewExpr(package, ExprKind_LitFloat, pos);
    e->LitFloat.val = val;
    return e;
}

Expr *NewExprLitString(Package *package, SourceRange pos, const char *val) {
    Expr *e = NewExpr(package, ExprKind_LitString, pos);
    e->LitString.val = val;
    return e;
}

Expr *NewExprLitCompound(Package *package, SourceRange pos, Expr *type, DynamicArray(Expr_KeyValue *) elements) {
    Expr *e = NewExpr(package, ExprKind_LitCompound, pos);
    e->LitCompound.type = type;
    e->LitCompound.elements = elements;
    return e;
}

Expr *NewExprLitFunction(Package *package, SourceRange pos, Expr *type, Stmt_Block *body, u8 flags) {
    Expr *e = NewExpr(package, ExprKind_LitFunction, pos);
    e->LitFunction.type = type;
    e->LitFunction.body = body;
    e->LitFunction.flags = flags;
    return e;
}

Expr *NewExprTypePointer(Package *package, SourceRange pos, Expr *type) {
    Expr *e = NewExpr(package, ExprKind_TypePointer, pos);
    e->TypePointer.type = type;
    return e;
}

Expr *NewExprTypeArray(Package *package, SourceRange pos, Expr *length, Expr *type) {
    Expr *e = NewExpr(package, ExprKind_TypeArray, pos);
    e->TypeArray.length = length;
    e->TypeArray.type = type;
    return e;
}

Expr *NewExprTypeSlice(Package *package, SourceRange pos, Expr *type) {
    Expr *e = NewExpr(package, ExprKind_TypeSlice, pos);
    e->TypeSlice.type = type;
    return e;
}

Expr *NewExprTypeStruct(Package *package, SourceRange pos, DynamicArray(AggregateItem) items) {
    Expr *e = NewExpr(package, ExprKind_TypeStruct, pos);
    e->TypeStruct.items = items;
    return e;
}

Expr *NewExprTypeEnum(Package *package, SourceRange pos, Expr *explicitType, DynamicArray(EnumItem) items) {
    Expr *e = NewExpr(package, ExprKind_TypeEnum, pos);
    e->TypeEnum.explicitType = explicitType;
    e->TypeEnum.items = items;
    return e;
}

Expr *NewExprTypeUnion(Package *package, SourceRange pos, DynamicArray(AggregateItem) items) {
    Expr *e = NewExpr(package, ExprKind_TypeUnion, pos);
    e->TypeUnion.items = items;
    return e;
}

Expr *NewExprTypePolymorphic(Package *package, SourceRange pos, const char *name) {
    Expr *e = NewExpr(package, ExprKind_TypePolymorphic, pos);
    e->TypePolymorphic.name = name;
    return e;
}

Expr *NewExprTypeVariadic(Package *package, SourceRange pos, Expr *type, u8 flags) {
    Expr *e = NewExpr(package, ExprKind_TypeVariadic, pos);
    e->TypeVariadic.type = type;
    e->TypeVariadic.flags = flags;
    return e;
}

Expr *NewExprTypeFunction(Package *package, SourceRange pos, DynamicArray(Expr_KeyValue *) params, DynamicArray(Expr *)result) {
    Expr *e = NewExpr(package, ExprKind_TypeFunction, pos);
    e->TypeFunction.params = params;
    e->TypeFunction.result = result;
    return e;
}

Stmt *NewStmtEmpty(Package *package, SourceRange pos) {
    return NewStmt(package, StmtKind_Empty, pos);
}

Stmt *NewStmtLabel(Package *package, SourceRange pos, const char *name) {
    Stmt *s = NewStmt(package, StmtKind_Label, pos);
    s->Label.name = name;
    return s;
}

Stmt *NewStmtAssign(Package *package, SourceRange pos, DynamicArray(Expr *) lhs, DynamicArray(Expr*) rhs) {
    Stmt *s = NewStmt(package, StmtKind_Assign, pos);
    s->Assign.lhs = lhs;
    s->Assign.rhs = rhs;
    return s;
}

Stmt *NewStmtReturn(Package *package, SourceRange pos, DynamicArray(Expr *) exprs) {
    Stmt *s = NewStmt(package, StmtKind_Return, pos);
    s->Return.exprs = exprs;
    return s;
}

Stmt *NewStmtDefer(Package *package, SourceRange pos, Stmt *stmt) {
    Stmt *s = NewStmt(package, StmtKind_Defer, pos);
    s->Defer.stmt = stmt;
    return s;
}

Stmt *NewStmtUsing(Package *package, SourceRange pos, Expr *expr) {
    Stmt *s = NewStmt(package, StmtKind_Using, pos);
    s->Using.expr = expr;
    return s;
}

Stmt *NewStmtGoto(Package *package, SourceRange pos, const char *keyword, Expr *target) {
    Stmt *s = NewStmt(package, StmtKind_Goto, pos);
    s->Goto.keyword = keyword;
    s->Goto.target = target;
    return s;
}

Stmt *NewStmtBlock(Package *package, SourceRange pos, DynamicArray(Stmt *) stmts) {
    Stmt *s = NewStmt(package, StmtKind_Block, pos);
    s->Block.stmts = stmts;
    return s;
}

Stmt *NewStmtIf(Package *package, SourceRange pos, Expr *cond, Stmt *pass, Stmt *fail) {
    Stmt *s = NewStmt(package, StmtKind_If, pos);
    s->If.cond = cond;
    s->If.pass = pass;
    s->If.fail = fail;
    return s;
}

Stmt *NewStmtFor(Package *package, SourceRange pos, Stmt *init, Expr *cond, Stmt *step, Stmt_Block *body) {
    Stmt *s = NewStmt(package, StmtKind_For, pos);
    s->For.init = init;
    s->For.cond = cond;
    s->For.step = step;
    s->For.body = body;
    return s;
}

Stmt *NewStmtForIn(Package *package, SourceRange pos, Expr_Ident *valueName, Expr_Ident *indexName, Expr *aggregate, Stmt_Block *body) {
    Stmt *s = NewStmt(package, StmtKind_ForIn, pos);
    s->ForIn.valueName = valueName;
    s->ForIn.indexName = indexName;
    s->ForIn.aggregate = aggregate;
    s->ForIn.body = body;
    return s;
}

Stmt *NewStmtSwitch(Package *package, SourceRange pos, Expr *match, DynamicArray(Stmt *) cases) {
    Stmt *s = NewStmt(package, StmtKind_Switch, pos);
    s->Switch.match = match;
    s->Switch.cases = cases;
    return s;
}

Stmt *NewStmtSwitchCase(Package *package, SourceRange pos, DynamicArray(Expr *) matches, Stmt_Block *block) {
    Stmt *s = NewStmt(package, StmtKind_SwitchCase, pos);
    s->SwitchCase.matches = matches;
    s->SwitchCase.block = block;
    return s;
}

Decl *NewDeclVariable(Package *package, SourceRange pos, DynamicArray(Expr_Ident *) names, Expr *type, DynamicArray(Expr *) values) {
    Decl *d = NewDecl(package, DeclKind_Variable, pos);
    d->Variable.names = names;
    d->Variable.type = type;
    d->Variable.values = values;
    return d;
}

Decl *NewDeclConstant(Package *package, SourceRange pos, DynamicArray(Expr_Ident *) names, Expr *type, DynamicArray(Expr *) values) {
    Decl *d = NewDecl(package, DeclKind_Constant, pos);
    d->Constant.names = names;
    d->Constant.type = type;
    d->Constant.values = values;
    return d;
}

Decl *NewDeclForeign(Package *package, SourceRange pos, Expr *library, bool isConstant, const char *name, Expr *type, const char *linkname, const char *callingConvention) {
    Decl *d = NewDecl(package, DeclKind_Foreign, pos);
    d->Foreign.library = library;
    d->Foreign.isConstant = isConstant;
    d->Foreign.name = name;
    d->Foreign.type = type;
    d->Foreign.linkname = linkname;
    d->Foreign.callingConvention = callingConvention;
    return d;
}

Decl *NewDeclForeignBlock(Package *package, SourceRange pos, Expr *library, const char *callingConvention, DynamicArray(Decl_ForeignBlockMember) members) {
    Decl *d = NewDecl(package, DeclKind_ForeignBlock, pos);
    d->ForeignBlock.library = library;
    d->ForeignBlock.members = members;
    d->ForeignBlock.callingConvention = callingConvention;
    return d;
}

Decl *NewDeclImport(Package *package, SourceRange pos, Expr *path, const char *alias) {
    Decl *d = NewDecl(package, DeclKind_Import, pos);
    d->Import.path = path;
    d->Import.alias = alias;
    return d;
}

#if TEST
void test_isExpr_and_isDecl() {
    Package pkg = {0};
    SourceRange pos = {0};
    Stmt *expr = (Stmt *) NewExprIdent(&pkg, pos, NULL);
    ASSERT(isExpr(expr));
    
    Stmt *decl = (Stmt *) NewDeclVariable(&pkg, pos, NULL, NULL, NULL);
    ASSERT(isDecl(decl));
}

void test_doesExprAllocate() {
    ASSERT(DoesStmtKindAllocateTypeInfo[StmtExprKind_Ident]);
    ASSERT(DoesStmtKindAllocateTypeInfo[StmtExprKind_Selector]);
    ASSERT(DoesStmtKindAllocateTypeInfo[StmtKind_Defer] == false);
}
#endif
