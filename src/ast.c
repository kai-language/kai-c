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

// TODO: We can implement these correctly to improve error quality at some point
// Alternatively we *may* be able to come up with something smarter
Position EndForStmt(Stmt *stmt) {
    return stmt->start;
}

Position EndForExpr(Expr *expr) {
    return expr->start;
}

Position EndForDecl(Decl *decl) {
    return decl->start;
}

Expr *NewExpr(Package *package, ExprKind kind, Position start) {
    Expr *e = AllocAst(package, sizeof(Expr));
    e->kind = kind;
    e->start = start;
    e->id = DoesStmtKindAllocateTypeInfo[kind] ? ++package->astIdCount: 0;
    return e;
}

Stmt *NewStmt(Package *package, StmtKind kind, Position start) {
    Stmt *s = AllocAst(package, sizeof(Stmt));
    s->kind = kind;
    s->start = start;
    s->id = DoesStmtKindAllocateTypeInfo[kind] ? ++package->astIdCount: 0;
    return s;
}

Decl *NewDecl(Package *package, DeclKind kind, Position start) {
    Decl *d = AllocAst(package, sizeof(Decl));
    d->kind = kind;
    d->start = start;
    d->id = DoesStmtKindAllocateTypeInfo[kind] ? ++package->astIdCount: 0;

#if DEBUG
    d->owningPackage = package;
#endif
    return d;
}

Expr *NewExprInvalid(Package *package, Position start) {
    Expr *e = NewExpr(package, ExprKind_Invalid, start);
    return e;
}

Stmt *NewStmtInvalid(Package *package, Position start) {
    Stmt *s = NewStmt(package, StmtKind_Invalid, start);
    return s;
}

Decl *NewDeclInvalid(Package *package, Position start) {
    Decl *d = NewDecl(package, DeclKind_Invalid, start);
    return d;
}

Expr *NewExprIdent(Package *package, Position start, const char *name) {
    Expr *e = NewExpr(package, ExprKind_Ident, start);
    e->Ident.name = name;
    return e;
}

Expr *NewExprParen(Package *package, Expr *expr, Position start) {
    Expr *e = NewExpr(package, ExprKind_Paren, start);
    e->Paren.expr = expr;
    return e;
}

Expr *NewExprCall(Package *package, Expr *expr, DynamicArray(Expr_KeyValue *) args) {
    Expr *e = NewExpr(package, ExprKind_Call, expr->start);
    e->Call.expr = expr;
    e->Call.args = args;
    return e;
}

Expr *NewExprSelector(Package *package, Expr *expr, const char *name) {
    Expr *e = NewExpr(package, ExprKind_Selector, expr->start);
    e->Selector.expr = expr;
    e->Selector.name = name;
    return e;
}

Expr *NewExprSubscript(Package *package, Expr *expr, Expr *index) {
    Expr *e = NewExpr(package, ExprKind_Subscript, expr->start);
    e->Subscript.expr = expr;
    e->Subscript.index = index;
    return e;
}

Expr *NewExprSlice(Package *package, Expr *expr, Expr *lo, Expr *hi) {
    Expr *e = NewExpr(package, ExprKind_Slice, expr->start);
    e->Slice.expr = expr;
    e->Slice.lo = lo;
    e->Slice.hi = hi;
    return e;
}

Expr *NewExprUnary(Package *package, Position start, TokenKind op, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Unary, start);
    e->Unary.op = op;
    e->Unary.expr = expr;
    return e;
}

Expr *NewExprBinary(Package *package, Token op, Expr *lhs, Expr *rhs) {
    Expr *e = NewExpr(package, ExprKind_Binary, lhs->start);
    e->Binary.op = op;
    e->Binary.lhs = lhs;
    e->Binary.rhs = rhs;
    return e;
}

Expr *NewExprTernary(Package *package, Expr *cond, Expr *pass, Expr *fail) {
    Expr *e = NewExpr(package, ExprKind_Ternary, cond->start);
    e->Ternary.cond = cond;
    e->Ternary.pass = pass;
    e->Ternary.fail = fail;
    return e;
}

Expr *NewExprCast(Package *package, Position start, Expr *type, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Cast, start);
    e->Cast.type = type;
    e->Cast.expr = expr;
    return e;
}

Expr *NewExprAutocast(Package *package, Position start, Expr *expr) {
    Expr *e = NewExpr(package, ExprKind_Autocast, start);
    e->Autocast.expr = expr;
    return e;
}

Expr *NewExprKeyValue(Package *package, Expr *key, Expr *value) {
    Expr *e = NewExpr(package, ExprKind_KeyValue, key->start);
    e->KeyValue.key = key;
    e->KeyValue.value = value;
    return e;
}

Expr *NewExprLocationDirective(Package *package, Position start, const char *name) {
    Expr *e = NewExpr(package, ExprKind_LocationDirective, start);
    e->LocationDirective.name = name;
    return e;
}

Expr *NewExprLitNil(Package *package, Position start) {
    Expr *e = NewExpr(package, ExprKind_LitNil, start);
    return e;
}

Expr *NewExprLitInt(Package *package, Position start, u64 val) {
    Expr *e = NewExpr(package, ExprKind_LitInt, start);
    e->LitInt.val = val;
    return e;
}

Expr *NewExprLitFloat(Package *package, Position start, f64 val) {
    Expr *e = NewExpr(package, ExprKind_LitFloat, start);
    e->LitFloat.val = val;
    return e;
}

Expr *NewExprLitString(Package *package, Position start, const char *val) {
    Expr *e = NewExpr(package, ExprKind_LitString, start);
    e->LitString.val = val;
    return e;
}

Expr *NewExprLitCompound(Package *package, Position start, Expr *type, DynamicArray(Expr_KeyValue *) elements) {
    Expr *e = NewExpr(package, ExprKind_LitCompound, start);
    e->LitCompound.type = type;
    e->LitCompound.elements = elements;
    return e;
}

Expr *NewExprLitFunction(Package *package, Expr *type, Stmt_Block *body, u8 flags) {
    Expr *e = NewExpr(package, ExprKind_LitFunction, type->start);
    e->LitFunction.type = type;
    e->LitFunction.body = body;
    e->LitFunction.flags = flags;
    return e;
}

Expr *NewExprTypePointer(Package *package, Position start, Expr *type) {
    Expr *e = NewExpr(package, ExprKind_TypePointer, start);
    e->TypePointer.type = type;
    return e;
}

Expr *NewExprTypeArray(Package *package, Position start, Expr *length, Expr *type) {
    Expr *e = NewExpr(package, ExprKind_TypeArray, start);
    e->TypeArray.length = length;
    e->TypeArray.type = type;
    return e;
}

Expr *NewExprTypeSlice(Package *package, Position start, Expr *type) {
    Expr *e = NewExpr(package, ExprKind_TypeSlice, start);
    e->TypeSlice.type = type;
    return e;
}

Expr *NewExprTypeStruct(Package *package, Position start, DynamicArray(AggregateItem) items) {
    Expr *e = NewExpr(package, ExprKind_TypeStruct, start);
    e->TypeStruct.items = items;
    return e;
}

Expr *NewExprTypeEnum(Package *package, Position start, Expr *explicitType, DynamicArray(EnumItem) items) {
    Expr *e = NewExpr(package, ExprKind_TypeEnum, start);
    e->TypeEnum.explicitType = explicitType;
    e->TypeEnum.items = items;
    return e;
}

Expr *NewExprTypeUnion(Package *package, Position start, DynamicArray(AggregateItem) items) {
    Expr *e = NewExpr(package, ExprKind_TypeUnion, start);
    e->TypeUnion.items = items;
    return e;
}

Expr *NewExprTypePolymorphic(Package *package, Position start, const char *name) {
    Expr *e = NewExpr(package, ExprKind_TypePolymorphic, start);
    e->TypePolymorphic.name = name;
    return e;
}

Expr *NewExprTypeVariadic(Package *package, Position start, Expr *type, u8 flags) {
    Expr *e = NewExpr(package, ExprKind_TypeVariadic, start);
    e->TypeVariadic.type = type;
    e->TypeVariadic.flags = flags;
    return e;
}

Expr *NewExprTypeFunction(Package *package, Position start, DynamicArray(Expr_KeyValue *) params, DynamicArray(Expr *)result) {
    Expr *e = NewExpr(package, ExprKind_TypeFunction, start);
    e->TypeFunction.params = params;
    e->TypeFunction.result = result;
    return e;
}

Stmt *NewStmtEmpty(Package *package, Position start) {
    return NewStmt(package, StmtKind_Empty, start);
}

Stmt *NewStmtLabel(Package *package, Position start, const char *name) {
    Stmt *s = NewStmt(package, StmtKind_Label, start);
    s->Label.name = name;
    return s;
}

Stmt *NewStmtAssign(Package *package, Position start, DynamicArray(Expr *) lhs, DynamicArray(Expr*) rhs) {
    Stmt *s = NewStmt(package, StmtKind_Assign, start);
    s->Assign.lhs = lhs;
    s->Assign.rhs = rhs;
    return s;
}

Stmt *NewStmtReturn(Package *package, Position start, DynamicArray(Expr *) exprs) {
    Stmt *s = NewStmt(package, StmtKind_Return, start);
    s->Return.exprs = exprs;
    return s;
}

Stmt *NewStmtDefer(Package *package, Position start, Stmt *stmt) {
    Stmt *s = NewStmt(package, StmtKind_Defer, start);
    s->Defer.stmt = stmt;
    return s;
}

Stmt *NewStmtUsing(Package *package, Position start, Expr *expr) {
    Stmt *s = NewStmt(package, StmtKind_Using, start);
    s->Using.expr = expr;
    return s;
}

Stmt *NewStmtGoto(Package *package, Position start, const char *keyword, Expr *target) {
    Stmt *s = NewStmt(package, StmtKind_Goto, start);
    s->Goto.keyword = keyword;
    s->Goto.target = target;
    return s;
}

Stmt *NewStmtBlock(Package *package, Position start, DynamicArray(Stmt *) stmts) {
    Stmt *s = NewStmt(package, StmtKind_Block, start);
    s->Block.stmts = stmts;
    return s;
}

Stmt *NewStmtIf(Package *package, Position start, Expr *cond, Stmt *pass, Stmt *fail) {
    Stmt *s = NewStmt(package, StmtKind_If, start);
    s->If.cond = cond;
    s->If.pass = pass;
    s->If.fail = fail;
    return s;
}

Stmt *NewStmtFor(Package *package, Position start, Stmt *init, Expr *cond, Stmt *step, Stmt_Block *body) {
    Stmt *s = NewStmt(package, StmtKind_For, start);
    s->For.init = init;
    s->For.cond = cond;
    s->For.step = step;
    s->For.body = body;
    return s;
}

Stmt *NewStmtForIn(Package *package, Position start, Expr_Ident *valueName, Expr_Ident *indexName, Expr *aggregate, Stmt_Block *body) {
    Stmt *s = NewStmt(package, StmtKind_ForIn, start);
    s->ForIn.valueName = valueName;
    s->ForIn.indexName = indexName;
    s->ForIn.aggregate = aggregate;
    s->ForIn.body = body;
    return s;
}

Stmt *NewStmtSwitch(Package *package, Position start, Expr *match, DynamicArray(Stmt *) cases) {
    Stmt *s = NewStmt(package, StmtKind_Switch, start);
    s->Switch.match = match;
    s->Switch.cases = cases;
    return s;
}

Stmt *NewStmtSwitchCase(Package *package, Position start, DynamicArray(Expr *) matches, Stmt_Block *block) {
    Stmt *s = NewStmt(package, StmtKind_SwitchCase, start);
    s->SwitchCase.start = start;
    s->SwitchCase.matches = matches;
    s->SwitchCase.block = block;
    return s;
}

Decl *NewDeclVariable(Package *package, Position start, DynamicArray(Expr_Ident *) names, Expr *type, DynamicArray(Expr *) values) {
    Decl *d = NewDecl(package, DeclKind_Variable, start);
    d->Variable.names = names;
    d->Variable.type = type;
    d->Variable.values = values;
    return d;
}

Decl *NewDeclConstant(Package *package, Position start, DynamicArray(Expr_Ident *) names, Expr *type, DynamicArray(Expr *) values) {
    Decl *d = NewDecl(package, DeclKind_Constant, start);
    d->Constant.names = names;
    d->Constant.type = type;
    d->Constant.values = values;
    return d;
}

Decl *NewDeclForeign(Package *package, Position start, Expr *library, bool isConstant, const char *name, Expr *type, const char *linkname, const char *callingConvention) {
    Decl *d = NewDecl(package, DeclKind_Foreign, start);
    d->Foreign.library = library;
    d->Foreign.isConstant = isConstant;
    d->Foreign.name = name;
    d->Foreign.type = type;
    d->Foreign.linkname = linkname;
    d->Foreign.callingConvention = callingConvention;
    return d;
}

Decl *NewDeclForeignBlock(Package *package, Position start, Expr *library, const char *callingConvention, DynamicArray(Decl_ForeignBlockMember) members) {
    Decl *d = NewDecl(package, DeclKind_ForeignBlock, start);
    d->ForeignBlock.library = library;
    d->ForeignBlock.members = members;
    d->ForeignBlock.callingConvention = callingConvention;
    return d;
}

Decl *NewDeclImport(Package *package, Position start, Expr *path, const char *alias) {
    Decl *d = NewDecl(package, DeclKind_Import, start);
    d->Import.path = path;
    d->Import.alias = alias;
    return d;
}

#if TEST
void test_isExpr_and_isDecl() {
    Package pkg = {0};
    Position pos = {0};
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
