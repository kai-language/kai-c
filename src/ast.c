#include "ast.h"

const char *AstDescriptions[] = {
    "invalid",
    [ExprKindIdent]             = "identifier",
    [ExprKindParen]             = "parenthesis",
    [ExprKindCall]              = "call",
    [ExprKindCast]              = "cast",
    [ExprKindSelector]          = "selector",
    [ExprKindSubscript]         = "subscript",
    [ExprKindSlice]             = "slice",
    [ExprKindUnary]             = "unary",
    [ExprKindBinary]            = "binary",
    [ExprKindTernary]           = "ternary",
    [ExprKindAutocast]          = "autocast",
    [ExprKindLocationDirective] = "location directive",
    [ExprKindLitNil]            = "nil literal",
    [ExprKindLitInt]            = "integer literal",
    [ExprKindLitFloat]          = "float literal",
    [ExprKindLitString]         = "string literal",
    [ExprKindLitCompound]       = "compound literal",
    [ExprKindLitFunction]       = "function literal",
    [ExprKindTypePointer]       = "pointer type",
    [ExprKindTypeArray]         = "array type",
    [ExprKindTypeSlice]         = "slice type",
    [ExprKindTypeStruct]        = "struct type",
    [ExprKindTypeEnum]          = "enum type",
    [ExprKindTypeUnion]         = "union type",
    [ExprKindTypePolymorphic]   = "polymorphic type",
    [ExprKindTypeVariadic]      = "variadic type",
    [ExprKindTypeFunction]      = "function type",
    "invalid",
    [StmtKindEmpty]      = "empty",
    [StmtKindLabel]      = "label",
    [StmtKindAssign]     = "assignment",
    [StmtKindReturn]     = "return",
    [StmtKindDefer]      = "defer",
    [StmtKindUsing]      = "using",
    [StmtKindGoto]       = "goto",
    [StmtKindBlock]      = "block",
    [StmtKindIf]         = "if",
    [StmtKindFor]        = "for",
    [StmtKindForIn]      = "for in",
    [StmtKindSwitch]     = "switch",
    [StmtKindSwitchCase] = "case",
    "invalid",
    [DeclKindVariable]     = "variable",
    [DeclKindConstant]     = "constant",
    [DeclKindForeign]      = "foreign",
    [DeclKindForeignBlock] = "foreign block",
    [DeclKindImport]       = "import",
    "invalid",
};

b8 DoesStmtKindAllocateTypeInfo[] = {
    [ExprKindIdent]             = true,
    [ExprKindParen]             = false,
    [ExprKindCall]              = true,
    [ExprKindCast]              = true,
    [ExprKindSelector]          = true,
    [ExprKindSubscript]         = true,
    [ExprKindSlice]             = true,
    [ExprKindUnary]             = true,
    [ExprKindBinary]            = true,
    [ExprKindTernary]           = true,
    [ExprKindAutocast]          = false,
    [ExprKindLocationDirective] = true,
    [ExprKindLitNil]            = true,
    [ExprKindLitInt]            = true,
    [ExprKindLitFloat]          = true,
    [ExprKindLitString]         = true,
    [ExprKindLitCompound]       = true,
    [ExprKindLitFunction]       = true,
    [ExprKindTypePointer]       = true,
    [ExprKindTypeArray]         = true,
    [ExprKindTypeSlice]         = true,
    [ExprKindTypeStruct]        = true,
    [ExprKindTypeEnum]          = true,
    [ExprKindTypeUnion]         = true,
    [ExprKindTypePolymorphic]   = true,
    [ExprKindTypeVariadic]      = true,
    [ExprKindTypeFunction]      = true,

    [StmtKindEmpty]      = false,
    [StmtKindLabel]      = true,
    [StmtKindAssign]     = false,
    [StmtKindReturn]     = false,
    [StmtKindDefer]      = false,
    [StmtKindUsing]      = false,
    [StmtKindGoto]       = true,
    [StmtKindBlock]      = false,
    [StmtKindIf]         = false,
    [StmtKindFor]        = true,
    [StmtKindForIn]      = true,
    [StmtKindSwitch]     = true,
    [StmtKindSwitchCase] = true,

    [DeclKindVariable]     = true,
    [DeclKindConstant]     = true,
    [DeclKindForeign]      = true,
    [DeclKindForeignBlock] = false,
    [DeclKindImport]       = true,
};

b32 isExpr(Stmt *stmt) {
    return (stmt->kind & AST_KIND_MASK) == EXPR_KIND_PREFIX;
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
    Expr *e = NewExpr(package, ExprKindInvalid, pos);
    return e;
}

Stmt *NewStmtInvalid(Package *package, SourceRange pos) {
    Stmt *s = NewStmt(package, StmtKindInvalid, pos);
    return s;
}

Decl *NewDeclInvalid(Package *package, SourceRange pos) {
    Decl *d = NewDecl(package, DeclKindInvalid, pos);
    return d;
}

Expr *NewExprIdent(Package *package, SourceRange pos, const char *name) {
    Expr *e = NewExpr(package, ExprKindIdent, pos);
    e->Ident.name = name;
    return e;
}

Expr *NewExprParen(Package *package, SourceRange pos, Expr *expr) {
    Expr *e = NewExpr(package, ExprKindParen, pos);
    e->Paren.expr = expr;
    return e;
}

Expr *NewExprCall(Package *package, SourceRange pos, Expr *expr, DynamicArray(KeyValue) args) {
    Expr *e = NewExpr(package, ExprKindCall, pos);
    e->Call.expr = expr;
    e->Call.args = args;
    return e;
}

Expr *NewExprSelector(Package *package, SourceRange pos, Expr *expr, const char *name) {
    Expr *e = NewExpr(package, ExprKindSelector, pos);
    e->Selector.expr = expr;
    e->Selector.name = name;
    return e;
}

Expr *NewExprSubscript(Package *package, SourceRange pos, Expr *expr, Expr *index) {
    Expr *e = NewExpr(package, ExprKindSubscript, pos);
    e->Subscript.expr = expr;
    e->Subscript.index = index;
    return e;
}

Expr *NewExprSlice(Package *package, SourceRange pos, Expr *expr, Expr *lo, Expr *hi) {
    Expr *e = NewExpr(package, ExprKindSlice, pos);
    e->Slice.expr = expr;
    e->Slice.lo = lo;
    e->Slice.hi = hi;
    return e;
}

Expr *NewExprUnary(Package *package, SourceRange pos, TokenKind op, Expr *expr) {
    Expr *e = NewExpr(package, ExprKindUnary, pos);
    e->Unary.op = op;
    e->Unary.expr = expr;
    return e;
}

Expr *NewExprBinary(Package *package, SourceRange pos, Token op, Expr *lhs, Expr *rhs) {
    Expr *e = NewExpr(package, ExprKindBinary, pos);
    e->Binary.op = op;
    e->Binary.lhs = lhs;
    e->Binary.rhs = rhs;
    return e;
}

Expr *NewExprTernary(Package *package, SourceRange pos, Expr *cond, Expr *pass, Expr *fail) {
    Expr *e = NewExpr(package, ExprKindTernary, pos);
    e->Ternary.cond = cond;
    e->Ternary.pass = pass;
    e->Ternary.fail = fail;
    return e;
}

Expr *NewExprCast(Package *package, SourceRange pos, Expr *type, Expr *expr) {
    Expr *e = NewExpr(package, ExprKindCast, pos);
    e->Cast.type = type;
    e->Cast.expr = expr;
    return e;
}

Expr *NewExprAutocast(Package *package, SourceRange pos, Expr *expr) {
    Expr *e = NewExpr(package, ExprKindAutocast, pos);
    e->Autocast.expr = expr;
    return e;
}

Expr *NewExprLocationDirective(Package *package, SourceRange pos, const char *name) {
    Expr *e = NewExpr(package, ExprKindLocationDirective, pos);
    e->LocationDirective.name = name;
    return e;
}

Expr *NewExprLitNil(Package *package, SourceRange pos) {
    Expr *e = NewExpr(package, ExprKindLitNil, pos);
    return e;
}

Expr *NewExprLitInt(Package *package, SourceRange pos, u64 val) {
    Expr *e = NewExpr(package, ExprKindLitInt, pos);
    e->LitInt.val = val;
    return e;
}

Expr *NewExprLitFloat(Package *package, SourceRange pos, f64 val) {
    Expr *e = NewExpr(package, ExprKindLitFloat, pos);
    e->LitFloat.val = val;
    return e;
}

Expr *NewExprLitString(Package *package, SourceRange pos, const char *val) {
    Expr *e = NewExpr(package, ExprKindLitString, pos);
    e->LitString.val = val;
    return e;
}

Expr *NewExprLitCompound(Package *package, SourceRange pos, Expr *type, DynamicArray(KeyValue) elements) {
    Expr *e = NewExpr(package, ExprKindLitCompound, pos);
    e->LitCompound.type = type;
    e->LitCompound.elements = elements;
    return e;
}

Expr *NewExprLitFunction(Package *package, SourceRange pos, Expr *type, Stmt *body, u8 flags) {
    ASSERT(body->kind == StmtKindBlock);
    Expr *e = NewExpr(package, ExprKindLitFunction, pos);
    e->LitFunction.type = type;
    e->LitFunction.body = body;
    e->LitFunction.flags = flags;
    return e;
}

Expr *NewExprTypePointer(Package *package, SourceRange pos, Expr *type) {
    Expr *e = NewExpr(package, ExprKindTypePointer, pos);
    e->TypePointer.type = type;
    return e;
}

Expr *NewExprTypeArray(Package *package, SourceRange pos, Expr *length, Expr *type) {
    Expr *e = NewExpr(package, ExprKindTypeArray, pos);
    e->TypeArray.length = length;
    e->TypeArray.type = type;
    return e;
}

Expr *NewExprTypeSlice(Package *package, SourceRange pos, Expr *type) {
    Expr *e = NewExpr(package, ExprKindTypeSlice, pos);
    e->TypeSlice.type = type;
    return e;
}

Expr *NewExprTypeStruct(Package *package, SourceRange pos, DynamicArray(AggregateItem) items) {
    Expr *e = NewExpr(package, ExprKindTypeStruct, pos);
    e->TypeStruct.items = items;
    return e;
}

Expr *NewExprTypeEnum(Package *package, SourceRange pos, Expr *explicitType, DynamicArray(EnumItem) items) {
    Expr *e = NewExpr(package, ExprKindTypeEnum, pos);
    e->TypeEnum.explicitType = explicitType;
    e->TypeEnum.items = items;
    return e;
}

Expr *NewExprTypeUnion(Package *package, SourceRange pos, DynamicArray(AggregateItem) items) {
    Expr *e = NewExpr(package, ExprKindTypeUnion, pos);
    e->TypeUnion.items = items;
    return e;
}

Expr *NewExprTypePolymorphic(Package *package, SourceRange pos, const char *name) {
    Expr *e = NewExpr(package, ExprKindTypePolymorphic, pos);
    e->TypePolymorphic.name = name;
    return e;
}

Expr *NewExprTypeVariadic(Package *package, SourceRange pos, Expr *type, u8 flags) {
    Expr *e = NewExpr(package, ExprKindTypeVariadic, pos);
    e->TypeVariadic.type = type;
    e->TypeVariadic.flags = flags;
    return e;
}

Expr *NewExprTypeFunction(Package *package, SourceRange pos, DynamicArray(KeyValue) params, DynamicArray(Expr *)result) {
    Expr *e = NewExpr(package, ExprKindTypeFunction, pos);
    e->TypeFunction.params = params;
    e->TypeFunction.result = result;
    return e;
}

Stmt *NewStmtEmpty(Package *package, SourceRange pos) {
    return NewStmt(package, StmtKindEmpty, pos);
}

Stmt *NewStmtLabel(Package *package, SourceRange pos, const char *name) {
    Stmt *s = NewStmt(package, StmtKindLabel, pos);
    s->Label.name = name;
    return s;
}

Stmt *NewStmtAssign(Package *package, SourceRange pos, DynamicArray(Expr *) lhs, DynamicArray(Expr*) rhs) {
    Stmt *s = NewStmt(package, StmtKindAssign, pos);
    s->Assign.lhs = lhs;
    s->Assign.rhs = rhs;
    return s;
}

Stmt *NewStmtReturn(Package *package, SourceRange pos, DynamicArray(Expr *) exprs) {
    Stmt *s = NewStmt(package, StmtKindReturn, pos);
    s->Return.exprs = exprs;
    return s;
}

Stmt *NewStmtDefer(Package *package, SourceRange pos, Stmt *stmt) {
    Stmt *s = NewStmt(package, StmtKindDefer, pos);
    s->Defer.stmt = stmt;
    return s;
}

Stmt *NewStmtUsing(Package *package, SourceRange pos, Expr *expr) {
    Stmt *s = NewStmt(package, StmtKindUsing, pos);
    s->Using.expr = expr;
    return s;
}

Stmt *NewStmtGoto(Package *package, SourceRange pos, const char *keyword, Expr *target) {
    Stmt *s = NewStmt(package, StmtKindGoto, pos);
    s->Goto.keyword = keyword;
    s->Goto.target = target;
    return s;
}

Stmt *NewStmtBlock(Package *package, SourceRange pos, DynamicArray(Stmt *) stmts) {
    Stmt *s = NewStmt(package, StmtKindBlock, pos);
    s->Block.stmts = stmts;
    return s;
}

Stmt *NewStmtIf(Package *package, SourceRange pos, Expr *cond, Stmt *pass, Stmt *fail) {
    Stmt *s = NewStmt(package, StmtKindIf, pos);
    s->If.cond = cond;
    s->If.pass = pass;
    s->If.fail = fail;
    return s;
}

Stmt *NewStmtFor(Package *package, SourceRange pos, Stmt *init, Expr *cond, Stmt *step, Stmt *body) {
    ASSERT(body->kind == StmtKindBlock);
    Stmt *s = NewStmt(package, StmtKindFor, pos);
    s->For.init = init;
    s->For.cond = cond;
    s->For.step = step;
    s->For.body = body;
    return s;
}

Stmt *NewStmtForIn(Package *package, SourceRange pos, Expr *valueName, Expr *indexName, Expr *aggregate, Stmt *body) {
    ASSERT(valueName->kind == ExprKindIdent);
    ASSERT(indexName->kind == ExprKindIdent);
    ASSERT(body->kind == StmtKindBlock);
    Stmt *s = NewStmt(package, StmtKindForIn, pos);
    s->ForIn.valueName = valueName;
    s->ForIn.indexName = indexName;
    s->ForIn.aggregate = aggregate;
    s->ForIn.body = body;
    return s;
}

Stmt *NewStmtSwitch(Package *package, SourceRange pos, Expr *match, DynamicArray(Stmt *) cases) {
    Stmt *s = NewStmt(package, StmtKindSwitch, pos);
    s->Switch.match = match;
    s->Switch.cases = cases;
    return s;
}

Stmt *NewStmtSwitchCase(Package *package, SourceRange pos, DynamicArray(Expr *) matches, Stmt *body) {
    ASSERT(body->kind == StmtKindBlock);
    Stmt *s = NewStmt(package, StmtKindSwitchCase, pos);
    s->SwitchCase.matches = matches;
    s->SwitchCase.body = body;
    return s;
}

Decl *NewDeclVariable(Package *package, SourceRange pos, DynamicArray(Expr *) names, Expr *type, DynamicArray(Expr *) values) {
    Decl *d = NewDecl(package, DeclKindVariable, pos);
    d->Variable.names = names;
    d->Variable.type = type;
    d->Variable.values = values;
    return d;
}

Decl *NewDeclConstant(Package *package, SourceRange pos, DynamicArray(Expr *) names, Expr *type, DynamicArray(Expr *) values) {
    Decl *d = NewDecl(package, DeclKindConstant, pos);
    d->Constant.names = names;
    d->Constant.type = type;
    d->Constant.values = values;
    return d;
}

Decl *NewDeclForeign(Package *package, SourceRange pos, Expr *library, bool isConstant, const char *name, Expr *type, const char *linkname, const char *callingConvention) {
    Decl *d = NewDecl(package, DeclKindForeign, pos);
    d->Foreign.library = library;
    d->Foreign.isConstant = isConstant;
    d->Foreign.name = name;
    d->Foreign.type = type;
    d->Foreign.linkname = linkname;
    d->Foreign.callingConvention = callingConvention;
    return d;
}

Decl *NewDeclForeignBlock(Package *package, SourceRange pos, Expr *library, const char *callingConvention, DynamicArray(Decl_ForeignBlockMember) members) {
    Decl *d = NewDecl(package, DeclKindForeignBlock, pos);
    d->ForeignBlock.library = library;
    d->ForeignBlock.members = members;
    d->ForeignBlock.callingConvention = callingConvention;
    return d;
}

Decl *NewDeclImport(Package *package, SourceRange pos, Expr *path, const char *alias) {
    Decl *d = NewDecl(package, DeclKindImport, pos);
    d->Import.path = path;
    d->Import.alias = alias;
    return d;
}

#if TEST
void test_isExpr() {
    Package pkg = {0};
    SourceRange pos = {0};
    Stmt *expr = (Stmt *) NewExprIdent(&pkg, pos, NULL);
    ASSERT(isExpr(expr));
}

void test_doesExprAllocate() {
    ASSERT(DoesStmtKindAllocateTypeInfo[ExprKindIdent]);
    ASSERT(DoesStmtKindAllocateTypeInfo[ExprKindSelector]);
    ASSERT(DoesStmtKindAllocateTypeInfo[StmtKindDefer] == false);
}
#endif
