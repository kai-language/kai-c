#include "checker.h"

typedef struct ExprInfo ExprInfo;
struct ExprInfo {
    Type *desiredType;
};

#define DeclCase(kind, node) case StmtDeclKind_##kind: { \
    Decl_##kind *decl = &node->kind;
    
#define ExprCase(kind, node) case ExprKind_##kind: { \
    Expr_##kind *expr = &node->kind;


#define CaseEnd() } break;

Type *checkExpr(Expr *expr, ExprInfo *exprInfo) {
    return NULL;
}

Type *checkFuncType(Expr_TypeFunction *func, ExprInfo *exprInfo) {
    return NULL;
}

void checkConstDecl(Package *package, Decl_Constant *decl) {
    if (ArrayLen(decl->names) != 1) {
        ReportError(package, MultipleConstantDeclError, decl->start, "Constant declarations must declare at most one item");
        return;
    }

    if (ArrayLen(decl->values) > 1) {
        ReportError(package, ArityMismatchError, decl->start, "Constant declarations only allow for a single value, but got %zu", ArrayLen(decl->values));
        return;
    }

    Type *explicitType;

    if (decl->type) {
        ExprInfo info = {0};
        explicitType = checkExpr(decl->type, &info);
    }

    Expr_Ident *name = decl->names[0];
    Expr *value = decl->values[0];

    switch (value->kind) {
    ExprCase(TypeFunction, value)
        ExprInfo info = {.desiredType = explicitType};
        Type *type = checkFuncType(expr, &info);
        // TODO(Brett): store this info and update context
    CaseEnd()
    }


}

void checkVarDecl(Package *package, Decl_Variable *var) {

}

void checkImportDecl(Package *package, Decl_Import *import) {

}

void check(Package *package, Stmt *stmt) {
    switch (stmt->kind) {
    DeclCase(Constant, stmt)
        checkConstDecl(package, decl);
    CaseEnd()

    DeclCase(Variable, stmt)
        checkVarDecl(package, decl);
    CaseEnd()

    DeclCase(Import, stmt)
        checkImportDecl(package, decl);
    CaseEnd()

    default: ASSERT_MSG_VA(false, "Statement of type '%s' went unchecked", AstDescriptions[stmt->kind]);
    }
}
