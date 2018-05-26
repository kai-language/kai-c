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

void checkConstDecl(Package *pkg, Decl *declStmt) {
    Decl_Constant decl = declStmt->Constant;

    if (ArrayLen(decl.names) != 1) {
        ReportError(pkg, MultipleConstantDeclError, decl.start, "Constant declarations must declare at most one item");
        return;
    }

    if (ArrayLen(decl.values) > 1) {
        ReportError(pkg, ArityMismatchError, decl.start, "Constant declarations only allow for a single value, but got %zu", ArrayLen(decl.values));
        return;
    }

    Type *explicitType;

    if (decl.type) {
        ExprInfo info = {0};
        explicitType = checkExpr(decl.type, &info);
    }

    Expr_Ident *name = decl.names[0];
    Expr *value = decl.values[0];
    Symbol *symbol = MapGet(&pkg->symbolMap, name->name);

    switch (value->kind) {
    ExprCase(TypeFunction, value)
        ExprInfo info = {.desiredType = explicitType};
        Type *type = checkFuncType(expr, &info);
        // TODO(Brett): store this info and update context
    CaseEnd()
    }


}

void checkVarDecl(Package *pkg, Decl *declStmt) {
    Decl_Variable var = declStmt->Variable;
}

void checkImportDecl(Package *pkg, Decl *declStmt) {
    Decl_Import import = declStmt->Import;
}

void check(Package *pkg, Stmt *stmt) {
    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        checkConstDecl(pkg, (Decl *)stmt);
    } break;

    case StmtDeclKind_Variable: {
        checkVarDecl(pkg, (Decl *)stmt);
    } break;

    case StmtDeclKind_Import: {
        checkImportDecl(pkg, (Decl *)stmt);
    } break;

    default: ASSERT_MSG_VA(false, "Statement of type '%s' went unchecked", AstDescriptions[stmt->kind]);
    }
}
