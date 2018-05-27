#include "checker.h"

typedef enum ExprMode {
    ExprMode_Invalid,
    ExprMode_Computed,
    ExprMode_Assignable,
    ExprMode_Addressable,
    ExprMode_Nil,
    ExprMode_File,
    ExprMode_Library,
    ExprMode_Type
} ExprMode;

typedef struct ExprInfo ExprInfo;
struct ExprInfo {
    Type *desiredType;
    ExprMode mode;
};

#define DeclCase(kind, node) case StmtDeclKind_##kind: { \
    Decl_##kind *decl = &node->kind;
    
#define ExprCase(kind, node) case ExprKind_##kind: { \
    Expr_##kind *expr = &node->kind;


#define CaseEnd() } break;

void invalidateSymbol(Symbol *symbol) {
    if (symbol) {
        symbol->state = SymbolState_Resolved;
        symbol->kind = SymbolKind_Invalid;
    }
}

Type *lowerMeta(Package *pkg, Type *type, Position pos) {
    if (type->kind != TypeKind_Metatype) {
        ReportError(pkg, InvalidMetatypeError, pos, "%s cannot be used as a type", DescribeTypeKind(type->kind));
        return NULL;
    }

    return type->Metatype.instanceType;
}

Scope *pushScope(Package *pkg, Scope *parent) {
    Scope *scope = ArenaCalloc(&pkg->arena, sizeof(Scope));
    scope->parent = parent;
    return scope;
}

Scope *popScope(Package *pkg, Scope *scope) {
    ASSERT(scope->parent);
    return scope->parent;
}

Symbol *Lookup(Scope *scope, const char *name) {
    do {
        Symbol *symbol = MapGet(&scope->members, name);
        if (symbol) {
            return symbol;
        }

        scope = scope->parent;
    } while (scope);

    return NULL;
}

Type *checkExpr(Package *pkg, Expr *expr, ExprInfo *exprInfo) {
    switch (expr->kind) {
    case ExprKind_Ident: {
        Expr_Ident ident = expr->Ident; 
        // FIXME(Brett): just use global scope as an example. Update with context-specific scope
        Symbol *symbol = Lookup(pkg->globalScope, ident.name);
        if (!symbol) {
            ReportError(pkg, UndefinedIdentError, expr->start, "Use of undefined identifier '%s'", ident.name);
            exprInfo->mode = ExprMode_Invalid;
            return InvalidType;
        }

    } break;
    }

    return NULL;
}

Type *checkFuncType(Expr_TypeFunction *func, ExprInfo *exprInfo) {
    return NULL;
}

b32 checkConstDecl(Package *pkg, Decl *declStmt) {
    Decl_Constant decl = declStmt->Constant;

    if (ArrayLen(decl.names) != 1) {
        ReportError(pkg, MultipleConstantDeclError, decl.start, "Constant declarations must declare at most one item");

        if (ArrayLen(decl.names) > 0) {
            for (size_t i = 0; i < ArrayLen(decl.names); i++) {
                Symbol *symbol = MapGet(&pkg->symbolMap, decl.names[0]->name);
                invalidateSymbol(symbol);
            }
        }

        return false;
    }

    if (ArrayLen(decl.values) > 1) {
        ReportError(pkg, ArityMismatchError, decl.start, "Constant declarations only allow for a single value, but got %zu", ArrayLen(decl.values));
        return false;
    }

    Type *expectedType = NULL;

    if (decl.type) {
        ExprInfo info = {0};
        expectedType = lowerMeta(pkg, checkExpr(pkg, decl.type, &info), decl.type->start);
    }

    Expr_Ident *name = decl.names[0];
    Expr *value = decl.values[0];
    Symbol *symbol = MapGet(&pkg->symbolMap, name->name);
    symbol->state = SymbolState_Resolving;

    switch (value->kind) {
    ExprCase(TypeFunction, value)
        ExprInfo info = {.desiredType = expectedType};

        Type *type = checkFuncType(expr, &info);
        // TODO(Brett): store this info and update context
    CaseEnd()
    }

    return false;
}

b32 checkVarDecl(Package *pkg, Decl *declStmt) {
    Decl_Variable var = declStmt->Variable;

    return false;
}

b32 checkImportDecl(Package *pkg, Decl *declStmt) {
    Decl_Import import = declStmt->Import;

    return false;
}

b32 check(Package *pkg, Stmt *stmt) {
    b32 shouldRequeue;

    Symbol *test = Lookup(pkg->globalScope, "i32");

    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        shouldRequeue = checkConstDecl(pkg, (Decl *)stmt);
    } break;

    case StmtDeclKind_Variable: {
        shouldRequeue = checkVarDecl(pkg, (Decl *)stmt);
    } break;

    case StmtDeclKind_Import: {
        shouldRequeue = checkImportDecl(pkg, (Decl *)stmt);
    } break;

    default:
        ASSERT_MSG_VA(false, "Statement of type '%s' went unchecked", AstDescriptions[stmt->kind]);
    }

    return shouldRequeue;
}

#if TEST
void test_checkerTest() {

}
#endif
