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
    Scope *scope;
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

Inline
void resolveSymbol(Symbol *symbol, Type *type) {
    symbol->state = SymbolState_Resolved;
    symbol->type = type;
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
        Symbol *symbol = Lookup(exprInfo->scope, ident.name);
        if (!symbol) {
            ReportError(pkg, UndefinedIdentError, expr->start, "Use of undefined identifier '%s'", ident.name);
            exprInfo->mode = ExprMode_Invalid;
            return InvalidType;
        }

        symbol->used = true;
        if (symbol->state != SymbolState_Resolved) {
            // TODO(Brett): requeue stmt and try again later
            UNIMPLEMENTED();
        }

        CheckerInfo_Ident identInfo = {.symbol = symbol};
        CheckerInfo info;
        info.kind = CheckerInfoKind_Ident;
        info.Ident = identInfo;
        pkg->checkerInfo[expr->id] = info;

        return symbol->type;
    } break;
    }

    return NULL;
}

Type *checkFuncType(Expr_TypeFunction *func, ExprInfo *exprInfo) {
    return NULL;
}

b32 checkConstDecl(Package *pkg, Scope *scope, Decl *declStmt) {
    Decl_Constant decl = declStmt->Constant;

    ASSERT(scope);

    if (ArrayLen(decl.names) != 1) {
        ReportError(pkg, MultipleConstantDeclError, decl.start, "Constant declarations must declare at most one item");

        if (ArrayLen(decl.names) > 0) {
            for (size_t i = 0; i < ArrayLen(decl.names); i++) {
                const char *name = decl.names[i]->name;
                Symbol *symbol = MapGet(&pkg->symbolMap, name);
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
        ExprInfo info = {.scope = scope};
        expectedType = lowerMeta(pkg, checkExpr(pkg, decl.type, &info), decl.type->start);
    }

    Expr_Ident *name = decl.names[0];
    Expr *value = decl.values[0];
    Symbol *symbol = MapGet(&pkg->symbolMap, name->name);
    symbol->state = SymbolState_Resolving;

    switch (value->kind) {
    case ExprKind_TypeFunction: {
        Expr_TypeFunction *expr = &value->TypeFunction;
        ExprInfo info = {.desiredType = expectedType, .scope = scope};
        Type *type = checkFuncType(expr, &info);
        resolveSymbol(symbol, type);
    } break;

    case ExprKind_TypeStruct:
    case ExprKind_TypeUnion:
    case ExprKind_TypeEnum: {

    } break;
    }

    return false;
}

b32 checkVarDecl(Package *pkg, Scope *scope, Decl *declStmt) {
    Decl_Variable var = declStmt->Variable;

    return false;
}

b32 checkImportDecl(Package *pkg, Decl *declStmt) {
    Decl_Import import = declStmt->Import;

    return false;
}

b32 check(Package *pkg, Stmt *stmt) {
    b32 shouldRequeue;

    Scope *scope = pkg->globalScope;

    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        shouldRequeue = checkConstDecl(pkg, scope, (Decl *)stmt);
    } break;

    case StmtDeclKind_Variable: {
        shouldRequeue = checkVarDecl(pkg, scope, (Decl *)stmt);
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
