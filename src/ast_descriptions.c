
// @ErrorQuality
// FIXME: Better description
const char *DescribeStmt(Stmt *stmt) {
    return AstDescriptions[stmt->kind];
}

const char *DescribeExpr(Expr *expr) {
    char buf[1024];

    switch (expr->kind) {
        case ExprKind_Ident:
            return expr->Ident.name;

        case ExprKind_LitInt:
            sprintf(buf, "%llu", expr->LitInt.val);
            return StrIntern(buf);

        default:
            return AstDescriptions[expr->kind];
    }
}

const char *DescribeDecl(Decl *decl) {
    return AstDescriptions[decl->kind];
}
