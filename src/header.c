void cgenStmt(String buffer, Stmt *stmt) {

}

void CodegenCHeader(Package *pkg) {
    String buffer = NULL;

    For(pkg->stmts) {
        cgenStmt(buffer, pkg->stmts[i]);
    }

    printf("%s\n");
}
