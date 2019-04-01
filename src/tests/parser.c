
#if TEST

Package parserTestPackage = {0};
Parser newTestParser(const char *stream) {
    Lexer lex = MakeLexer(stream, NULL);
    Token tok = NextToken(&lex);

    ArrayFree(parserTestPackage.diagnostics.errors);
    ArrayFree(parserTestPackage.stmts);
    ArrayFree(parserTestPackage.symbols);

    ArenaFree(&parserTestPackage.arena);
    ArenaFree(&parserTestPackage.diagnostics.arena);

    Parser p = {lex, .tok = tok, &parserTestPackage};
    return p;
}

void test_parseExprAtom() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a 1 1.0 #line nil");
    ASSERT_EXPR_KIND(ExprKind_Ident);
    ASSERT_EXPR_KIND(ExprKind_LitInt);
    ASSERT_EXPR_KIND(ExprKind_LitFloat);
    ASSERT_EXPR_KIND(ExprKind_LocationDirective);
    ASSERT_EXPR_KIND(ExprKind_LitNil);

    p = newTestParser("fn () -> a []a [2]a *a ..a $a struct { anA: a } enum { A }");
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeSlice);
    ASSERT_EXPR_KIND(ExprKind_TypeArray);
    ASSERT_EXPR_KIND(ExprKind_TypePointer);
    ASSERT_EXPR_KIND(ExprKind_TypeVariadic);
    ASSERT_EXPR_KIND(ExprKind_TypePolymorphic);
    ASSERT_EXPR_KIND(ExprKind_TypeStruct);
    ASSERT_EXPR_KIND(ExprKind_TypeEnum);

    p = newTestParser("fn (a, b: u32, c: $T, d: #cvargs ..any) -> (a: u32, b: u32)"
                      "fn (a, b: $T) -> (b, a: T)"
                      "fn (u8, u8) -> (u8, u8)");
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);

#undef ASSERT_EXPR_KIND
}

void test_parseExprPrimary() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprPrimary(&p, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a.a.a.a.a.a.a.a.a.a.a.a");
    ASSERT_EXPR_KIND(ExprKind_Selector);

    p = newTestParser("a[:] a[5:] a[:5] a[5:5] a[5]");
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT(expr->Slice.lo != NULL);
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT(expr->Slice.hi != NULL);
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT(expr->Slice.lo != NULL && expr->Slice.hi != NULL);
    ASSERT_EXPR_KIND(ExprKind_Subscript);

    p = newTestParser("a() a(a,\n) a(a, b, c,)");
    ASSERT_EXPR_KIND(ExprKind_Call);
    ASSERT_EXPR_KIND(ExprKind_Call);
    ASSERT(ArrayLen(expr->Call.args) == 1);
    ASSERT_EXPR_KIND(ExprKind_Call);
    ASSERT(ArrayLen(expr->Call.args) == 3);

    p = newTestParser("A{} A{1, a: 2, 3,} A{[23]: 1, 2, 3}");
    ASSERT_EXPR_KIND(ExprKind_LitCompound);
    ASSERT_EXPR_KIND(ExprKind_LitCompound);
    ASSERT(ArrayLen(expr->LitCompound.elements) == 3);
    ASSERT_EXPR_KIND(ExprKind_LitCompound);
    ASSERT(ArrayLen(expr->LitCompound.elements) == 3);
    ASSERT(expr->LitCompound.elements[0].flags & KeyValueFlag_Index);

#undef ASSERT_EXPR_KIND
}

void test_parseExprUnary() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprUnary(&p, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("+5 -5 ~a ^a !a &a <a !!a");
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);

#undef ASSERT_EXPR_KIND
}

void test_parseExprBinary() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprBinary(&p, 1, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a + b * c");
    ASSERT_EXPR_KIND(ExprKind_Binary);
    ASSERT(expr->Binary.op.kind == TK_Add);

    p = newTestParser("(a + b) * c");
    ASSERT_EXPR_KIND(ExprKind_Binary);
    ASSERT(expr->Binary.op.kind == TK_Mul);

#undef ASSERT_EXPR_KIND
}

void test_parseExprTernary() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprBinary(&p, 1, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a ? b : c");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

    p = newTestParser("a ?: b");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

#undef ASSERT_EXPR_KIND
}

void test_parseSimpleStmt() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_STMT_KIND(expected) \
stmt = parseSimpleStmt(&p, false, NULL); \
ASSERT(stmt->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Stmt *stmt;

    Parser p = newTestParser("a := b");
    ASSERT_STMT_KIND(DeclKind_Variable);

#undef ASSERT_STMT_KIND
}

void test_parseStmt() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_STMT_KIND(expected) \
stmt = parseStmt(&p); \
ASSERT(stmt->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Stmt *stmt;
    Parser p;

    p = newTestParser("a := b");
    ASSERT_STMT_KIND(DeclKind_Variable);

    p = newTestParser("a :: b");
    ASSERT_STMT_KIND(DeclKind_Constant);

    p = newTestParser("a:");
    ASSERT_STMT_KIND(StmtKind_Label);

    p = newTestParser("for a := 1; a < 2; a += 1 {}");
    ASSERT_STMT_KIND(StmtKind_For);

    p = newTestParser("for a, b in foo {}");
    ASSERT_STMT_KIND(StmtKind_ForIn);

    p = newTestParser("defer Free(mem)");
    ASSERT_STMT_KIND(StmtKind_Defer);

    p = newTestParser("break");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_break);
    ASSERT(stmt->Goto.target == NULL);

    p = newTestParser("break label");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_break);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("continue label");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_continue);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("fallthrough");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_fallthrough);
    ASSERT(stmt->Goto.target == NULL);

    p = newTestParser("goto label");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_goto);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("for { break }");
    ASSERT_STMT_KIND(StmtKind_For);
    ASSERT(ArrayLen(stmt->For.body->Block.stmts) == 1);

    p = newTestParser("if true {}");
    ASSERT_STMT_KIND(StmtKind_If);
    ASSERT(stmt->If.cond);
    ASSERT(stmt->If.cond->kind == ExprKind_Ident);
    ASSERT(stmt->If.pass);
    ASSERT(stmt->If.pass->kind == StmtKind_Block);
    ASSERT(!stmt->If.fail);

    p = newTestParser("if true {} else {}");
    ASSERT_STMT_KIND(StmtKind_If);
    ASSERT(stmt->If.cond);
    ASSERT(stmt->If.cond->kind == ExprKind_Ident);
    ASSERT(stmt->If.pass);
    ASSERT(stmt->If.pass->kind == StmtKind_Block);
    ASSERT(stmt->If.fail);
    ASSERT(stmt->If.fail->kind == StmtKind_Block);

    p = newTestParser("error:");
    ASSERT_STMT_KIND(StmtKind_Label);
    ASSERT(stmt->Label.name == StrIntern("error"));

    p = newTestParser("return");
    ASSERT_STMT_KIND(StmtKind_Return);
    ASSERT(!stmt->Return.exprs);

    p = newTestParser("return 1, 2, 3");
    ASSERT_STMT_KIND(StmtKind_Return);
    ASSERT(ArrayLen(stmt->Return.exprs) == 3);

    p = newTestParser("switch {"
                      "}");
    ASSERT_STMT_KIND(StmtKind_Switch);
    ASSERT(!stmt->Switch.match);
    ASSERT(!stmt->Switch.cases);

    p = newTestParser("switch foo {\n"
                      "case 1:\n"
                      "case:\n"
                      "}\n");
    ASSERT_STMT_KIND(StmtKind_Switch);
    ASSERT(stmt->Switch.match);
    ASSERT(ArrayLen(stmt->Switch.cases) == 2);

#undef ASSERT_STMT_KIND
}

void test_automaticTerminatorAfterFunction() {
    InitTestCompiler(&compiler, NULL);

    Parser p = newTestParser("a :: fn() -> void {}" "\n"
                             "b :: fn() -> void {}" "\n");
    parseStmts(&p);
    ASSERT(!parserTestPackage.diagnostics.errors);
}

void test_parseStruct() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;
    Parser p;

    p = newTestParser("struct { a, b, c: u32 }");
    ASSERT_EXPR_KIND(ExprKind_TypeStruct);
}

void test_parseUnion() {
    InitTestCompiler(&compiler, NULL);

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;
    Parser p;

    p = newTestParser("union { a, b, c: u32 }");
    ASSERT_EXPR_KIND(ExprKind_TypeUnion);
}
#endif
