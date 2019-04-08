
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
    ASSERT_EXPR_KIND(ExprKindIdent);
    ASSERT_EXPR_KIND(ExprKindLitInt);
    ASSERT_EXPR_KIND(ExprKindLitFloat);
    ASSERT_EXPR_KIND(ExprKindLocationDirective);
    ASSERT_EXPR_KIND(ExprKindLitNil);

    p = newTestParser("fn () -> a []a [2]a *a ..a $a struct { anA: a } enum { A }");
    ASSERT_EXPR_KIND(ExprKindTypeFunction);
    ASSERT_EXPR_KIND(ExprKindTypeSlice);
    ASSERT_EXPR_KIND(ExprKindTypeArray);
    ASSERT_EXPR_KIND(ExprKindTypePointer);
    ASSERT_EXPR_KIND(ExprKindTypeVariadic);
    ASSERT_EXPR_KIND(ExprKindTypePolymorphic);
    ASSERT_EXPR_KIND(ExprKindTypeStruct);
    ASSERT_EXPR_KIND(ExprKindTypeEnum);

    p = newTestParser("fn (a, b: u32, c: $T, d: #cvargs ..any) -> (a: u32, b: u32)"
                      "fn (a, b: $T) -> (b, a: T)"
                      "fn (u8, u8) -> (u8, u8)");
    ASSERT_EXPR_KIND(ExprKindTypeFunction);
    ASSERT_EXPR_KIND(ExprKindTypeFunction);
    ASSERT_EXPR_KIND(ExprKindTypeFunction);

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
    ASSERT_EXPR_KIND(ExprKindSelector);

    p = newTestParser("a[:] a[5:] a[:5] a[5:5] a[5]");
    ASSERT_EXPR_KIND(ExprKindSlice);
    ASSERT_EXPR_KIND(ExprKindSlice);
    ASSERT(expr->Slice.lo != NULL);
    ASSERT_EXPR_KIND(ExprKindSlice);
    ASSERT(expr->Slice.hi != NULL);
    ASSERT_EXPR_KIND(ExprKindSlice);
    ASSERT(expr->Slice.lo != NULL && expr->Slice.hi != NULL);
    ASSERT_EXPR_KIND(ExprKindSubscript);

    p = newTestParser("a() a(a,\n) a(a, b, c,)");
    ASSERT_EXPR_KIND(ExprKindCall);
    ASSERT_EXPR_KIND(ExprKindCall);
    ASSERT(ArrayLen(expr->Call.args) == 1);
    ASSERT_EXPR_KIND(ExprKindCall);
    ASSERT(ArrayLen(expr->Call.args) == 3);

    p = newTestParser("A{} A{1, a: 2, 3,} A{[23]: 1, 2, 3}");
    ASSERT_EXPR_KIND(ExprKindLitCompound);
    ASSERT_EXPR_KIND(ExprKindLitCompound);
    ASSERT(ArrayLen(expr->LitCompound.elements) == 3);
    ASSERT_EXPR_KIND(ExprKindLitCompound);
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
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);
    ASSERT_EXPR_KIND(ExprKindUnary);

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
    ASSERT_EXPR_KIND(ExprKindBinary);
    ASSERT(expr->Binary.op.kind == TK_Add);

    p = newTestParser("(a + b) * c");
    ASSERT_EXPR_KIND(ExprKindBinary);
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
    ASSERT_EXPR_KIND(ExprKindTernary);

    p = newTestParser("a ?: b");
    ASSERT_EXPR_KIND(ExprKindTernary);

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
    ASSERT_STMT_KIND(DeclKindVariable);

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
    ASSERT_STMT_KIND(DeclKindVariable);

    p = newTestParser("a :: b");
    ASSERT_STMT_KIND(DeclKindConstant);

    p = newTestParser("a:");
    ASSERT_STMT_KIND(StmtKindLabel);

    p = newTestParser("for a := 1; a < 2; a += 1 {}");
    ASSERT_STMT_KIND(StmtKindFor);

    p = newTestParser("for a, b in foo {}");
    ASSERT_STMT_KIND(StmtKindForIn);

    p = newTestParser("defer Free(mem)");
    ASSERT_STMT_KIND(StmtKindDefer);

    p = newTestParser("break");
    ASSERT_STMT_KIND(StmtKindGoto);
    ASSERT(stmt->Goto.keyword == Keyword_break);
    ASSERT(stmt->Goto.target == NULL);

    p = newTestParser("break label");
    ASSERT_STMT_KIND(StmtKindGoto);
    ASSERT(stmt->Goto.keyword == Keyword_break);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("continue label");
    ASSERT_STMT_KIND(StmtKindGoto);
    ASSERT(stmt->Goto.keyword == Keyword_continue);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("fallthrough");
    ASSERT_STMT_KIND(StmtKindGoto);
    ASSERT(stmt->Goto.keyword == Keyword_fallthrough);
    ASSERT(stmt->Goto.target == NULL);

    p = newTestParser("goto label");
    ASSERT_STMT_KIND(StmtKindGoto);
    ASSERT(stmt->Goto.keyword == Keyword_goto);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("for { break }");
    ASSERT_STMT_KIND(StmtKindFor);
    ASSERT(ArrayLen(stmt->For.body->Block.stmts) == 1);

    p = newTestParser("if true {}");
    ASSERT_STMT_KIND(StmtKindIf);
    ASSERT(stmt->If.cond);
    ASSERT(stmt->If.cond->kind == ExprKindIdent);
    ASSERT(stmt->If.pass);
    ASSERT(stmt->If.pass->kind == StmtKindBlock);
    ASSERT(!stmt->If.fail);

    p = newTestParser("if true {} else {}");
    ASSERT_STMT_KIND(StmtKindIf);
    ASSERT(stmt->If.cond);
    ASSERT(stmt->If.cond->kind == ExprKindIdent);
    ASSERT(stmt->If.pass);
    ASSERT(stmt->If.pass->kind == StmtKindBlock);
    ASSERT(stmt->If.fail);
    ASSERT(stmt->If.fail->kind == StmtKindBlock);

    p = newTestParser("error:");
    ASSERT_STMT_KIND(StmtKindLabel);
    ASSERT(stmt->Label.name == StrIntern("error"));

    p = newTestParser("return");
    ASSERT_STMT_KIND(StmtKindReturn);
    ASSERT(!stmt->Return.exprs);

    p = newTestParser("return 1, 2, 3");
    ASSERT_STMT_KIND(StmtKindReturn);
    ASSERT(ArrayLen(stmt->Return.exprs) == 3);

    p = newTestParser("switch {"
                      "}");
    ASSERT_STMT_KIND(StmtKindSwitch);
    ASSERT(!stmt->Switch.match);
    ASSERT(!stmt->Switch.cases);

    p = newTestParser("switch foo {\n"
                      "case 1:\n"
                      "case:\n"
                      "}\n");
    ASSERT_STMT_KIND(StmtKindSwitch);
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
    ASSERT_EXPR_KIND(ExprKindTypeStruct);
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
    ASSERT_EXPR_KIND(ExprKindTypeUnion);
}
#endif
