
#if TEST
Package reset_package = {0};
Package test_package = {0};

Parser test_parser;

Parser new_test_parser(const char *stream) {
    test_package.scope = scope_push(&test_package, NULL);
    arrfree(test_package.errors);
    init_test_compiler(&compiler, NULL);
    Source source = { .code = stream, .len = (u32) strlen(stream) };
    Parser parser = { .package = &test_package, .source = &source };
    lexer_init(&parser.lexer, source.code);
    parser.lexer.client.data = &test_parser;
    parser.lexer.client.online = (void *) parser_online;
    parser.lexer.client.onname = (void *) parser_onname;
    parser.lexer.client.onstr  = (void *) parser_onstr;
    parser.lexer.client.onmsg  = (void *) parser_onmsg;
    eat_tok(&parser);
    return parser;
}

Expr *expr;

// Assert expression parses and is kind
#define AEKIND(expect, from) \
test_parser = new_test_parser(from);  \
expr = parse_expr(&test_parser); \
ASSERT_MSG_VA(!test_package.errors, "Parsing produced error: '%s'", test_package.errors[0].msg); \
ASSERT_MSG_VA(expr->kind == expect, "expected '%s' got '%s'", describe_ast_kind(expect), describe_ast_kind(expr->kind))

void test_parse_atoms() {
    AEKIND(EXPR_NAME, "a");
    AEKIND(EXPR_NIL, "nil");
    AEKIND(EXPR_INT, "1");
    AEKIND(EXPR_FLOAT, "1.0");
    AEKIND(EXPR_FIELD, "a.a.a.a.a.a.a.a.a.a.a");
}

void test_parse_types() {
    AEKIND(EXPR_FUNCTYPE, "fn () -> A");
    AEKIND(EXPR_SLICETYPE, "[]A");
    AEKIND(EXPR_ARRAY, "[2]A");
    AEKIND(EXPR_POINTER, "*A");
}

void test_parse_aggregate() {
    AEKIND(EXPR_STRUCT, "struct { a: A }");
    AEKIND(EXPR_STRUCT, "struct #opaque");
    AEKIND(EXPR_UNION, "union { a: A }");
    AEKIND(EXPR_STRUCT, "struct { a, b, c: u32 }");
    AEKIND(EXPR_UNION, "union { a, b, c: u32 }");
}

void test_parse_enums() {
    AEKIND(EXPR_ENUM, "enum { A }");
    AEKIND(EXPR_ENUM, "enum { A; B }");
}

void test_parse_functypes() {
    AEKIND(EXPR_FUNCTYPE, "fn");
    AEKIND(EXPR_FUNCTYPE, "fn (u8) -> u8");
    AEKIND(EXPR_FUNCTYPE, "fn (u8, u8) -> (u8, u8)");
    AEKIND(EXPR_FUNCTYPE, "fn (any.. #cvargs)");
    ASSERT(expr->flags&FUNC_VARGS);
    ASSERT(expr->flags&FUNC_CVARGS);
    AEKIND(EXPR_FUNCTYPE, "fn (a, b: u32, c: u32, d: any..) -> (a: u32, b: u32)");
    ASSERT(expr->flags&FUNC_VARGS);
//    ASSERT(expr->efunctype.flags&FUNC_CVARGS);
}

void test_parse_func() {
    AEKIND(EXPR_FUNC, "fn {}");
    AEKIND(EXPR_FUNC, "fn (a, b: u32) -> u32 { return a | b }");
}

void test_parse_slice() {
    AEKIND(EXPR_SLICE, "a[:]");
    AEKIND(EXPR_SLICE, "a[5:]");
    ASSERT(expr->eslice.lo != NULL);
    AEKIND(EXPR_SLICE, "a[:5]");
    ASSERT(expr->eslice.hi != NULL);
    AEKIND(EXPR_SLICE, "a[5:5]");
    ASSERT(expr->eslice.lo != NULL && expr->eslice.hi != NULL);
}

void test_parse_field() {
    AEKIND(EXPR_INDEX, "a[5]");
}

void test_parse_calls() {
    AEKIND(EXPR_CALL, "a()");
    AEKIND(EXPR_CALL, "a(a,\n)");
    ASSERT(arrlen(expr->ecall.args) == 1);
    AEKIND(EXPR_CALL, "a(a, b, c,)");
    ASSERT(arrlen(expr->ecall.args) == 3);
}

void test_parse_compound() {
    AEKIND(EXPR_COMPOUND, "A{}");
    AEKIND(EXPR_COMPOUND, "A{1, a: 2, 3}");
    ASSERT(arrlen(expr->ecompound.fields) == 3);
    AEKIND(EXPR_COMPOUND, "A{[2]: 1, 2, 3}");
    ASSERT(arrlen(expr->ecompound.fields) == 3);
    ASSERT(expr->ecompound.fields[0].kind == FIELD_INDEX);
}

void test_parse_unary() {
    AEKIND(EXPR_UNARY, "+5");
    AEKIND(EXPR_UNARY, "-5");
    AEKIND(EXPR_UNARY, "~a");
    AEKIND(EXPR_UNARY, "^a");
    AEKIND(EXPR_UNARY, "&a");
    AEKIND(EXPR_UNARY, "<a");
    AEKIND(EXPR_UNARY, "!!a");
}

void test_parse_binary() {
    AEKIND(EXPR_BINARY, "a + b * c");
    ASSERT(expr->flags == TK_Add);

    AEKIND(EXPR_BINARY, "(a + b) * c");
    ASSERT(expr->flags == TK_Mul);
}

void test_parse_ternary() {
    AEKIND(EXPR_TERNARY, "a ? b : c");
    AEKIND(EXPR_TERNARY, "a ?: b");
}

Stmt *stmt;

#define ASKIND(expect, from) \
test_parser = new_test_parser(from);  \
stmt = parse_stmt(&test_parser); \
ASSERT_MSG_VA(!test_package.errors, "Parsing produced error: '%s'", test_package.errors[0].msg); \
ASSERT_MSG_VA(stmt->kind == expect, "expected '%s' got '%s'", describe_ast_kind(expect), describe_ast_kind(expr->kind))

void test_parse_val() {
    ASKIND(DECL_VAL, "a :: b");
    ASKIND(DECL_VAL, "a : Ty : b");
}

void test_parse_var() {
    ASKIND(DECL_VAR, "a := b");
    ASKIND(DECL_VAR, "a : Ty = b");
    ASKIND(DECL_VAR, "a : Ty = b, c");
}

void test_parse_foreign() {
    ASKIND(DECL_FOREIGN, "#foreign libc" "\n"
                         "printf :: fn(fmt: *u8, args: any..) -> i32");
}

void test_parse_foreignblock() {
    ASKIND(DECL_FOREIGNBLOCK,
           "#foreign libc {" "\n"
           "  printf :: fn(fmt: *u8, args: any..) -> i32" "\n"
           "  puts :: fn(str: *u8) -> i32" "\n"
           "}");
    ASKIND(DECL_FOREIGNBLOCK,
           "#foreign libc #linkprefix \"glfw\" #callconv \"c\"" "\n"
           "{" "\n"
           "  Init :: fn -> i32" "\n"
           "  WindowHint :: fn (target, hint: i32)" "\n"
           "}");
}

void test_parse_label() {
    ASKIND(STMT_LABEL, "label:");
}

void test_parse_assign() {
    ASKIND(STMT_ASSIGN, "x = y");
    ASKIND(STMT_ASSIGN, "x, y = y, x");
    ASKIND(STMT_ASSIGN, "x, y = f()");
}

void test_parse_return() {
    ASKIND(STMT_RETURN, "return x");
    ASKIND(STMT_RETURN, "return x, y");
    ASKIND(STMT_RETURN, "return x, y()");
}

void test_parse_defer() {
    ASKIND(STMT_DEFER, "defer x = y");
    ASKIND(STMT_DEFER, "defer {x = y}");
}

//void test_parse_using() {
//    ASKIND(STMT_USING, "using x");
//    ASKIND(STMT_USING, "using x, y, z");
//}

void test_parse_goto() {
    ASKIND(STMT_GOTO, "goto label");
    ASSERT(stmt->flags == GOTO_GOTO);
    ASSERT(stmt->sgoto);
    ASKIND(STMT_GOTO, "break");
    ASSERT(stmt->flags == GOTO_BREAK);
    ASSERT(!stmt->sgoto);
    ASKIND(STMT_GOTO, "fallthrough");
    ASSERT(!stmt->sgoto);
    ASKIND(STMT_GOTO, "continue");
    ASSERT(!stmt->sgoto);
}

void test_parse_block() {
    ASKIND(STMT_BLOCK, "{}");
    ASKIND(STMT_BLOCK,
           "{" "\n"
           "  call()" "\n"
           "}");
}

void test_parse_if() {
    ASKIND(STMT_IF, "if expr {}");
    ASKIND(STMT_IF, "if expr {} else {}");
    ASKIND(STMT_IF, "if expr a() else b()");
}

void test_parse_for() {
    ASKIND(STMT_FOR, "for {}");
    ASKIND(STMT_FOR, "for true { }");
    ASKIND(STMT_FOR, "for i := 0; i < 10; i = i + 1 { }");
    ASSERT(stmt->flags == FOR_REGULAR);
    ASKIND(STMT_FOR, "for val in arr {}");
    ASKIND(STMT_FOR, "for val, idx in arr {}");
    ASSERT(stmt->flags == FOR_AGGREGATE);
}

void test_parse_switch() {
    ASKIND(STMT_SWITCH, "switch value {}");
    ASKIND(STMT_SWITCH,
           "switch value {" "\n"
           "case 0:" "\n"
           "case:" "\n"
           "}");
    ASSERT(arrlen(stmt->sswitch.cases) == 2);
}
#endif
