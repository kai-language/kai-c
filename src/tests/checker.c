
#if TEST
void test_canCoerce() {
    InitTestCompiler(&compiler, NULL);

    CheckerContext ctx = {0}; // No need for scope

    Type *PtrToU8 = NewTypePointer(TypeFlag_None, U8Type);

    ASSERT(canCoerce(I8Type, AnyType, &ctx));
    ASSERT(canCoerce(PtrToU8, AnyType, &ctx));
    ASSERT(canCoerce(F32Type, AnyType, &ctx));

    ASSERT(canCoerce(I8Type, BoolType, &ctx));
    ASSERT(canCoerce(F32Type, BoolType, &ctx));
    ASSERT(canCoerce(PtrToU8, BoolType, &ctx));

    ASSERT(canCoerce(I64Type, F32Type, &ctx));

    ASSERT(canCoerce(I8Type, I16Type, &ctx));
    ASSERT(canCoerce(U8Type, I16Type, &ctx));
    ASSERT(canCoerce(U8Type, I32Type, &ctx));

    ASSERT(canCoerce(I32Type, IntType, &ctx));
    ASSERT(canCoerce(IntType, I32Type, &ctx));

    ASSERT(canCoerce(UintType, UintptrType, &ctx));
    ASSERT(!canCoerce(I8Type, U64Type, &ctx));
    ASSERT(canCoerce(PtrToU8, RawptrType, &ctx));
    ASSERT(canCoerce(RawptrType, PtrToU8, &ctx));
    // TODO: Coercion to union
    // TODO: Coercion from enum

    ctx.flags |= CheckerContextFlag_Constant;
    ctx.val.i8 = 100;
    ASSERT(canCoerce(I8Type, U8Type, &ctx));

    ctx.val.i8 = -100;
    ASSERT(!canCoerce(I8Type, U8Type, &ctx));

    ctx.val.u64 = 100;
    ASSERT(canCoerce(U64Type, I8Type, &ctx));
}

void reset_package(Package *package);
Stmt *reset_parse_check_upto_last(Package *package, const char *code) {
    ArenaFree(&compiler.parsing_queue.arena);
    ArenaFree(&compiler.checking_queue.arena);

    InitTestCompiler(&compiler, NULL);
    reset_package(package);

    Source source = {":test:", ":test:", code};
    package->current_source = &source;
    parse_test_package(package);

    ASSERT_MSG(!HasErrors(package), "Parsing resulted in errors");

    while (compiler.checking_queue.size > 1) {
        CheckerWork *work = QueuePopFront(&compiler.checking_queue);
        CheckerContext ctx = { work->package->scope };
        checkStmt(work->stmt, &ctx, work->package);
    }

    CheckerWork *work = QueuePopFront(&compiler.checking_queue);
    ASSERT(work);
    Stmt *stmt = work->stmt;
    return stmt;
}

void reset_package(Package *package) {
    ArrayFree(package->diagnostics.errors);
    ArrayFree(package->stmts);
    ArrayFree(package->symbols);
    ArenaFree(&package->arena);
    ArenaFree(&package->diagnostics.arena);
    memset(package, 0, sizeof *package);
    package->scope = pushScope(package, compiler.builtin_package.scope);
}

void test_checkConstantDeclarations() {
	Package package = {0};
    Stmt *stmt = reset_parse_check_upto_last(&package, "x :: 8");

    CheckerContext ctx = { package.scope };
    checkStmt(stmt, &ctx, &package);
    ASSERT(ctx.mode != ExprMode_Unresolved);

    Symbol *sym = Lookup(package.scope, StrIntern("x"));
    ASSERT(sym);
    ASSERT(IsInteger(sym->type));
    ASSERT(sym->state == SymbolState_Resolved);
    ASSERT(!sym->used);
    ASSERT(sym->kind == SymbolKindConstant);
    ASSERT(sym->decl->pos.offset == 0);
    ASSERT(sym->val.u64 == 8);
}

#define RESET_CONTEXT(_CTX) \
memset(((u8*) &_CTX) + sizeof(_CTX.scope), 0, sizeof(_CTX) - sizeof(_CTX.scope)); \
memcpy((u8*) &ctx.scope, &package.scope, sizeof(package.scope));

void test_coercionsAreMarked() {
    Package package = {0};

    //                                                  1   2     3 5 4
    Stmt *stmt = reset_parse_check_upto_last(&package, "x : u64 : 1 + 2");
    CheckerContext ctx = {package.scope};
    checkStmt(stmt, &ctx, &package);
    CheckerInfo *info = CheckerInfoForStmt(&package, stmt);

    ASSERT(info->Constant.symbol->name == StrIntern("x"));
    ASSERT(package.astIdCount == 6);
    Conversion coerce = package.checkerInfo[5].BasicExpr.coerce;
    ASSERT(coerce & ConversionKindSame);
    ASSERT(coerce & ConversionFlag_Extend);

    reset_package(&package);
}

void test_checkTypeFunction() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeFunction(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeFunction((Expr *) stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->BasicExpr

    checkTypeFunction("fn () -> void");
    ASSERT(type->kind == TypeKindFunction);
    ASSERT(info.type == type);
    ASSERT(type->Function.numParams == 0);
    ASSERT(type->Function.numResults == 0);

    checkTypeFunction("fn (u8, u8, u8, u8) -> (u8, u8, u8, u8)");
    ASSERT(type->kind == TypeKindFunction);
    ASSERT(info.type == type);
    ASSERT(type->Function.numParams == 4);
    ASSERT(type->Function.numResults == 4);
}

void test_checkTypePointer() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypePointer(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypePointer((Expr *) stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->BasicExpr

    checkTypePointer("*u8");
    ASSERT(type->kind == TypeKindPointer);
    ASSERT(info.type == type);
}

void test_checkTypeArray() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeArray(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeArray((Expr *) stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->BasicExpr

    checkTypeArray("[30]u8");
    ASSERT(type->kind == TypeKindArray);
    ASSERT(type->Width == 30 * 8);
    ASSERT(type->Array.length == 30);
    ASSERT(type->Array.elementType == U8Type);
    ASSERT(type->Array.Flags == TypeFlag_None);
    ASSERT(info.type == type);

    checkTypeArray("[1]void");
    ASSERT(package.diagnostics.errors);

    // TODO: Implicitly sized array's should error without context.
}

void test_checkTypeSlice() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeSlice(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeSlice((Expr *) stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->BasicExpr

    checkTypeSlice("[]u8");
    ASSERT(type->kind == TypeKindSlice);
    ASSERT(type->Slice.elementType == U8Type);
    ASSERT(type->Slice.Flags == TypeFlag_None);
    ASSERT(info.type == type);

    checkTypeSlice("[]void");
    ASSERT(package.diagnostics.errors);
}

void test_checkTypeStruct() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeStruct(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeStruct((Expr *) stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->BasicExpr

    checkTypeStruct("struct {}");
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Struct.numMembers == 0);
    ASSERT(!package.diagnostics.errors);

    checkTypeStruct("struct {a: u8}");
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Struct.members[0].type == U8Type);
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 8);
    ASSERT(type->Width == 8);
    ASSERT(info.type == type);

    checkTypeStruct("struct {a: u8; b: u16}");
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Struct.members[0].type == U8Type);
    ASSERT(type->Struct.members[1].type == U16Type);
    ASSERT_MSG(type->Struct.members[1].offset == 16, "Fields should be aligned to at least their size");
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 16);
    ASSERT(type->Width == 32);
    ASSERT(info.type == type);

    checkTypeStruct("struct {a: u8; b: u8; b: u16; c: u32; d: u32}");
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Struct.members[0].type == U8Type);
    ASSERT(type->Struct.members[1].type == U8Type);
    ASSERT(type->Struct.members[2].type == U16Type);
    ASSERT(type->Struct.members[3].type == U32Type);
    ASSERT(type->Struct.members[4].type == U32Type);

    ASSERT_MSG(type->Struct.members[0].offset == 0, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[1].offset == 8, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[2].offset == 16, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[3].offset == 32, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[4].offset == 64, "Fields should be aligned to at least their size");
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 32);
    ASSERT(type->Width == 96);
    ASSERT(info.type == type);
}

void test_checkConstantUnaryExpressions() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
#define checkUnary(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprUnary((Expr *) stmt, &ctx, &package)

    checkUnary("-100");
    ASSERT(type == I8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == -100);

    checkUnary("!false");
    ASSERT(type == BoolType);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.b32 == true);

    checkUnary("!!false");
    ASSERT(type == BoolType);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.b32 == false);

    checkUnary("~0xffff");
    ASSERT_MSG(type == U8Type, "Expected a u8 type to represent the value 0");
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u64 == 0);

#undef checkUnary
}

void test_checkConstantBinaryExpressions() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
#define checkBinary(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprBinary((Expr *) stmt, &ctx, &package)

    checkBinary("1 + 2");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u64 == 3);

    checkBinary("1 + 2.0");
    ASSERT(type == F64Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.f64 == 3.0);

    checkBinary("1 + 2.0 - 3");
    ASSERT(type == F64Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.f64 == 0.f);

    checkBinary("1 / 0");
    ASSERT(ArrayLen(package.diagnostics.errors) == 1);
    ArrayFree(package.diagnostics.errors);

    checkBinary("-1 + -8");
    ASSERT(type == I8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == -9);

    checkBinary("255 + 255");
    ASSERT(type == U16Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u16 == 510);
}

void test_checkConstantTernaryExpression() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
#define checkTernary(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTernary((Expr *) stmt, &ctx, &package)

    checkTernary("true ? 1 : 2");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == 1);

    checkTernary("false ? 1.5 : 2.5");
    ASSERT(type == F64Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.f64 == 2.5);

    checkTernary("0 ? 1 ? 2 : 3 : 4");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == 4);

    checkTernary("1 ? 1 ? 2 : 3 : 4");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == 2);

    checkTernary("false ? 100000 : 1");
    ASSERT(type == U8Type);

    // NOTE: This would have a different type condition wasn't a constant
    checkTernary("rawptr(nil) ?: 250");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u64 == 250);
}

void test_checkConstantCastExpression() {
    Package package = {0};

    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
#define checkCastUsingCallSyntax(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprCall((Expr *) stmt, &ctx, &package)

    checkCastUsingCallSyntax("i64(8)");
    ASSERT(type == I64Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == 8);

    checkCastUsingCallSyntax("u8(100000000000042)");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u64 == 42);

#define checkCast(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprCast((Expr *) stmt, &ctx, &package)

    checkCast("cast(i64) 8");
    ASSERT(type == I64Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == 8);

    checkCast("cast(u8) 100000000000042");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u64 == 42);
}

void test_callToCVargs() {
    Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;

#define checkCall(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprCall((Expr *) stmt, &ctx, &package)

    checkCall("#foreign libc\n"
              "printf :: fn(rawptr, #cvargs ..any) -> void;"
              "printf(\"%d %d %d\", 1, 2, 3);");
    ASSERT(!package.diagnostics.errors);
}

void test_checkExprSelector() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_Selector info;

#define checkSelector(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprSelector((Expr *) stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->Selector

    checkSelector("Foo :: struct {a: u8; b: u16};"
                  "foo := Foo{};"
                  "foo.b;");
    ASSERT(type == U16Type);
    ASSERT(type == info.type);
}

void test_checkExprLitInteger() {
	Package package = {0};
    Expr *expr;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo_BasicExpr info;

#define checkInteger(_CODE) \
expr = (Expr *) reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprLitInt(expr, &ctx, &package); \
info = GetExprInfo(&package, expr)->BasicExpr

    checkInteger("5");
    ASSERT(type == U8Type);
    ASSERT(info.val.u64 == 5);
}

Type *typeFromParsing(Package *package, const char *code) {
    reset_package(package);
    Source source = {":test:", ":test:", code};
    package->current_source = &source;
    parse_test_package(package);
    ASSERT(compiler.checking_queue.size == 1);
    CheckerContext ctx = { package->scope };
    return checkExpr((Expr *) package->stmts[0], &ctx, package);
}

void test_checkExprLitFunction() {
	Package package = {0};
    Expr *expr;
    CheckerContext ctx = { package.scope };
    Type *type;

#define checkFunction(_CODE) \
expr = (Expr *) reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprLitFunction(expr, &ctx, &package);

    checkFunction("fn (a: u64) -> u64 { return a }");
    ASSERT(type == typeFromParsing(&package, "fn(u64) -> u64"));

    checkFunction("fn (a, b: u64) -> u64, bool { return a }");
    ASSERT(type == typeFromParsing(&package, "fn(u64, u64) -> u64, bool"));

    checkFunction("fn (fmt: *u8, args: ..any) -> i32 { return 0 }");
    ASSERT(type == typeFromParsing(&package, "fn (fmt: *u8, args: ..any) -> i32"));
}

void test_checkExprLitCompound() {
	Package package = {0};
    Expr *expr;
    CheckerContext ctx = { package.scope };
    Type *type;
    CheckerInfo *info;

#define checkCompound(_CODE) \
expr = (Expr *) reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
type = checkExprLitCompound(expr, &ctx, &package); \
info = CheckerInfoForExpr(&package, expr);

    checkCompound("[5]i8{1, 2, 3, 4, 5}");
    ASSERT(info->BasicExpr.type == type);
    ASSERT(type == typeFromParsing(&package, "[5]i8"));

    checkCompound("Foo :: struct {a: u8; b: u16};"
                  "Foo{};");
    ASSERT(info->BasicExpr.type == type);
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Symbol->name == StrIntern("Foo"));

    checkCompound("Foo :: struct {a: u8; b: u16};"
                  "Foo{a: 4, b: 89};");
    ASSERT(info->BasicExpr.type == type);
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Symbol->name == StrIntern("Foo"));

    // TODO: Implicitely sized array's should be the size of their maxIndex (not just the number of elements)
    //    checkCompound("[..]u8{1, 2, 3, [9]: 4, 5}");
    //    ASSERT(info->BasicExpr.type == type);
    //    ASSERT(type == typeFromParsing("[10]u8"));

    Stmt *stmt = reset_parse_check_upto_last(&package, "Foo :: struct {a: u8; b: u16};"
                                                       "foo : Foo = {};");
    RESET_CONTEXT(ctx);
    checkStmt(stmt, &ctx, &package);
    expr = stmt->Variable.values[0];
    info = CheckerInfoForExpr(&package, expr);
    type = TypeFromCheckerInfo(*info);
    ASSERT(type->kind == TypeKindStruct);
    ASSERT(type->Symbol->name == StrIntern("Foo"));
}

void test_checkStmtAssign() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };

#define checkAssign(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtAssign(stmt, &ctx, &package)

    checkAssign("x := 1;"
                "x  = 2;");

    checkAssign("x, y := 1, 2;"
                "x, y  = y, x;");
}

void test_checkStmtBlock() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };

#define checkBlock(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtBlock(stmt, &ctx, &package)

    checkBlock("{" "\n"
               "}" "\n");
    ASSERT(!package.diagnostics.errors);
}

void test_checkStmtDefer() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };

#define checkDefer(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtDefer(stmt, &ctx, &package)

    checkDefer("defer { }");
    ASSERT(!package.diagnostics.errors);
}

void test_checkStmtFor() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    CheckerInfo_For info;

#define checkFor(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtFor(stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->For

    checkFor("for { }");
    ASSERT(!package.diagnostics.errors);
    ASSERT(info.breakTarget);
    ASSERT(info.continueTarget);

    checkFor("for 1 < 2 { }");
    ASSERT(!package.diagnostics.errors);

    checkFor("for x := 0; x < 10; x += 1 { }");
    ASSERT(!package.diagnostics.errors);
}

void test_checkStmtForIn() {
//    Package package = {0};
//    Stmt *stmt;
//    CheckerContext ctx = { package.scope };
//    CheckerInfo_For info;

#define checkForIn(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtForIn(stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->For

    // TODO: Need to implement array types
//    checkForIn("arr: []u8;"
//               "for el in arr { };");
//    ASSERT(!package.diagnostics.errors);
//    ASSERT(info.breakTarget);
//    ASSERT(info.continueTarget);
}

void test_checkStmtGoto() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    CheckerInfo_For info;

#define checkFor(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtFor(stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->For

    checkFor("for { continue }");
    ASSERT(!package.diagnostics.errors);

    checkFor("for { break }");
    ASSERT(!package.diagnostics.errors);

    // TODO: Goto & fallthrough
}

void test_checkStmtIf() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };

#define checkIf(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtIf(stmt, &ctx, &package)

    checkIf("if true {}");
    ASSERT(!package.diagnostics.errors);

    checkIf("if true { } else { }");
    ASSERT(!package.diagnostics.errors);
}

void test_checkStmtLabel() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    CheckerInfo_Label info;

#define checkLabel(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtLabel(stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->Label

    checkLabel("error:");
    ASSERT(!package.diagnostics.errors);
    ASSERT(info.symbol);
    ASSERT(info.symbol->kind == SymbolKindLabel);
}

void test_checkStmtReturn() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };

#define checkReturn(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
checkStmtReturn(stmt, &ctx, &package); \
RESET_CONTEXT(ctx)

    Type *types[3] = {I64Type, I64Type, F64Type};
    Type type = {TypeKindTuple, .Tuple = {TypeFlag_None, types, 1}};

    ctx.desiredType = &type;
    checkReturn("return 42");
    ASSERT(!package.diagnostics.errors);

    type.Tuple.numTypes = 3;
    ctx.desiredType = &type;
    checkReturn("return 1, 2, 6.28");
    ASSERT(!package.diagnostics.errors);
}

void test_checkStmtSwitch() {
	Package package = {0};
    Stmt *stmt;
    CheckerContext ctx = { package.scope };
    CheckerInfo_Switch info;

#define checkSwitch(_CODE) \
stmt = reset_parse_check_upto_last(&package, _CODE); \
RESET_CONTEXT(ctx); \
checkStmtSwitch(stmt, &ctx, &package); \
info = GetStmtInfo(&package, stmt)->Switch

    checkSwitch("switch {}");
    ASSERT(!package.diagnostics.errors);
    ASSERT(info.breakTarget);
    ASSERT(info.breakTarget->kind == SymbolKindLabel);

    checkSwitch("switch 8 {"            "\n"
                "case 1:"               "\n"
                "case 2: fallthrough"   "\n"
                "case: break"           "\n"
                "}");
    ASSERT(!package.diagnostics.errors);
    ASSERT(info.breakTarget);
    ASSERT(info.breakTarget->kind == SymbolKindLabel);
}

#undef pkg
#endif
