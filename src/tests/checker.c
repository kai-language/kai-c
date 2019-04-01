
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

//#define pkg checkerTestPackage
Package pkg = {0};
Queue resetAndParse(const char *code) {
    // reset package
    ArrayFree(pkg.diagnostics.errors);
    ArrayFree(pkg.stmts);
    ArrayFree(pkg.symbols);

    ArenaFree(&pkg.arena);
    ArenaFree(&pkg.diagnostics.arena);
    memset(&pkg, 0, sizeof(Package));

    pkg.scope = pushScope(&pkg, builtinPackage.scope);

    parseSourceCode(&pkg, code);
    return compiler.checking_queue;
}

Stmt *resetAndParseReturningLastStmt(const char *code) {
    Queue queue = resetAndParse(code);
    ASSERT(queue.size > 0);
    while (queue.size > 1) {
        CheckerWork *work = QueuePopFront(&queue);
        CheckerContext ctx = { work->package->scope };
        checkStmt(work->stmt, &ctx, work->package);
    }
    CheckerWork *work = QueuePopFront(&queue);
    ASSERT(work);
    Stmt *stmt = work->stmt;
    ArenaFree(&queue.arena);
    return stmt;
}

void test_checkConstantDeclarations() {
    InitTestCompiler(&compiler, NULL);
    Queue queue = resetAndParse("x :: 8");

    CheckerWork *work = QueuePopFront(&queue);

    ASSERT(queue.size == 0);

    Stmt *stmt = work->stmt;
    CheckerContext ctx = { pkg.scope };
    checkStmt(stmt, &ctx, &pkg);
    ASSERT(ctx.mode != ExprMode_Unresolved);

    Symbol *sym = Lookup(pkg.scope, StrIntern("x"));
    ASSERT(sym);
    ASSERT(IsInteger(sym->type));
    ASSERT(sym->state == SymbolState_Resolved);
    ASSERT(!sym->used);
    ASSERT(sym->kind == SymbolKind_Constant);
    ASSERT(sym->decl->pos.offset == 0);
    ASSERT(sym->val.u64 == 8);
}

#define RESET_CONTEXT(_CTX) \
memset(((u8*) &_CTX) + sizeof(_CTX.scope), 0, sizeof(_CTX) - sizeof(_CTX.scope)); \
memcpy((u8*) &ctx.scope, &pkg.scope, sizeof(pkg.scope));


void test_coercionsAreMarked() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerInfo* info;
    CheckerContext ctx = { pkg.scope };
#define checkBasicExpr(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmt(stmt, &ctx, &pkg); \
info = CheckerInfoForStmt(&pkg, stmt)

    //              1   2     3 5 4
    checkBasicExpr("x : u64 : 1 + 2");

    ASSERT(info->Constant.symbol->name == StrIntern("x"));
    ASSERT_MSG(pkg.astIdCount == 6, "Package was not fully reset as expected");
    Conversion coerce = pkg.checkerInfo[5].BasicExpr.coerce;
    ASSERT(coerce & ConversionKind_Same);
    ASSERT(coerce & ConversionFlag_Extend);
}

void test_checkTypeFunction() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeFunction(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeFunction((Expr *) stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->BasicExpr

    checkTypeFunction("fn () -> void");
    ASSERT(type->kind == TypeKind_Function);
    ASSERT(info.type == type);
    ASSERT(type->Function.numParams == 0);
    ASSERT(type->Function.numResults == 0);

    checkTypeFunction("fn (u8, u8, u8, u8) -> (u8, u8, u8, u8)");
    ASSERT(type->kind == TypeKind_Function);
    ASSERT(info.type == type);
    ASSERT(type->Function.numParams == 4);
    ASSERT(type->Function.numResults == 4);
}

void test_checkTypePointer() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypePointer(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypePointer((Expr *) stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->BasicExpr

    checkTypePointer("*u8");
    ASSERT(type->kind == TypeKind_Pointer);
    ASSERT(info.type == type);
}

void test_checkTypeArray() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeArray(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeArray((Expr *) stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->BasicExpr

    checkTypeArray("[30]u8");
    ASSERT(type->kind == TypeKind_Array);
    ASSERT(type->Width == 30 * 8);
    ASSERT(type->Array.length == 30);
    ASSERT(type->Array.elementType == U8Type);
    ASSERT(type->Array.Flags == TypeFlag_None);
    ASSERT(info.type == type);

    checkTypeArray("[1]void");
    ASSERT(pkg.diagnostics.errors);

    // TODO: Implicitly sized array's should error without context.
}

void test_checkTypeSlice() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeSlice(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeSlice((Expr *) stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->BasicExpr

    checkTypeSlice("[]u8");
    ASSERT(type->kind == TypeKind_Slice);
    ASSERT(type->Slice.elementType == U8Type);
    ASSERT(type->Slice.Flags == TypeFlag_None);
    ASSERT(info.type == type);

    checkTypeSlice("[]void");
    ASSERT(pkg.diagnostics.errors);
}

void test_checkTypeStruct() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_BasicExpr info;
#define checkTypeStruct(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTypeStruct((Expr *) stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->BasicExpr

    checkTypeStruct("struct {}");
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Struct.numMembers == 0);
    ASSERT(!pkg.diagnostics.errors);

    checkTypeStruct("struct {a: u8}");
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Struct.members[0].type == U8Type);
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 8);
    ASSERT(type->Width == 8);
    ASSERT(info.type == type);

    checkTypeStruct("struct {a: u8; b: u16}");
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Struct.members[0].type == U8Type);
    ASSERT(type->Struct.members[1].type == U16Type);
    ASSERT_MSG(type->Struct.members[1].offset == 16, "Fields should be aligned to at least their size");
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 16);
    ASSERT(type->Width == 32);
    ASSERT(info.type == type);

    checkTypeStruct("struct {a: u8; b: u8; b: u16; c: u32; d: u32}");
    ASSERT(type->kind == TypeKind_Struct);
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
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
#define checkUnary(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprUnary((Expr *) stmt, &ctx, &pkg)

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
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
#define checkBinary(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprBinary((Expr *) stmt, &ctx, &pkg)

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
    ASSERT(ArrayLen(pkg.diagnostics.errors) == 1);
    ArrayFree(pkg.diagnostics.errors);

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
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
#define checkTernary(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprTernary((Expr *) stmt, &ctx, &pkg)

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
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
#define checkCastUsingCallSyntax(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprCall((Expr *) stmt, &ctx, &pkg)

    checkCastUsingCallSyntax("i64(8)");
    ASSERT(type == I64Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.i64 == 8);

    checkCastUsingCallSyntax("u8(100000000000042)");
    ASSERT(type == U8Type);
    ASSERT(IsConstant(&ctx));
    ASSERT(ctx.val.u64 == 42);

#define checkCast(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprCast((Expr *) stmt, &ctx, &pkg)

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
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;

#define checkCall(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprCall((Expr *) stmt, &ctx, &pkg)

    checkCall("#foreign libc\n"
              "printf :: fn(rawptr, #cvargs ..any) -> void;"
              "printf(\"%d %d %d\", 1, 2, 3);");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkExprSelector() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_Selector info;

#define checkSelector(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprSelector((Expr *) stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->Selector

    checkSelector("Foo :: struct {a: u8; b: u16};"
                  "foo := Foo{};"
                  "foo.b;");
    ASSERT(type == U16Type);
    ASSERT(type == info.type);
}

Type *typeFromParsing(const char *code) {
    pkg.scope = pushScope(&pkg, builtinPackage.scope);

    Stmt *stmt = resetAndParseReturningLastStmt(code);
    CheckerContext ctx = { pkg.scope };
    return checkExpr((Expr *) stmt, &ctx, &pkg);
}

void test_checkExprLitInteger() {
    InitTestCompiler(&compiler, NULL);
    Expr *expr;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo_BasicExpr info;

#define checkInteger(_CODE) \
expr = (Expr *) resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprLitInt(expr, &ctx, &pkg); \
info = GetExprInfo(&pkg, expr)->BasicExpr

    checkInteger("5");
    ASSERT(type == U8Type);
    ASSERT(info.val.u64 == 5);
}

void test_checkExprLitFunction() {
    InitTestCompiler(&compiler, NULL);
    Expr *expr;
    CheckerContext ctx = { pkg.scope };
    Type *type;

#define checkFunction(_CODE) \
expr = (Expr *) resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprLitFunction(expr, &ctx, &pkg);

    checkFunction("fn (a: u64) -> u64 { return a }");
    ASSERT(type == typeFromParsing("fn(u64) -> u64"));

    checkFunction("fn (a, b: u64) -> u64, bool { return a }");
    ASSERT(type == typeFromParsing("fn(u64, u64) -> u64, bool"));

    checkFunction("fn (fmt: *u8, args: ..any) -> i32 { return 0 }");
    ASSERT(type == typeFromParsing("fn (fmt: *u8, args: ..any) -> i32"));
}

void test_checkExprLitCompound() {
    InitTestCompiler(&compiler, NULL);
    Expr *expr;
    CheckerContext ctx = { pkg.scope };
    Type *type;
    CheckerInfo *info;

#define checkCompound(_CODE) \
expr = (Expr *) resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
type = checkExprLitCompound(expr, &ctx, &pkg); \
info = CheckerInfoForExpr(&pkg, expr);

    checkCompound("[5]i8{1, 2, 3, 4, 5}");
    ASSERT(info->BasicExpr.type == type);
    ASSERT(type == typeFromParsing("[5]i8"));

    checkCompound("Foo :: struct {a: u8; b: u16};"
                  "Foo{};");
    ASSERT(info->BasicExpr.type == type);
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Symbol->name == StrIntern("Foo"));

    checkCompound("Foo :: struct {a: u8; b: u16};"
                  "Foo{a: 4, b: 89};");
    ASSERT(info->BasicExpr.type == type);
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Symbol->name == StrIntern("Foo"));

    // TODO: Implicitely sized array's should be the size of their maxIndex (not just the number of elements)
    //    checkCompound("[..]u8{1, 2, 3, [9]: 4, 5}");
    //    ASSERT(info->BasicExpr.type == type);
    //    ASSERT(type == typeFromParsing("[10]u8"));

    Stmt *stmt = resetAndParseReturningLastStmt("Foo :: struct {a: u8; b: u16};"
                                                "foo : Foo = {};");
    RESET_CONTEXT(ctx);
    checkStmt(stmt, &ctx, &pkg);
    expr = stmt->Variable.values[0];
    info = CheckerInfoForExpr(&pkg, expr);
    type = TypeFromCheckerInfo(*info);
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Symbol->name == StrIntern("Foo"));
}

void test_checkStmtAssign() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };

#define checkAssign(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtAssign(stmt, &ctx, &pkg)

    checkAssign("x := 1;"
                "x  = 2;");

    checkAssign("x, y := 1, 2;"
                "x, y  = y, x;");
}

void test_checkStmtBlock() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };

#define checkBlock(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtBlock(stmt, &ctx, &pkg)

    checkBlock("{" "\n"
               "}" "\n");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkStmtDefer() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };

#define checkDefer(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtDefer(stmt, &ctx, &pkg)

    checkDefer("defer { }");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkStmtFor() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    CheckerInfo_For info;

#define checkFor(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtFor(stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->For

    checkFor("for { }");
    ASSERT(!pkg.diagnostics.errors);
    ASSERT(info.breakTarget);
    ASSERT(info.continueTarget);

    checkFor("for 1 < 2 { }");
    ASSERT(!pkg.diagnostics.errors);

    checkFor("for x := 0; x < 10; x += 1 { }");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkStmtForIn() {
    //    InitTestCompiler(&compiler, NULL);
    //    Stmt *stmt;
    //    CheckerContext ctx = { pkg.scope };
    //    CheckerInfo_For info;

#define checkForIn(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtForIn(stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->For

    // TODO: Need to implement array types
    //    checkForIn("arr: []u8" "\n"
    //             "for el in arr { }");
    //    ASSERT(!pkg.diagnostics.errors);
    //    ASSERT(info.breakTarget);
    //    ASSERT(info.continueTarget);
}

void test_checkStmtGoto() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    CheckerInfo_For info;

#define checkFor(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtFor(stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->For

    checkFor("for { continue }");
    ASSERT(!pkg.diagnostics.errors);

    checkFor("for { break }");
    ASSERT(!pkg.diagnostics.errors);

    // TODO: Goto & fallthrough
}

void test_checkStmtIf() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };

#define checkIf(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtIf(stmt, &ctx, &pkg)

    checkIf("if true {}");
    ASSERT(!pkg.diagnostics.errors);

    checkIf("if true { } else { }");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkStmtLabel() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    CheckerInfo_Label info;

#define checkLabel(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtLabel(stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->Label

    checkLabel("error:");
    ASSERT(!pkg.diagnostics.errors);
    ASSERT(info.symbol);
    ASSERT(info.symbol->kind == SymbolKind_Label);
}

void test_checkStmtReturn() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };

#define checkReturn(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
checkStmtReturn(stmt, &ctx, &pkg); \
RESET_CONTEXT(ctx)

    Type *types[3] = {I64Type, I64Type, F64Type};
    Type type = {TypeKind_Tuple, .Tuple = {TypeFlag_None, types, 1}};

    ctx.desiredType = &type;
    checkReturn("return 42");
    ASSERT(!pkg.diagnostics.errors);

    type.Tuple.numTypes = 3;
    ctx.desiredType = &type;
    checkReturn("return 1, 2, 6.28");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkStmtSwitch() {
    InitTestCompiler(&compiler, NULL);
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    CheckerInfo_Switch info;

#define checkSwitch(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
RESET_CONTEXT(ctx); \
checkStmtSwitch(stmt, &ctx, &pkg); \
info = GetStmtInfo(&pkg, stmt)->Switch

    checkSwitch("switch {}");
    ASSERT(!pkg.diagnostics.errors);
    ASSERT(info.breakTarget);
    ASSERT(info.breakTarget->kind == SymbolKind_Label);

    checkSwitch("switch 8 {"            "\n"
                "case 1:"               "\n"
                "case 2: fallthrough"   "\n"
                "case: break"           "\n"
                "}");
    ASSERT(!pkg.diagnostics.errors);
    ASSERT(info.breakTarget);
    ASSERT(info.breakTarget->kind == SymbolKind_Label);
}

#undef pkg
#endif
