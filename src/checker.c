
typedef enum ExprMode {
    ExprMode_Invalid,
    ExprMode_Unresolved,
    ExprMode_Computed,
    ExprMode_Assignable,
    ExprMode_Addressable,
    ExprMode_Nil,
    ExprMode_File,
    ExprMode_Library,
    ExprMode_Type,
    ExprMode_Function,
} ExprMode;

typedef struct CheckerContext CheckerContext;
struct CheckerContext {
    Type *desiredType;
    Scope *scope;
    ExprMode mode;
    b8 isConstant;
    Val val;
};

void markSymbolInvalid(Symbol *symbol) {
    if (symbol) {
        symbol->state = SymbolState_Resolved;
        symbol->kind = SymbolKind_Invalid;
    }
}

Inline
void markSymbolResolved(Symbol *symbol, Type *type) {
    symbol->state = SymbolState_Resolved;
    symbol->type = type;
}

Inline
CheckerInfo *GetStmtInfo(Package *pkg, Stmt *stmt) {
    ASSERT(DoesStmtKindAllocateTypeInfo[stmt->kind]);
    return &pkg->checkerInfo[stmt->id];
}

Inline
CheckerInfo *GetCheckerContext(Package *pkg, Expr *expr) {
    return GetStmtInfo(pkg, (Stmt *) expr);
}

Inline
CheckerInfo *GetDeclInfo(Package *pkg, Decl *decl) {
    return GetStmtInfo(pkg, (Stmt *) decl);
}

void storeInfoConstant(Package *pkg, Decl *decl, Symbol *symbol) {
    ASSERT(decl->kind == DeclKind_Constant);
    CheckerInfo info = {CheckerInfoKind_Constant, .Constant.symbol = symbol};
    pkg->checkerInfo[decl->id] = info;
}

void storeInfoVariable(Package *pkg, Decl *decl, Symbol **symbols) {
    ASSERT(decl->kind == DeclKind_Variable);
    CheckerInfo info = {CheckerInfoKind_Variable, .Variable.symbols = symbols};
    pkg->checkerInfo[decl->id] = info;
}

void storeInfoIdent(Package *pkg, Expr *expr, Symbol *symbol) {
    ASSERT(expr->kind == ExprKind_Ident);
    CheckerInfo info = {CheckerInfoKind_Ident, .Ident.symbol = symbol};
    pkg->checkerInfo[expr->id] = info;
}

void storeInfoBasicExpr(Package *pkg, Expr *expr, Type *type, CheckerContext *ctx) {
    CheckerInfo info = {CheckerInfoKind_BasicExpr, .BasicExpr.type = type};
    info.BasicExpr.isConstant = ctx->isConstant;
    info.BasicExpr.val = ctx->val;
    pkg->checkerInfo[expr->id] = info;
}

CheckerInfo *CheckerInfoForExpr(Package *pkg, Expr *expr) {
    return &pkg->checkerInfo[expr->id];
}

CheckerInfo *CheckerInfoForStmt(Package *pkg, Stmt *stmt) {
    return &pkg->checkerInfo[stmt->id];
}

CheckerInfo *CheckerInfoForDecl(Package *pkg, Decl *decl) {
    return &pkg->checkerInfo[decl->id];
}

b32 declareSymbol(Package *pkg, Scope *scope, const char *name, Symbol **symbol, u64 declId, Decl *decl) {
    Symbol *old = Lookup(scope, name);
    if (old) {
        ReportError(pkg, RedefinitionError, decl->start, "Duplicate definition of symbol %s", name);
        ReportNote(pkg, old->decl->start, "Previous definition of %s", name);
        *symbol = old;
        return true;
    }

    Symbol *sym = ArenaAlloc(&pkg->arena, sizeof(Symbol));
    sym->name = name;
    sym->kind = SymbolKind_Invalid;
    sym->state = SymbolState_Resolving;
    sym->decl = decl;

    MapSet(&scope->members, name, sym);

    *symbol = sym;
    
    return false;
}

b32 expectType(Package *pkg, Type *type, CheckerContext *info, Position pos) {
    if (info->mode != ExprMode_Type) {
        ReportError(pkg, NotATypeError, pos, "%s cannot be used as a type", DescribeTypeKind(type->kind));
    }
    return info->mode == ExprMode_Type;
}

b32 IsInteger(Type *type) {
    return type->kind == TypeKind_Int && ((type->Flags & TypeFlag_Boolean) == 0);
}

b32 IsSigned(Type *type) {
    ASSERT_MSG(IsInteger(type), "IsSigned should only be called for integers. Try `IsInteger(type) && IsSigned(type)`");
    return (type->Flags & TypeFlag_Signed) != 0;
}

b32 IsFloat(Type *type) {
    return type->kind == TypeKind_Float;
}

b32 isNumeric(Type *type) {
    return IsInteger(type) || IsFloat(type);
}

b32 isBoolean(Type *type) {
    return type->kind == TypeKind_Int && (type->Flags & TypeFlag_Boolean) != 0;
}

b32 isPointer(Type *type) {
    return type->kind == TypeKind_Pointer;
}

b32 isNilable(Type *type) {
    return type->kind == TypeKind_Pointer;
}

b32 isIntegerOrPointer(Type *type) {
    return IsInteger(type) || isPointer(type);
}

b32 isNumericOrPointer(Type *type) {
    return isNumeric(type) || isPointer(type);
}

b32 canBeUsedForLogical(Type *type) {
    return isBoolean(type) || IsInteger(type) || isPointer(type);
}

b32 isEnum(Type *type) {
    return type->kind == TypeKind_Enum;
}

b32 isEnumFlags(Type *type) {
    return isEnum(type) && (type->Flags & TypeFlag_EnumFlags) != 0;
}

b32 isAlias(Type *type) {
    return (type->Flags & TypeFlag_Alias) != 0;
}

b32 canBeUsedForBitwise(Type *type) {
    return IsInteger(type) || isEnumFlags(type);
}

b32 isComparable(Type *type) {
    return IsInteger(type) || IsFloat(type) || isEnum(type);
}

b32 isEquatable(Type *type) {
    static b32 equatables[NUM_TYPE_KINDS] = {
        [TypeKind_Int] = true,
        [TypeKind_Float] = true,
        [TypeKind_Pointer] = true,
        [TypeKind_Enum] = true,
    };
    return equatables[type->kind];
}

#include "constant_eval.c"

b32 (*unaryPredicates[NUM_TOKEN_KINDS])(Type *) = {
    [TK_Add] = isNumeric,
    [TK_Sub] = isNumeric,
    [TK_BNot] = IsInteger,
    [TK_Not] = canBeUsedForLogical,
    [TK_Lss] = isPointer,
};

b32 (*binaryPredicates[NUM_TOKEN_KINDS])(Type *) = {
    [TK_Add] = isNumeric,
    [TK_Sub] = isNumeric,
    [TK_Mul] = isNumeric,
    [TK_Div] = isNumeric,
    [TK_Rem] = IsInteger,

    [TK_And] = canBeUsedForBitwise,
    [TK_Or]  = canBeUsedForBitwise,
    [TK_Xor] = canBeUsedForBitwise,
    [TK_Shl] = canBeUsedForBitwise,
    [TK_Shr] = canBeUsedForBitwise,

    [TK_Eql] = isEquatable,
    [TK_Neq] = isEquatable,

    [TK_Lss] = isComparable,
    [TK_Gtr] = isComparable,
    [TK_Leq] = isComparable,
    [TK_Geq] = isComparable,

    [TK_Land] = canBeUsedForLogical,
    [TK_Lor]  = canBeUsedForLogical,
};

b32 canCoerce(Type *type, Type *target, CheckerContext *info) {
    if (TypesIdentical(type, target)) return true;
    if (target->kind == TypeKind_Any) return true;

    // Any numeric or pointer type can coerce to Bool type
    if (isNumericOrPointer(type) && isBoolean(target)) return true;

    if (IsInteger(target) && IsInteger(type)) {
        if (IsSigned(type) && !IsSigned(target)) {
            // Signed to Unsigned
            // Source is a positive constant less than Target's Max Value
            if (info->isConstant && info->val.u64 <= MaxValueForIntOrPointerType(target)) {
                return SignExtend(type, target, info->val) >= 0;
            }
            // Coercion from a signed integer to an unsigned requires a cast.
            return false;
        }

        if (!IsSigned(type) && IsSigned(target)) {
            // Unsigned to Signed
            // Source is a constant less than Target's Max Value
            if (info->isConstant && info->val.u64 <= MaxValueForIntOrPointerType(target)) return true;
            // Target's Max Value is larger than the Source's
            return type->Width < target->Width;
        }

        // The two integer types have the same signedness. The target must be the same or a larger size.
        return type->Width <= target->Width;
    }

    if (IsFloat(type)) {
        // Coercion between float types requires the target is larger or equal in size.
        return IsFloat(target) && type->Width <= target->Width;
    }
    if (IsFloat(target)) {
        // Coercion to float type only requires the source type is numeric
        return isNumeric(type);
    }

    if (isPointer(type)) {
        // Coercion from any pointer type to rawptr is allowed
        return TypesIdentical(target, RawptrType) || TypesIdentical(type, RawptrType);
    }

    return false;
}

#if TEST
void test_canCoerce() {
    INIT_COMPILER();

    CheckerContext info = {0};

    Type *PtrToU8 = NewTypePointer(TypeFlag_None, U8Type);

    ASSERT(canCoerce(I8Type, AnyType, &info));
    ASSERT(canCoerce(PtrToU8, AnyType, &info));
    ASSERT(canCoerce(F32Type, AnyType, &info));

    ASSERT(canCoerce(I8Type, BoolType, &info));
    ASSERT(canCoerce(F32Type, BoolType, &info));
    ASSERT(canCoerce(PtrToU8, BoolType, &info));

    ASSERT(canCoerce(I64Type, F32Type, &info));

    ASSERT(canCoerce(I8Type, I16Type, &info));
    ASSERT(canCoerce(U8Type, I16Type, &info));

    ASSERT(canCoerce(UintType, UintptrType, &info));
    ASSERT(!canCoerce(I8Type, U64Type, &info));
    ASSERT(canCoerce(PtrToU8, RawptrType, &info));
    ASSERT(canCoerce(RawptrType, PtrToU8, &info));
    // TODO: Coercion to union
    // TODO: Coercion from enum

    info.isConstant = true;
    info.val.i8 = 100;
    ASSERT(canCoerce(I8Type, U8Type, &info));

    info.val.i8 = -100;
    ASSERT(!canCoerce(I8Type, U8Type, &info));

    info.val.u64 = 100;
    ASSERT(canCoerce(U64Type, I8Type, &info));
}
#endif

b32 representValueSilently(CheckerContext *info, Expr *expr, Type *type, Type *target, Package *pkg) {
    // TODO: Implement this properly
    // Also tests for it.
    return true;
}

Conversion conversion(Type *type, Type *target) {
    Conversion conversion = 0;
    if (type->kind == target->kind) {
        conversion |= ConversionClass_Same;
        switch (type->kind) {
            case TypeKind_Float:
                if (type->Width < target->Width) conversion |= ConversionFlag_Extend;
                return conversion;

            case TypeKind_Int:
                if (type->Width < target->Width) conversion |= ConversionFlag_Extend;
                if (IsSigned(type)) conversion |= ConversionFlag_Signed;
                return conversion;

            default:
                return conversion;
        }
    }

    if (isBoolean(target)) {
        conversion |= ConversionClass_Bool;
        return conversion;
    }

    // TODO: All to Union

    if (IsInteger(type) && IsFloat(target)) {
        conversion |= ConversionClass_FtoI;
        if (IsSigned(type)) conversion |= ConversionFlag_Signed;
        return conversion;
    }

    if (IsFloat(type) && IsInteger(target)) {
        conversion |= ConversionClass_FtoI;
        if (IsSigned(type)) conversion |= ConversionFlag_Signed;
        return conversion;
    }

    if (isPointer(type) && IsInteger(target)) {
        conversion |= ConversionClass_PtoI;
        return conversion;
    }

    if (IsInteger(target) && isPointer(type)) {
        conversion |= ConversionClass_ItoP;
        return conversion;
    }

    // TODO: Integer to enum and vica versa

    // TODO: function to pointer and vica versa

    PANIC("Unhandled or prohibited conversion");
}

void changeTypeOrMarkConversionForExpr(Expr *expr, Type *type, Type *target, Package *pkg) {

    switch (expr->kind) {
        case ExprKind_LitNil:
        case ExprKind_LitInt:
        case ExprKind_LitFloat:
        case ExprKind_LitString:
            CheckerInfoForExpr(pkg, expr)->BasicExpr.type = target;
            break;

        default:
            CheckerInfoForExpr(pkg, expr)->coerce = conversion(type, target);
    }
}

void changeTypeOrRecordCoercionIfNeeded(Type **type, Expr *expr, CheckerContext *ctx, b32 isConstantNegative, Package *pkg) {
    if (IsInteger(*type)) {
        Type *requiredType = NULL;
        if (isConstantNegative) {
            i64 val = SignExtendTo64Bits(*type, ctx->val);
            requiredType = SmallestIntTypeForNegativeValue(val);
        } else {
            requiredType = SmallestIntTypeForPositiveValue(ctx->val.u64);
        }

        if (requiredType != *type) {
            changeTypeOrMarkConversionForExpr(expr, *type, requiredType, pkg);
            *type = requiredType;
        }
    }
}

// TODO: Test this
void convertValue(Type *type, Type *target, Val *val) {
    if (isBoolean(target) && isNumericOrPointer(type)) {
        val->u64 = val->u64 != 0;
        return;
    }

    if (IsInteger(type) && IsSigned(type) && IsInteger(target) && IsSigned(target)) {
        val->i64 = SignExtend(type, target, *val);
        return;
    }

    if (IsInteger(type) && IsFloat(target)) {
        if (IsSigned(type)) {
            i64 v = SignExtend(type, target, *val);
            switch (target->Width) {
                case 32:
                    val->f32 = (f32) v;
                    break;
                case 64:
                    val->f64 = (f64) v;
                    break;
                default:
                    PANIC("Unhandled float type during constant conversion");
            }
        } else {
            switch (target->Width) {
                case 32:
                    val->f32 = (f32) val->u32;
                    break;
                case 64:
                    val->f64 = (f64) val->u64;
                    break;
                default:
                    PANIC("Unhandled float type during constant conversion");
            }
        }
    }
}

b32 coerceTypeSilently(Expr *expr, CheckerContext *info, Type **type, Type *target, Package *pkg) {
    if (*type == InvalidType || target == InvalidType) return false;
    if (TypesIdentical(*type, target)) return true;

    if (!canCoerce(*type, target, info)) return false;

    if (info->isConstant) convertValue(*type, target, &info->val);
    changeTypeOrMarkConversionForExpr(expr, *type, target, pkg);

    *type = target;

    return true;
}

b32 coerceType(Expr *expr, CheckerContext *info, Type **type, Type *target, Package *pkg) {

    b32 success = coerceTypeSilently(expr, info, type, target, pkg);
    if (!success) {
        ReportError(pkg, InvalidConversionError, expr->start,
                    "Cannot convert %s to type %s", DescribeExpr(expr), DescribeType(target));
        *type = InvalidType;
    }

    return success;
}

b32 canCast(Type *source, Type *target) {
    if (TypesIdentical(source, target)) return true;
    if (target->kind == TypeKind_Any) return true;

    // TODO: Union type

    if (isNumericOrPointer(source) && isBoolean(target)) return true;

    if (isNumeric(source) && isNumeric(target)) return true;

    if (isPointer(source)) {
        if (isPointer(target)) return true;
        if (TypesIdentical(target, IntptrType)) return true;
        if (TypesIdentical(target, UintptrType)) return true;
    }

    if (IsInteger(source) &&
        (TypesIdentical(target, IntptrType) || TypesIdentical(target, UintptrType))) return true;

    // TODO: Function <-> Pointer
    // TODO: Enum <-> Integer

    return false;
}

b32 cast(Type *source, Type *target, CheckerContext *info) {
    if (source == InvalidType || target == InvalidType) return false;

    if (!canCast(source, target)) return false;

    if (info->isConstant) {
        convertValue(source, target, &info->val);
        info->val.u64 &= BITMASK(u64, target->Width);
    }

    return true;
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
        if (symbol) return symbol;

        scope = scope->parent;
    } while (scope);

    return NULL;
}

Type *checkExpr(Package *pkg, Expr *expr, CheckerContext *ctx);
b32 checkStmt(Package *pkg, Stmt *stmt, CheckerContext *ctx);

Type *checkExprIdent(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Expr_Ident ident = expr->Ident;
    Symbol *symbol = Lookup(ctx->scope, ident.name);
    if (!symbol) {
        ReportError(pkg, UndefinedIdentError, expr->start, "Use of undefined identifier '%s'", ident.name);
        ctx->mode = ExprMode_Invalid;
        return InvalidType;
    }

    symbol->used = true;
    if (symbol->state != SymbolState_Resolved) {
        ctx->mode = ExprMode_Unresolved;
        return InvalidType;
    }

    storeInfoIdent(pkg, expr, symbol);

    switch (symbol->kind) {
        case SymbolKind_Type:
            ctx->mode = ExprMode_Type;
            break;

        case SymbolKind_Constant:
            ctx->mode = ExprMode_Computed;
            ctx->isConstant = true;
            break;

        default:
            ctx->mode = ExprMode_Addressable;
    }

    ctx->val = symbol->val;
    return symbol->type;
}

Type *checkExprLitInt(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Expr_LitInt lit = expr->LitInt;

    Type *type = SmallestIntTypeForPositiveValue(lit.val);
    if (ctx->desiredType) {
        if (isIntegerOrPointer(ctx->desiredType) || isBoolean(ctx->desiredType)) {
            ctx->val.u64 = lit.val;

            if (lit.val > MaxValueForIntOrPointerType(ctx->desiredType)) {
                ReportError(pkg, InvalidConversionError, expr->start,
                            "Cannot coerce value '%s' to type %s as loss of information would occur",
                            DescribeExpr(expr), DescribeType(ctx->desiredType));
                ReportNote(pkg, expr->start, "If you wish for this overflow to occur add an explicit cast");
            }
        } else if (IsFloat(ctx->desiredType)) {
            ctx->val.f64 = (f64)lit.val;
        } else {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Unable to coerce %s to expected type %s",
                        DescribeExpr(expr), DescribeType(ctx->desiredType));
            // TODO: Check if it's possible through casting and add note that you can cast to make the conversion occur
            goto error;
        }

        type = ctx->desiredType;
    } else {
        ctx->val.u64 = lit.val;
    }

    ctx->mode = ExprMode_Computed;
    ctx->isConstant = true;
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitFloat(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Expr_LitFloat lit = expr->LitFloat;

    Type *type = F64Type;
    if (ctx->desiredType) {
        if (IsFloat(ctx->desiredType)) {
            if (ctx->desiredType->Width == 32) {
                ctx->val.f32 = (f32) lit.val;
                // NOTE: We could report loss of information for coercing literal floats into f32, but opt not to.
                //  This could be a flag...
            } else {
                ctx->val.f64 = lit.val;
            }
        } else {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Unable to coerce %s to expected type %s",
                        DescribeExpr(expr), DescribeType(ctx->desiredType));
            // TODO: Check if it's possible through casting and add note that you can cast to make the conversion occur
            goto error;
        }

        type = ctx->desiredType;
    } else {
        ctx->val.f64 = lit.val;
    }

    ctx->mode = ExprMode_Computed;
    ctx->isConstant = true;
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitNil(Expr *expr, CheckerContext *ctx, Package *pkg) {
    if (ctx->desiredType && !isNilable(ctx->desiredType)) {
        ReportError(pkg, NotNilableError, expr->start,
                    "'nil' is not convertable to '%s'", DescribeType(ctx->desiredType));
        goto error;
    }

    Type *type = ctx->desiredType;
    if (!type) type = RawptrType;

    ctx->mode = ExprMode_Nil;
    ctx->isConstant = true;
    ctx->val = (Val){ .u64 = 0 };

    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTypeVariadic(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Type *type = checkExpr(pkg, expr->TypeVariadic.type, ctx);
    if (!expectType(pkg, type, ctx, expr->TypeVariadic.type->start)) goto error;
    TypeFlag flags = expr->TypeVariadic.flags & TypeVariadicFlagCVargs ? TypeFlag_CVargs : TypeFlag_None;
    type = NewTypeSlice(flags, type);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTypeFunction(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Expr_TypeFunction func = expr->TypeFunction;
    TypeFlag flags = TypeFlag_None;

    b32 isInvalid = false;

    DynamicArray(Type *) params = NULL;
    ArrayFit(params, ArrayLen(func.params));

    CheckerContext info = { .scope = pushScope(pkg, ctx->scope) };
    For (func.params) {
        Type *type = checkExpr(pkg, func.params[i]->value, &info);
        if (!expectType(pkg, type, &info, func.params[i]->start)) isInvalid = true;

        flags |= type->Flags & TypeFlag_Variadic;
        flags |= type->Flags & TypeFlag_CVargs;
        ArrayPush(params, type);
    }

    DynamicArray(Type *) returnTypes = NULL;
    ArrayFit(returnTypes, ArrayLen(func.result));

    ForEach(func.result, Expr *) {
        Type *type = checkExpr(pkg, it, &info);
        if (!expectType(pkg, type, &info, it->start)) {
            isInvalid = true;
        }
        ArrayPush(returnTypes, type);
    }

    if (isInvalid) goto error;

    Type *type = NewTypeFunction(flags, params, returnTypes);

    ctx->mode = ExprMode_Type;
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitFunction(Expr *funcExpr, CheckerContext *ctx, Package *pkg) {
    Expr_LitFunction func = funcExpr->LitFunction;
    Scope *parameterScope = pushScope(pkg, ctx->scope);
    CheckerContext funcInfo = { .scope = parameterScope };

    DynamicArray(Type *) paramTypes = NULL;
    DynamicArray(Type *) resultTypes = NULL;
    DynamicArray(Symbol *) paramSymbols = NULL;
    ArrayFit(paramTypes, ArrayLen(func.type->TypeFunction.params));
    ArrayFit(resultTypes, ArrayLen(func.type->TypeFunction.result));
    ArrayFit(paramSymbols, ArrayLen(func.type->TypeFunction.params));

    TypeFlag typeFlags = TypeFlag_None;

    // FIXME: We need to extract and pass on desired types parameters & results
    ForEach(func.type->TypeFunction.params, Expr_KeyValue *) {
        if (!it->key || it->key->kind != ExprKind_Ident) {
            ReportError(pkg, ParamNameMissingError, it->start, "Parameters for a function literal must be named");
            continue;
        }

        Symbol *symbol;
        declareSymbol(pkg, parameterScope, it->key->Ident.name, &symbol, 0, (Decl *) it);
        ArrayPush(paramSymbols, symbol);

        Type *type = checkExpr(pkg, it->value, &funcInfo);
        if (!expectType(pkg, type, &funcInfo, it->value->start)) continue;
        markSymbolResolved(symbol, type);

        typeFlags |= type->Flags & TypeFlag_Variadic;
        typeFlags |= type->Flags & TypeFlag_CVargs;
        ArrayPush(paramTypes, type);
    }

    ForEach(func.type->TypeFunction.result, Expr *) {
        Type *type = checkExpr(pkg, it, &funcInfo);
        if (!expectType(pkg, type, &funcInfo, it->start)) continue;
        ArrayPush(resultTypes, type);
        if (type == VoidType) {
            if (ArrayLen(func.type->TypeFunction.result) > 1) {
                ReportError(pkg, InvalidUseOfVoidError, it->start,
                            "Void must be a functions only return type");
            }
        }
    }

    Scope *bodyScope = pushScope(pkg, parameterScope);
    Type *tuple = VoidType;
    if (resultTypes[0] != VoidType) {
        tuple = NewTypeTuple(TypeFlag_None, resultTypes);
    }

    CheckerContext bodyContext = { .scope = bodyScope, .desiredType = tuple };
    ForEach(func.body->stmts, Stmt *) {
        checkStmt(pkg, it, &bodyContext);
    }

    ctx->mode = ExprMode_Type;
    return NewTypeFunction(typeFlags, paramTypes, resultTypes);
}

Type *checkExprTypePointer(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Type *desiredType = ctx->desiredType;
    ctx->desiredType = isPointer(ctx->desiredType) ? ctx->desiredType->Pointer.pointeeType : NULL;
    Type *type = checkExpr(pkg, expr->TypePointer.type, ctx);

    expectType(pkg, type, ctx, expr->TypePointer.type->start);
    if (ctx->mode != ExprMode_Type) {
        ReportError(pkg, InvalidPointeeTypeError, expr->start,
                    "'%s' is not a valid pointee type", DescribeType(type));
        goto error;
    } else if (type == VoidType) {
        ReportError(pkg, TODOError, expr->TypePointer.type->start,
                    "Kai does not use void * for raw pointers instead use rawptr");
    }

    if (desiredType == RawptrType) type = RawptrType;

    type = NewTypePointer(TypeFlag_None, type);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprUnary(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Type *type = checkExpr(pkg, expr->Unary.expr, ctx);
    if (ctx->mode == ExprMode_Unresolved) return InvalidType;

    switch (expr->Unary.op) {
        case TK_And:
            if (ctx->mode != ExprMode_Addressable) {
                ReportError(pkg, AddressOfNonAddressableError, expr->start,
                            "Cannot take address of %s", DescribeExpr(expr->Unary.expr));
                goto error;
            }
            type = NewTypePointer(TypeFlag_None, type);
            break;

        default:
            if (!unaryPredicates[expr->Unary.op](type)) {
                ReportError(pkg, InvalidUnaryOperationError, expr->start,
                            "Operation '%s' undefined for %s",
                            DescribeTokenKind(expr->Unary.op), DescribeExpr(expr->Unary.expr));
                goto error;
            }

            if (expr->Unary.op == TK_Not && !ctx->desiredType) {
                type = BoolType;
            } else if (expr->Unary.op == TK_Lss) {
                type = type->Pointer.pointeeType;
            }
    }

    b32 isConstantNegative;
    if (evalUnary(expr->Unary.op, type, ctx, &isConstantNegative)) {
        changeTypeOrRecordCoercionIfNeeded(&type, expr, ctx, isConstantNegative, pkg);
    }
    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Computed;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprBinary(Expr *expr, CheckerContext *ctx, Package *pkg) {
    CheckerContext lhsInfo = { .scope = ctx->scope };
    CheckerContext rhsInfo = { .scope = ctx->scope };
    Type *lhs = checkExpr(pkg, expr->Binary.lhs, &lhsInfo);
    Type *rhs = checkExpr(pkg, expr->Binary.rhs, &rhsInfo);

    if (lhsInfo.mode == ExprMode_Invalid || rhsInfo.mode == ExprMode_Invalid) {
        goto error;
    }

    // TODO: clean this up
    if (!(coerceTypeSilently(expr->Binary.lhs, &lhsInfo, &lhs, rhs, pkg) ||
          coerceTypeSilently(expr->Binary.rhs, &rhsInfo, &rhs, lhs, pkg))) {
        ReportError(pkg, TypeMismatchError, expr->start,
                    "No conversion possible to make %s and %s Identical types",
                    DescribeExpr(expr->Binary.lhs), DescribeExpr(expr->Binary.rhs));
        ReportNote(pkg, expr->start, "An explicit cast may be required");
        goto error;
    }

    // Both lhs & rhs have this type.
    Type *type = lhs;

    if (!binaryPredicates[expr->Binary.op](type)) {
        ReportError(pkg, InvalidBinaryOperationError, expr->Binary.pos,
                    "Operation '%s' undefined for type %s",
                    DescribeTokenKind(expr->Binary.op), DescribeType(lhs));
        goto error;
    }

    switch (expr->Binary.op) {
        case TK_Eql:
        case TK_Neq:
        case TK_Leq:
        case TK_Geq:
        case TK_Lss:
        case TK_Gtr:
        case TK_Lor:
        case TK_Land:
            type = BoolType;
            if (ctx->desiredType && canCoerce(type, ctx->desiredType, ctx)) {
                type = ctx->desiredType;
            }
            break;
        default:
            break;
    }

    if ((expr->Binary.op == TK_Div || expr->Binary.op == TK_Rem) && rhsInfo.isConstant && rhsInfo.val.i64 == 0) {
        ReportError(pkg, DivisionByZeroError, expr->Binary.rhs->start, "Division by zero");
    }

    ctx->isConstant = lhsInfo.isConstant && rhsInfo.isConstant;
    b32 isConstantNegative;
    if (evalBinary(expr->Binary.op, type, lhsInfo.val, rhsInfo.val, ctx, &isConstantNegative)) {
        changeTypeOrRecordCoercionIfNeeded(&type, expr, ctx, isConstantNegative, pkg);
    }
    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Computed;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTernary(Expr *expr, CheckerContext *ctx, Package *pkg) {

    CheckerContext condInfo = { .scope = ctx->scope, .desiredType = BoolType };
    Type *cond = checkExpr(pkg, expr->Ternary.cond, &condInfo);

    Type *pass = cond;
    CheckerContext passInfo = condInfo;
    if (expr->Ternary.pass) {
        passInfo.desiredType = ctx->desiredType;
        pass = checkExpr(pkg, expr->Ternary.pass, &passInfo);
    }

    CheckerContext failInfo = { .scope = ctx->scope, .desiredType = ctx->desiredType };
    Type *fail = checkExpr(pkg, expr->Ternary.fail, &failInfo);
    if (failInfo.mode == ExprMode_Unresolved)

    if (!isBoolean(cond) && !isNumericOrPointer(cond)) {
        ReportError(pkg, BadConditionError, expr->start,
                    "Expected a numeric or pointer type to act as a condition in the ternary expression");
        goto error;
    }

    Type *type = fail;
    if (condInfo.isConstant && passInfo.isConstant && failInfo.isConstant) {
        ctx->isConstant = true;
        ctx->val = condInfo.val.u64 ? passInfo.val : failInfo.val;
        type = condInfo.val.u64 ? pass : fail;
    } else {
        // NOTE: If we coerce the type before doing handling constant evaluation, we lose signedness information.
        if (!coerceType(expr->Ternary.fail, &failInfo, &fail, pass, pkg)) {
            ReportError(pkg, TypeMismatchError, expr->Ternary.fail->start,
                        "Expected type %s got type %s", DescribeType(pass ? pass : cond), DescribeType(fail));
            goto error;
        }
    }
    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Computed;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprCast(Expr *expr, CheckerContext *ctx, Package *pkg) {
    CheckerContext targetInfo = { .scope = ctx->scope };
    Type *type = checkExpr(pkg, expr->Cast.type, &targetInfo);

    if (targetInfo.mode != ExprMode_Type) {
        ReportError(pkg, NotATypeError, expr->start,
                    "Cannot cast to non type %s", DescribeExpr(expr->Cast.type));
        goto error;
    }

    Type *callersDesiredType = ctx->desiredType;

    ctx->desiredType = type;
    Type *exprType = checkExpr(pkg, expr->Cast.expr, ctx);

    if (!cast(exprType, type, ctx)) {
        ReportError(pkg, InvalidConversionError, expr->start,
                    "Unable to cast type %s to type %s", DescribeType(exprType), DescribeType(type));
        goto error;
    }

    if (callersDesiredType) coerceType(expr, ctx, &type, callersDesiredType, pkg);

    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Computed;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprAutocast(Expr *expr, CheckerContext *ctx, Package *pkg) {
    if (!ctx->desiredType) {
        ReportError(pkg, AutocastExpectsDesiredTypeError, expr->start,
                    "Autocast expression requires a contextual type to convert to");
        goto error;
    }

    Type *type = checkExpr(pkg, expr->Autocast.expr, ctx);

    if (!cast(type, ctx->desiredType, ctx)) {
        ReportError(pkg, InvalidConversionError, expr->Autocast.expr->start,
                    "Cannot convert expression of type %s to type %s",
                    DescribeType(type), DescribeType(ctx->desiredType));
        goto error;
    }

    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprCall(Expr *expr, CheckerContext *ctx, Package *pkg) {
    CheckerContext calleeInfo = { .scope = ctx->scope };
    Type *calleeType = checkExpr(pkg, expr->Call.expr, &calleeInfo);
    if (calleeInfo.mode == ExprMode_Type) {

        if (ArrayLen(expr->Call.args) < 1) {
            ReportError(pkg, CastArgumentCountError, expr->start,
                        "Missing argument in cast to %s", DescribeType(calleeType));
            goto error;
        } else if (ArrayLen(expr->Call.args) > 1) {
            ReportError(pkg, CastArgumentCountError, expr->start,
                        "Too many arguments in cast to %s", DescribeType(calleeType));
            goto error;
        }

        expr->kind = ExprKind_Cast;
        Expr_Cast cast = { .start = expr->start, .type = expr->Call.expr, .expr = expr->Call.args[0]->value };
        expr->Cast = cast;
        return checkExprCast(expr, ctx, pkg);
    }

    UNIMPLEMENTED();

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExpr(Package *pkg, Expr *expr, CheckerContext *ctx) {
    Type *type = NULL;
    switch (expr->kind) {
        case ExprKind_Ident:
            type = checkExprIdent(expr, ctx, pkg);
            break;

        case ExprKind_LitInt:
            type = checkExprLitInt(expr, ctx, pkg);
            break;

        case ExprKind_LitFloat:
            type = checkExprLitFloat(expr, ctx, pkg);
            break;

        case ExprKind_LitNil:
            type = checkExprLitNil(expr, ctx, pkg);
            break;

        case ExprKind_LitFunction:
            type = checkExprLitFunction(expr, ctx, pkg);
            break;

        case ExprKind_TypeVariadic:
            type = checkExprTypeVariadic(expr, ctx, pkg);
            break;

        case ExprKind_TypePointer:
            type = checkExprTypePointer(expr, ctx, pkg);
            break;

        case ExprKind_TypeFunction:
            type = checkExprTypeFunction(expr, ctx, pkg);
            break;

        case ExprKind_Unary:
            type = checkExprUnary(expr, ctx, pkg);
            break;

        case ExprKind_Binary:
            type = checkExprBinary(expr, ctx, pkg);
            break;

        case ExprKind_Ternary:
            type = checkExprTernary(expr, ctx, pkg);
            break;

        case ExprKind_Call:
            type = checkExprCall(expr, ctx, pkg);
            break;

        default:
            break;
    }

    return type;
}

b32 checkDeclConstant(Package *pkg, Decl *declStmt, CheckerContext *ctx) {
    Decl_Constant decl = declStmt->Constant;

    if (ArrayLen(decl.names) != 1) {
        ReportError(pkg, MultipleConstantDeclError, decl.start,
            "Constant declarations must declare at most one item");

        ForEach (decl.names, Expr_Ident *) {
            Symbol *symbol = Lookup(pkg->scope, it->name);
            markSymbolInvalid(symbol);
        }

        return false;
    }

    if (ArrayLen(decl.values) > 1) {
        ReportError(pkg, ArityMismatchError, decl.start,
                    "Constant declarations only allow for a single value, but got %zu", ArrayLen(decl.values));
        return false;
    }

    Type *expectedType = NULL;

    if (decl.type) {
        expectedType = checkExpr(pkg, decl.type, ctx);
        if (ctx->mode == ExprMode_Unresolved) return true;

        expectType(pkg, expectedType, ctx, decl.type->start);
    }

    Expr_Ident *ident = decl.names[0];
    Expr *value = decl.values[0];

    Symbol *symbol;
    if (ctx->scope == pkg->scope) {
        symbol = MapGet(&ctx->scope->members, ident->name);
        ASSERT_MSG(symbol, "Symbols in the file scope should be declared in the Parser");
    } else {
        declareSymbol(pkg, ctx->scope, ident->name, &symbol, declStmt->id, declStmt);
    }

    symbol->state = SymbolState_Resolving;

    switch (value->kind) {
    case ExprKind_LitFunction: {
        Expr_LitFunction func = value->LitFunction;
        Type *type = checkExprTypeFunction(func.type, ctx, pkg);
        if (ctx->mode == ExprMode_Unresolved) return true;

        expectType(pkg, type, ctx, func.type->start);

        markSymbolResolved(symbol, type);
    } break;

    case ExprKind_TypeStruct:
    case ExprKind_TypeUnion:
    case ExprKind_TypeEnum: {
        UNIMPLEMENTED();
    } break;
        default:
            break;
    }

    CheckerContext exprCtx = {.scope = ctx->scope, .desiredType = expectedType};
    Type *type = checkExpr(pkg, value, &exprCtx);
    if (exprCtx.mode == ExprMode_Unresolved) return true;

    symbol->val = exprCtx.val;

    if (expectedType && !coerceType(value, &exprCtx, &type, expectedType, pkg)) {
        ReportError(pkg, InvalidConversionError, value->start,
                    "Unable to convert type %s to expected type type %s", DescribeType(type), DescribeType(expectedType));
        markSymbolInvalid(symbol);
        return false;
    }

    markSymbolResolved(symbol, type);
    storeInfoConstant(pkg, declStmt, symbol);
    return false;
}

b32 checkDeclVariable(Package *pkg, Decl *declStmt, CheckerContext *ctx) {
    Decl_Variable var = declStmt->Variable;

    Type *expectedType = NULL;

    if (var.type) {
        expectedType = checkExpr(pkg, var.type, ctx);
        if (ctx->mode == ExprMode_Unresolved) return true;

        expectType(pkg, expectedType, ctx, var.type->start);
    }

    DynamicArray(Symbol *) symbols = NULL;
    ArrayFit(symbols, ArrayLen(var.names));

    if (ctx->scope == pkg->scope) {
        ForEach(var.names, Expr_Ident *) {
            Symbol *symbol = MapGet(&pkg->scope->members, it->name);
            ASSERT_MSG(symbol, "Symbols in the file scope should be declared in the Parser");
            ArrayPush(symbols, symbol);
        }
    } else {
        ForEach(var.names, Expr_Ident *) {
            Symbol *symbol;
            // FIXME(Brett): figure out how I want to recover from a duplicate
            declareSymbol(pkg, ctx->scope, it->name, &symbol, declStmt->id, declStmt);
            ArrayPush(symbols, symbol);
        }
    }

    // NOTE: decl like `x, y: i32`
    if (ArrayLen(var.values) == 0) {
        ASSERT(expectedType);
        ForEach(symbols, Symbol *) {
            it->type = expectedType;
            it->state = SymbolState_Resolved;
        }

        if (expectedType->kind == TypeKind_Array && expectedType->Array.length == -1) {
            ReportError(
                pkg, UninitImplicitArrayError, var.type->start, 
                "Implicit-length array must have an initial value"
            );
        }

        if (expectedType->kind == TypeKind_Function) {
            ReportError(pkg, UninitFunctionTypeError, var.type->start,
                "Variables of a function type must be initialized");
            ReportNote(pkg, var.type->start,
                       "If you want an uninitialized function pointer use *%s instead", DescribeType(expectedType));
        }
    } else {
        if (ArrayLen(var.values) != ArrayLen(var.names)) {
            // TODO: ensure that this is a function call otherwise report this error
            ReportError(
                pkg, ArityMismatchError, var.start, 
                "The amount of identifiers (%zu) doesn't match the amount of values (%zu)", 
                ArrayLen(var.names), ArrayLen(var.values)
            );

            For (symbols) {
                markSymbolInvalid(symbols[i]);
            }
            return false;
        }

        // TODO(Brett): check for multi-value call
        CheckerContext exprCtx = {.scope = ctx->scope, .desiredType = expectedType};
        For (var.names) {
            Type *type = checkExpr(pkg, var.values[i], &exprCtx);
            if (exprCtx.mode == ExprMode_Unresolved) {
                return true;
            }

            if (expectedType && !coerceType(var.values[i], &exprCtx, &type, expectedType, pkg)) {
                ReportError(pkg, InvalidConversionError, var.values[i]->start,
                            "Unable to convert type %s to expected type type %s",
                            DescribeType(type), DescribeType(expectedType));
                markSymbolInvalid(symbols[i]);
                return false;
            }

            if (exprCtx.mode == ExprMode_Type) {
                ReportError(pkg, TypeNotAnExpressionError, var.values[i]->start,
                            "Type %s is not an expression in this context", DescribeType(type));
                ReportNote(pkg, var.values[i]->start,
                           "Type metadata can be retrieved using the typeof or typeid functions");
                markSymbolInvalid(symbols[i]);
                continue;
            }

            symbols[i]->kind = SymbolKind_Variable;
            markSymbolResolved(symbols[i], type);
        }
    }

    storeInfoVariable(pkg, declStmt, symbols);

    return false;
}

b32 checkDeclImport(Package *pkg, Decl *declStmt) {
//    Decl_Import import = declStmt->Import;
    UNIMPLEMENTED();
    return false;
}

void checkStmtReturn(Package *pkg, Stmt *stmt, CheckerContext *ctx) {
    ASSERT(ctx->desiredType && ctx->desiredType->kind == TypeKind_Tuple);

    size_t nTypes = ArrayLen(ctx->desiredType->Tuple.types);
    size_t nExprs = ArrayLen(stmt->Return.exprs);

    if (nExprs != nTypes) {
        ReportError(pkg, WrongNumberOfReturnsError, stmt->start,
                    "Wrong number of return expressions, expected %zu, got %zu",
                    ArrayLen(ctx->desiredType->Tuple.types), ArrayLen(stmt->Return.exprs));
    }
    for (size_t i = 0; i < MIN(nTypes, nExprs); i++) {
        Expr *expr = stmt->Return.exprs[i];
        Type *expectedType = ctx->desiredType->Tuple.types[i];
        CheckerContext exprCtx = { .scope = ctx->scope, .desiredType = expectedType };
        Type *type = checkExpr(pkg, expr, &exprCtx);
        if (!TypesIdentical(type, expectedType)) {
            ReportError(pkg, TypeMismatchError, expr->start,
                        "Expected type %s got type %s",
                        DescribeType(expectedType), DescribeType(type));
            return;
        }
    }
}

b32 checkStmt(Package *pkg, Stmt *stmt, CheckerContext *ctx) {
    b32 shouldRequeue = false;

    switch (stmt->kind) {
        case StmtDeclKind_Constant:
            shouldRequeue = checkDeclConstant(pkg, (Decl *)stmt, ctx);
            break;

        case StmtDeclKind_Variable:
            shouldRequeue = checkDeclVariable(pkg, (Decl *)stmt, ctx);
            break;

        case StmtDeclKind_Import:
            shouldRequeue = checkDeclImport(pkg, (Decl *)stmt);
            break;

        case StmtKind_Return:
            checkStmtReturn(pkg, stmt, ctx);
            break;

        default:
            ASSERT_MSG_VA(false, "Statement of type '%s' went unchecked", AstDescriptions[stmt->kind]);
    }

    return shouldRequeue;
}

#if TEST
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

    ArenaFree(&parsingQueue.arena);
    memset(&checkingQueue, 0, sizeof(Queue));
    ArenaFree(&checkingQueue.arena);
    memset(&checkingQueue, 0, sizeof(Queue));

    pkg.scope = pushScope(&pkg, builtinPackage.scope);

    parsePackageCode(&pkg, code);
    return checkingQueue;
}

Stmt *resetAndParseSingleStmt(const char *code) {
    Queue queue = resetAndParse(code);
    CheckerWork *work = QueueDequeue(&queue);
    ASSERT(work);
    Stmt *stmt = work->stmt;
    ArenaFree(&queue.arena);
    return stmt;
}

void test_checkConstantDeclarations() {
    REINIT_COMPILER();
    Queue queue = resetAndParse("x :: 8");

    CheckerWork *work = QueueDequeue(&queue);

    ASSERT(queue.size == 0);

    Stmt *stmt = work->stmt;
    CheckerContext ctx = { .scope = pkg.scope };
    b32 requeue = checkStmt(&pkg, stmt, &ctx);
    ASSERT(!requeue);

    Symbol *sym = Lookup(pkg.scope, StrIntern("x"));
    ASSERT(sym);
    ASSERT(IsInteger(sym->type));
    ASSERT(sym->state == SymbolState_Resolved);
    ASSERT(!sym->used);
    ASSERT(sym->kind == SymbolKind_Constant);
    ASSERT(sym->decl->start.offset == 0);
    ASSERT(sym->val.u64 == 8);
}

void test_coercionsAreMarked() {
    REINIT_COMPILER();
    Stmt *stmt;
    CheckerInfo* info;
    CheckerContext ctx;
#define checkBasicExpr(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    checkStmt(&pkg, stmt, &ctx); \
    info = CheckerInfoForStmt(&pkg, stmt)

    //              1   2     3 5 4
    checkBasicExpr("x : u64 : 1 + 2");
    ASSERT(info->Constant.symbol->name == StrIntern("x"));
    ASSERT_MSG(pkg.astIdCount == 6, "Package was not fully reset as expected");
    Conversion coerce = pkg.checkerInfo[5].BasicExpr.coerce;
    ASSERT(coerce & ConversionClass_Same);
    ASSERT(coerce & ConversionFlag_Extend);
}

void test_checkConstantUnaryExpressions() {
    REINIT_COMPILER();
    Stmt *stmt;
    CheckerContext ctx;
    Type *type;
#define checkUnary(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprUnary((Expr *) stmt, &ctx, &pkg)

    checkUnary("-100");
    ASSERT(type == I8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == -100);

    checkUnary("!false");
    ASSERT(type == BoolType);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.b32 == true);

    checkUnary("!!false");
    ASSERT(type == BoolType);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.b32 == false);

    checkUnary("~0xffff");
    ASSERT_MSG(type == U8Type, "Expected a u8 type to represent the value 0");
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.u64 == 0);

#undef checkUnary
}

void test_checkConstantBinaryExpressions() {
    REINIT_COMPILER();
    Stmt *stmt;
    CheckerContext ctx;
    Type *type;
#define checkBinary(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprBinary((Expr *) stmt, &ctx, &pkg)

    checkBinary("1 + 2");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.u64 == 3);

    checkBinary("1 + 2.0");
    ASSERT(type == F64Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.f64 == 3.0);

    checkBinary("1 + 2.0 - 3");
    ASSERT(type == F64Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.f64 == 0.f);

    checkBinary("1 / 0");
    ASSERT(ArrayLen(pkg.diagnostics.errors) == 1);
    ArrayFree(pkg.diagnostics.errors);

    checkBinary("-1 + -8");
    ASSERT(type == I8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == -9);

    checkBinary("255 + 255");
    ASSERT(type == U16Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.u16 == 510);
}

void test_checkConstantTernaryExpression() {
    REINIT_COMPILER();
    Stmt *stmt;
    CheckerContext ctx;
    Type *type;
#define checkTernary(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprTernary((Expr *) stmt, &ctx, &pkg)

    checkTernary("true ? 1 : 2");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == 1);

    checkTernary("false ? 1.5 : 2.5");
    ASSERT(type == F64Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.f64 == 2.5);

    checkTernary("0 ? 1 ? 2 : 3 : 4");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == 4);

    checkTernary("1 ? 1 ? 2 : 3 : 4");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == 2);

    checkTernary("false ? 100000 : 1");
    ASSERT(type == U8Type);

    // NOTE: This would have a different type condition wasn't a constant
    checkTernary("rawptr(nil) ?: 250");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.u64 == 250);
}

void test_checkConstantCastExpression() {
    REINIT_COMPILER();
    Stmt *stmt;
    CheckerContext ctx;
    Type *type;
#define checkCastUsingCallSyntax(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprCall((Expr *) stmt, &ctx, &pkg)

    checkCastUsingCallSyntax("i64(8)");
    ASSERT(type == I64Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == 8);

    checkCastUsingCallSyntax("u8(100000000000042)");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.u64 == 42);

#define checkCast(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprCast((Expr *) stmt, &ctx, &pkg)

    checkCast("cast(i64) 8");
    ASSERT(type == I64Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.i64 == 8);

    checkCast("cast(u8) 100000000000042");
    ASSERT(type == U8Type);
    ASSERT(ctx.isConstant);
    ASSERT(ctx.val.u64 == 42);
}

Type *typeFromParsing(const char *code) {
    pkg.scope = pushScope(&pkg, builtinPackage.scope);

    Stmt *stmt = resetAndParseSingleStmt(code);
    CheckerContext ctx = { .scope = pkg.scope };
    return checkExpr(&pkg, (Expr *) stmt, &ctx);
}

void test_checkExprLitFunction() {
    REINIT_COMPILER();
    Expr *expr;
    CheckerContext ctx;
    Type *type;

#define checkFunction(_CODE) \
    expr = (Expr *) resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprLitFunction(expr, &ctx, &pkg);

    checkFunction("fn (a: u64) -> u64 { return a }");
    ASSERT(type == typeFromParsing("fn(u64) -> u64"));

    checkFunction("fn (a, b: u64) -> u64, bool { return a }");
    ASSERT(type == typeFromParsing("fn(u64, u64) -> u64, bool"));

    // See test_buggyVargs
//    checkFunction("fn (fmt: *u8, args: ..any) -> i32");
//    ASSERT(type == typeFromParsing("fn (fmt: *u8, args: ..any) -> i32"));
}

#undef pkg
#endif

/*
void test_buggyVargs() {
    REINIT_COMPILER();
    Expr *expr;
    CheckerContext ctx;
    Type *type;

#define checkFunction(_CODE) \
    expr = (Expr *) resetAndParseSingleStmt(_CODE); \
    ctx = (CheckerContext){ .scope = pkg.scope }; \
    type = checkExprLitFunction(expr, &ctx, &pkg);

    checkFunction("fn (fmt: *u8, args: ..any) -> i32");
    ASSERT(type == typeFromParsing("fn (fmt: *u8, args: ..any) -> i32"));
}
*/

