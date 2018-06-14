
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

typedef struct ExprInfo ExprInfo;
struct ExprInfo {
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
CheckerInfo *GetExprInfo(Package *pkg, Expr *expr) {
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

void storeInfoBasicExpr(Package *pkg, Expr *expr, Type *type) {
    CheckerInfo info = {CheckerInfoKind_BasicExpr, .BasicExpr.type = type};
    pkg->checkerInfo[expr->id] = info;
}

void storeInfoBasicExprWithConstant(Package *pkg, Expr *expr, Type *type, Val val) {
    CheckerInfo info = {CheckerInfoKind_BasicExpr, .BasicExpr.type = type};
    info.BasicExpr.isConstant = true;
    info.BasicExpr.val = val;
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

b32 expectType(Package *pkg, Type *type, ExprInfo *info, Position pos) {
    if (info->mode != ExprMode_Type) {
        ReportError(pkg, NotATypeError, pos, "%s cannot be used as a type", DescribeTypeKind(type->kind));
    }
    return info->mode == ExprMode_Type;
}

b32 IsInteger(Type *type) {
    return type->kind == TypeKind_Int;
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
    return IsInteger(type) && (type->Flags & TypeFlag_Boolean) != 0;
}

b32 isPointer(Type *type) {
    return type->kind == TypeKind_Pointer;
}

b32 isNilable(Type *type) {
    return type->kind == TypeKind_Pointer;
}

b32 isNumericOrPointer(Type *type) {
    return isNumeric(type) || isPointer(type);
}

b32 canBeUsedForLogical(Type *type) {
    return IsInteger(type) || isPointer(type);
}

b32 isEnum(Type *type) {
    return type->kind == TypeKind_Enum;
}

b32 isEnumFlags(Type *type) {
    return isEnum(type) && (type->Flags & TypeFlag_EnumFlags) != 0;
}

b32 isTyped(Type *type) {
    return (type->Flags & TypeFlag_Untyped) == 0;
}

b32 isUntyped(Type *type) {
    return (type->Flags & TypeFlag_Untyped) != 0;
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

b32 canCoerce(Type *type, Type *target) {
    if (type == target) return true;

    // Any numeric or pointer type can convert to Bool type
    if (isNumericOrPointer(type) && isBoolean(target)) return true;

    if (IsInteger(target) && IsInteger(type)) {
        if (IsSigned(type) && !IsSigned(target)) {
            // Conversion from a signed integer to an unsigned requires a cast.
            return false;
        }

        if (!IsSigned(type) && IsSigned(target)) {
            // Conversion from unsigned to signed requires the signed type is larger
            return type->Width < target->Width;
        }

        // The two integer types have the same signedness. The target must be the same or a larger size.
        return type->Width <= target->Width;
    }

    if (IsFloat(type)) {
        // Conversion between float types requires the target is larger or equal in size.
        return IsFloat(target) && type->Width <= target->Width;
    }
    if (IsFloat(target)) {
        // Conversion to float type only requires the source type is numeric
        return isNumeric(type);
    }

    if (isPointer(type)) {
        // Conversion from any pointer type to rawptr is allowed
        // Conversion from any pointer type to an integer is allowed if the intptr or uintptr types can also convert
        return target == RawptrType || canCoerce(IntptrType, target) || canCoerce(UintptrType, target);
    }

    if (isPointer(target)) {
        // Only untyped integers are allowed to implicitly convert to a pointer type
        // Well... pointers can implicitly convert to pointers too. But that's handled above.
        return IsInteger(type) && isUntyped(type);
    }

    return false;
}

#if TEST
void test_canCoerce() {
    INIT_COMPILER();

    ASSERT(canCoerce(I8Type, I16Type));
    ASSERT(canCoerce(U8Type, I16Type));
    ASSERT(canCoerce(I16Type, I16Type));
    ASSERT(canCoerce(UintType, UintptrType));
    ASSERT(!canCoerce(I8Type, U64Type));
    ASSERT(canCoerce(NewTypePointer(TypeFlag_None, U8Type), RawptrType));
    ASSERT(canCoerce(UntypedIntType, NewTypePointer(TypeFlag_None, U8Type)));
    ASSERT(canCoerce(NewTypePointer(TypeFlag_None, IntType), RawptrType));
    ASSERT(canCoerce(NewTypePointer(TypeFlag_None, U64Type), IntptrType));
}
#endif

b32 canRepresentValue(Val val, Type *target) {
    // TODO: Implement this properly
    // Also tests for it.
    return true;
}

// FIXME: This doesn't sign extend to the target size currently, only 64 bits.
i64 SignExtend(Type *type, Type *target, Val val) {
    if (type->Width == 64) return val.i64;
    u64 v = val.u64 & ((1ull << type->Width) - 1);
    u64 mask = 1ull << (type->Width - 1);
    return (v ^ mask) - mask;
}

void convertValue(Type *type, Type *target, Val *val) {
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

b32 TypesIdentical(Type *type, Type *target) {
    while (isAlias(type)) {
        type = type->Symbol->type;
    }
    while (isAlias(target)) {
        target = target->Symbol->type;
    }
    return type == target;
}

void updateExprTypeIfUntyped(Package *pkg, ExprInfo *info, Expr *expr, Type *type, Type *target) {
    if (isTyped(target)) return;

    switch (expr->kind) {
        case ExprKind_Paren:
            updateExprTypeIfUntyped(pkg, info, expr->Paren.expr, type, target);
            break;

        case ExprKind_Unary:
            updateExprTypeIfUntyped(pkg, info, expr->Unary.expr, type, target);
            break;

        case ExprKind_Binary:
            updateExprTypeIfUntyped(pkg, info, expr->Binary.lhs, type, target);
            updateExprTypeIfUntyped(pkg, info, expr->Binary.rhs, type, target);
            break;

        case ExprKind_LitInt:
        case ExprKind_LitFloat:
        case ExprKind_LitNil:
        case ExprKind_LitString:
            break;

        default:
            return;
    }

    CheckerInfo_BasicExpr *ci = &CheckerInfoForExpr(pkg, expr)->BasicExpr;
    if (ci->isConstant && isUntyped(ci->type)) {
        if (!canRepresentValue(ci->val, target)) {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Cannot implicitly convert %s to type %s as loss of information would occur",
                        DescribeExpr(expr), DescribeType(target));
            // TODO: Attach a note reporting what is forcing that type on this expression
            //  This will require  more parameters (the source of the target) for both
            //  updateExprTypeIfUntyped & for convertUntyped
        }
    }

    ci->type = target;
    convertValue(type, target, &info->val);
}

void coerceUntyped(Package *pkg, ExprInfo *info, Expr *expr, Type **type, Type *target) {
    if (*type == InvalidType || isTyped(*type) || target == InvalidType) return;

    if (isUntyped(target)) {
        // Both are untyped
        if (target->TypeId < (*type)->TypeId) {
            updateExprTypeIfUntyped(pkg, info, expr, *type, target);

            *type = target;
            return;
        }
        // Either the types match or the conversion cannot occur. Regardless we exit without warning becuase we
        //  should only get here in a binary expression where there will be a subsequent call to convert target to type
        return;
    }

    // target is typed
    switch (target->kind) {
        case TypeKind_Int:
        case TypeKind_Float:
        case TypeKind_Pointer:
            updateExprTypeIfUntyped(pkg, info, expr, *type, target);
            break;

        default:
            goto error;
    }

    return;

error:
    ReportError(pkg, InvalidConversionError, expr->start,
                "Cannot convert %s to %s", DescribeType(*type), DescribeType(target));
    *type = InvalidType;
}

b32 coerceTypeSilently(Expr *expr, ExprInfo *info, Type **type, Type *target, Package *pkg) {
    if (*type == InvalidType || target == InvalidType) return false;
    if (TypesIdentical(*type, target)) return true;

    if (isUntyped(*type)) coerceUntyped(pkg, info, expr, type, target);
    if (!canCoerce(*type, target)) return false;
    if (info->isConstant) convertValue(*type, target, &info->val);

    *type = target;

    return true;
}

b32 coerceType(Expr *expr, ExprInfo *info, Type **type, Type *target, Package *pkg) {

    b32 success = coerceTypeSilently(expr, info, type, target, pkg);
    if (!success) {
        ReportError(pkg, InvalidConversionError, expr->start,
                    "Cannot convert %s to type %s", DescribeExpr(expr), DescribeType(target));
        *type = InvalidType;
    }

    return success;
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

Type *checkExpr(Package *pkg, Expr *expr, ExprInfo *exprInfo);

Type *checkExprIdent(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    Expr_Ident ident = expr->Ident;
    Symbol *symbol = Lookup(exprInfo->scope, ident.name);
    if (!symbol) {
        ReportError(pkg, UndefinedIdentError, expr->start, "Use of undefined identifier '%s'", ident.name);
        exprInfo->mode = ExprMode_Invalid;
        return InvalidType;
    }

    symbol->used = true;
    if (symbol->state != SymbolState_Resolved) {
        exprInfo->mode = ExprMode_Unresolved;
        return InvalidType;
    }

    storeInfoIdent(pkg, expr, symbol);

    switch (symbol->kind) {
        case SymbolKind_Type:
            exprInfo->mode = ExprMode_Type;
            break;

        case SymbolKind_Constant:
            exprInfo->mode = ExprMode_Computed;
            exprInfo->isConstant = true;
            break;

        default:
            exprInfo->mode = ExprMode_Addressable;
    }

    exprInfo->val = symbol->val;
    return symbol->type;
}

Type *checkExprLitInt(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    Expr_LitInt lit = expr->LitInt;

    Type *type = UntypedIntType;
    if (exprInfo->desiredType) {
        if (IsInteger(exprInfo->desiredType) || isPointer(exprInfo->desiredType)) {
            exprInfo->val.u64 = lit.val;
        } else if (IsFloat(exprInfo->desiredType)) {
            exprInfo->val.f64 = (f64)lit.val;
        } else {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Unable to convert type %s to expected type type %s", DescribeType(UntypedIntType), DescribeType(exprInfo->desiredType));
            goto error;
        }

        type = exprInfo->desiredType;
    } else {
        exprInfo->val.u64 = lit.val;
    }

    exprInfo->mode = ExprMode_Computed;
    exprInfo->isConstant = true;
    storeInfoBasicExprWithConstant(pkg, expr, type, exprInfo->val);
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitFloat(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    Expr_LitFloat lit = expr->LitFloat;

    Type *type = UntypedFloatType;
    if (exprInfo->desiredType) {
        if (IsFloat(exprInfo->desiredType)) {
            if (exprInfo->desiredType->Width == 32) {
                exprInfo->val.f32 = (f32) lit.val;
            } else {
                exprInfo->val.f64 = lit.val;
            }
        } else {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Unable to convert type %s to expected type type %s", DescribeType(UntypedFloatType), DescribeType(exprInfo->desiredType));
            goto error;
        }

        type = exprInfo->desiredType;
    } else {
        exprInfo->val.f64 = lit.val;
    }

    exprInfo->mode = ExprMode_Computed;
    exprInfo->isConstant = true;
    storeInfoBasicExprWithConstant(pkg, expr, type, exprInfo->val);
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitNil(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    if (exprInfo->desiredType && !isNilable(exprInfo->desiredType)) {
        ReportError(pkg, NotNilableError, expr->start,
                    "'nil' is not convertable to '%s'", DescribeType(exprInfo->desiredType));
        goto error;
    }

    Type *type = exprInfo->desiredType;
    if (!type) type = UntypedIntType;

    storeInfoBasicExprWithConstant(pkg, expr, type, (Val){.u64 = 0});

    exprInfo->mode = ExprMode_Nil;
    exprInfo->isConstant = true;
    exprInfo->val = (Val){ .u64 = 0 };
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTypeVariadic(Package *pkg, Expr *expr, ExprInfo *info) {
    Type *type = checkExpr(pkg, expr->TypeVariadic.type, info);
    if (!expectType(pkg, type, info, expr->TypeVariadic.type->start)) goto error;
    TypeFlag flags = expr->TypeVariadic.flags & TypeVariadicFlagCVargs ? TypeFlag_CVargs : TypeFlag_None;
    type = NewTypeSlice(flags, type);
    info->mode = ExprMode_Type;
    return type;

error:
    info->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTypeFunction(Package *pkg, Expr *expr, ExprInfo *exprInfo) {
    Expr_TypeFunction func = expr->TypeFunction;
    TypeFlag flags = TypeFlag_None;

    b32 isInvalid = false;

    DynamicArray(Type *) params = NULL;
    ArrayFit(params, ArrayLen(func.params));

    ExprInfo info = { .scope = pushScope(pkg, exprInfo->scope) };
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

    storeInfoBasicExpr(pkg, expr, type);
    exprInfo->mode = ExprMode_Type;
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitFunction(Package *pkg, Expr *funcExpr, ExprInfo *exprInfo) {
    Expr_LitFunction func = funcExpr->LitFunction;
    Scope *scope = pushScope(pkg, exprInfo->scope);
    ExprInfo funcInfo = { .scope = scope };

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
        declareSymbol(pkg, scope, it->key->Ident.name, &symbol, 0, (Decl *) it);
        ArrayPush(paramSymbols, symbol);

        Type *type = checkExpr(pkg, it->value, &funcInfo);
        if (!expectType(pkg, type, &funcInfo, it->value->start)) continue;

        typeFlags |= type->Flags & TypeFlag_Variadic;
        typeFlags |= type->Flags & TypeFlag_CVargs;
        ArrayPush(paramTypes, type);
    }

    ForEach(func.type->TypeFunction.result, Expr *) {
        Type *type = checkExpr(pkg, it, &funcInfo);
        if (!expectType(pkg, type, &funcInfo, it->start)) continue;
        ArrayPush(resultTypes, type);
    }

    // TODO: call check for each stmt in body

    exprInfo->mode = ExprMode_Type;
    return NewTypeFunction(typeFlags, paramTypes, resultTypes);
}

Type *checkExprTypePointer(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    Type *desiredType = exprInfo->desiredType;
    exprInfo->desiredType = isPointer(exprInfo->desiredType) ? exprInfo->desiredType->Pointer.pointeeType : NULL;
    Type *type = checkExpr(pkg, expr->TypePointer.type, exprInfo);

    expectType(pkg, type, exprInfo, expr->TypePointer.type->start);
    if (exprInfo->mode != ExprMode_Type) {
        ReportError(pkg, InvalidPointeeTypeError, expr->start,
                    "'%s' is not a valid pointee type", DescribeType(type));
        goto error;
    } else if (type == VoidType) {
        ReportError(pkg, TODOError, expr->TypePointer.type->start, "Kai does not use void * for raw pointers instead use rawptr");
    }

    if (desiredType == RawptrType) type = RawptrType;

    type = NewTypePointer(TypeFlag_None, type);
    exprInfo->mode = ExprMode_Type;
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprUnary(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    Type *type = checkExpr(pkg, expr->Unary.expr, exprInfo);
    if (exprInfo->mode == ExprMode_Unresolved) return InvalidType;

    switch (expr->Unary.op) {
        case TK_And:
            if (exprInfo->mode != ExprMode_Addressable) {
                ReportError(pkg, AddressOfNonAddressableError, expr->start,
                            "Cannot take address of %s", DescribeExpr(expr->Unary.expr));
                goto error;
            }
            type = NewTypePointer(TypeFlag_None, type);
            break;

        default:
            if (!unaryPredicates[expr->Unary.op](type)) {
                ReportError(pkg, InvalidUnaryOperationError, expr->start,
                            "Operation '%s' undefined for %s", DescribeTokenKind(expr->Unary.op), DescribeExpr(expr->Unary.expr));
                goto error;
            }

            if (expr->Unary.op == TK_Not && !exprInfo->desiredType) {
                type = BoolType;
            } else if (expr->Unary.op == TK_Lss) {
                type = type->Pointer.pointeeType;
            }
    }

    if (evalUnary(expr->Unary.op, type, exprInfo)) {
        storeInfoBasicExprWithConstant(pkg, expr, type, exprInfo->val);
    } else {
        storeInfoBasicExpr(pkg, expr, type);
    }

    exprInfo->mode = ExprMode_Computed;
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprBinary(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    ExprInfo lhsInfo = { .scope = exprInfo->scope };
    ExprInfo rhsInfo = { .scope = exprInfo->scope };
    Type *lhs = checkExpr(pkg, expr->Binary.lhs, &lhsInfo);
    Type *rhs = checkExpr(pkg, expr->Binary.rhs, &rhsInfo);

    if (lhsInfo.mode == ExprMode_Invalid || rhsInfo.mode == ExprMode_Invalid) {
        goto error;
    }

    coerceUntyped(pkg, &lhsInfo, expr->Binary.lhs, &lhs, rhs);
    if (lhs == InvalidType) goto error;

    coerceUntyped(pkg, &rhsInfo, expr->Binary.rhs, &rhs, lhs);
    if (rhs == InvalidType) goto error;

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
            if (exprInfo->desiredType && canCoerce(type, exprInfo->desiredType)) {
                type = exprInfo->desiredType;
            } else {
                type = BoolType;
            }
            break;
        default:
            break;
    }

    if ((expr->Binary.op == TK_Div || expr->Binary.op == TK_Rem) && rhsInfo.isConstant && rhsInfo.val.i64 == 0) {
        ReportError(pkg, DivisionByZeroError, expr->Binary.rhs->start, "Division by zero");
    }

    exprInfo->isConstant = lhsInfo.isConstant && rhsInfo.isConstant;
    if (evalBinary(expr->Binary.op, type, lhsInfo.val, rhsInfo.val, exprInfo)) {
        storeInfoBasicExprWithConstant(pkg, expr, type, exprInfo->val);
    } else {
        storeInfoBasicExpr(pkg, expr, type);
    }

    exprInfo->mode = ExprMode_Computed;
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTernary(Expr *expr, ExprInfo *exprInfo, Package *pkg) {

    ExprInfo condInfo = { .scope = exprInfo->scope, .desiredType = BoolType };
    Type *cond = checkExpr(pkg, expr->Ternary.cond, &condInfo);

    Type *pass = NULL;
    ExprInfo passInfo = { .scope = exprInfo->scope, .desiredType = exprInfo->desiredType };
    if (expr->Ternary.pass) {
        pass = checkExpr(pkg, expr->Ternary.pass, &passInfo);
    }

    ExprInfo failInfo = { .scope = exprInfo->scope, .desiredType = pass };
    Type *fail = checkExpr(pkg, expr->Ternary.fail, &failInfo);

    if (!isNumericOrPointer(cond)) {
        ReportError(pkg, BadConditionError, expr->start,
                    "Expected a numeric or pointer type to act as a condition in the ternary expression");
        goto error;
    }

    convert(pkg, expr->Ternary.fail, &fail, pass ? pass : cond);
    if (fail == InvalidType) {
        ReportError(pkg, TypeMismatchError, expr->Ternary.fail->start,
                    "Expected type %s got type %s", DescribeType(pass ? pass : cond), DescribeType(fail));
        goto error;
    }

    if (condInfo.isConstant && passInfo.isConstant && failInfo.isConstant) {
        exprInfo->isConstant = true;
        exprInfo->val = condInfo.val.u64 ? passInfo.val : failInfo.val;
        storeInfoBasicExprWithConstant(pkg, expr, fail, exprInfo->val);
    } else {
        storeInfoBasicExpr(pkg, expr, fail);
    }

    exprInfo->mode = ExprMode_Computed;
    return fail;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExpr(Package *pkg, Expr *expr, ExprInfo *exprInfo) {
    Type *type = NULL;
    switch (expr->kind) {
        case ExprKind_Ident:
            type = checkExprIdent(expr, exprInfo, pkg);
            break;

        case ExprKind_LitInt:
            type = checkExprLitInt(expr, exprInfo, pkg);
            break;

        case ExprKind_LitFloat:
            type = checkExprLitFloat(expr, exprInfo, pkg);
            break;

        case ExprKind_LitNil:
            type = checkExprLitNil(expr, exprInfo, pkg);
            break;

        case ExprKind_LitFunction:
            type = checkExprLitFunction(pkg, expr, exprInfo);
            break;

        case ExprKind_TypeVariadic:
            type = checkExprTypeVariadic(pkg, expr, exprInfo);
            break;

        case ExprKind_TypePointer:
            type = checkExprTypePointer(expr, exprInfo, pkg);
            break;

        case ExprKind_Unary:
            type = checkExprUnary(expr, exprInfo, pkg);
            break;

        case ExprKind_Binary:
            type = checkExprBinary(expr, exprInfo, pkg);
            break;

        case ExprKind_Ternary:
            type = checkExprTernary(expr, exprInfo, pkg);
            break;

        default:
            break;
    }

    return type;
}

b32 checkDeclConstant(Package *pkg, Scope *scope, Decl *declStmt) {
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
        ExprInfo info = { .scope = scope };
        expectedType = checkExpr(pkg, decl.type, &info);
        if (info.mode == ExprMode_Unresolved) return true;

        expectType(pkg, expectedType, &info, decl.type->start);
    }

    Expr_Ident *ident = decl.names[0];
    Expr *value = decl.values[0];

    Symbol *symbol;
    if (scope == pkg->scope) {
        symbol = MapGet(&scope->members, ident->name);
        ASSERT_MSG(symbol, "Symbols in the file scope should be declared in the Parser");
    } else {
        declareSymbol(pkg, scope, ident->name, &symbol, declStmt->id, declStmt);
    }

    symbol->state = SymbolState_Resolving;

    switch (value->kind) {
    case ExprKind_LitFunction: {
        Expr_LitFunction func = value->LitFunction;
        ExprInfo info = {.scope = scope};
        Type *type = checkExprTypeFunction(pkg, func.type, &info);
        if (info.mode == ExprMode_Unresolved) return true;

        expectType(pkg, type, &info, func.type->start);

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

    ExprInfo info = {.scope = scope, .desiredType = expectedType};
    Type *type = checkExpr(pkg, value, &info);
    if (info.mode == ExprMode_Unresolved) return true;

    symbol->val = info.val;

    if (expectedType && !coerceType(value, &info, &type, expectedType, pkg)) {
        ReportError(pkg, InvalidConversionError, value->start,
                    "Unable to convert type %s to expected type type %s", DescribeType(type), DescribeType(expectedType));
        markSymbolInvalid(symbol);
        return false;
    }

    markSymbolResolved(symbol, type);
    storeInfoConstant(pkg, declStmt, symbol);
    return false;
}

b32 checkDeclVariable(Package *pkg, Scope *scope, b32 isGlobal, Decl *declStmt) {
    Decl_Variable var = declStmt->Variable;

    Type *expectedType = NULL;

    if (var.type) {
        ExprInfo info = { .scope = scope };
        expectedType = checkExpr(pkg, var.type, &info);
        if (info.mode == ExprMode_Unresolved) return true;

        expectType(pkg, expectedType, &info, var.type->start);
    }

    DynamicArray(Symbol *) symbols = NULL;
    ArrayFit(symbols, ArrayLen(var.names));

    if (scope == pkg->scope) {
        ForEach(var.names, Expr_Ident *) {
            Symbol *symbol = MapGet(&scope->members, it->name);
            ASSERT_MSG(symbol, "Symbols in the file scope should be declared in the Parser");
            ArrayPush(symbols, symbol);
        }
    } else {
        ForEach(var.names, Expr_Ident *) {
            Symbol *symbol;
            // FIXME(Brett): figure out how I want to recover from a duplicate
            declareSymbol(pkg, scope, it->name, &symbol, declStmt->id, declStmt);
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
        ExprInfo info = {.scope = scope, .desiredType = expectedType};
        For (var.names) {
            Type *type = checkExpr(pkg, var.values[i], &info);
            if (info.mode == ExprMode_Unresolved) {
                return true;
            }

            if (expectedType && !coerceType(var.values[i], &info, &type, expectedType, pkg)) {
                ReportError(pkg, InvalidConversionError, var.values[i]->start,
                            "Unable to convert type %s to expected type type %s",
                            DescribeType(type), DescribeType(expectedType));
                markSymbolInvalid(symbols[i]);
                return false;
            }

            if (info.mode == ExprMode_Type) {
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

b32 checkImportDecl(Package *pkg, Decl *declStmt) {
//    Decl_Import import = declStmt->Import;
    UNIMPLEMENTED();
    return false;
}

b32 checkStmt(Package *pkg, Stmt *stmt) {
    b32 shouldRequeue;

    switch (stmt->kind) {
        case StmtDeclKind_Constant:
            shouldRequeue = checkDeclConstant(pkg, pkg->scope, (Decl *)stmt);
            break;

        case StmtDeclKind_Variable:
            shouldRequeue = checkDeclVariable(pkg, pkg->scope, true, (Decl *)stmt);
            break;

        case StmtDeclKind_Import:
            shouldRequeue = checkImportDecl(pkg, (Decl *)stmt);
            break;

        default:
            ASSERT_MSG_VA(false, "Statement of type '%s' went unchecked", AstDescriptions[stmt->kind]);
    }

    return shouldRequeue;
}




#if TEST
#define pkg checkerTestPackage
Package pkg = {0};
Queue resetAndParse(const char *code) {
    // reset package
    ArrayFree(pkg.diagnostics.errors);
    ArrayFree(pkg.stmts);
    ArrayFree(pkg.symbols);

    ArenaFree(&pkg.arena);
    ArenaFree(&pkg.diagnostics.arena);

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
    b32 requeue = checkStmt(&pkg, stmt);
    ASSERT(!requeue);

    Symbol *sym = Lookup(pkg.scope, StrIntern("x"));
    ASSERT(sym);
    ASSERT(sym->type == UntypedIntType);
    ASSERT(sym->state == SymbolState_Resolved);
    ASSERT(!sym->used);
    ASSERT(sym->kind == SymbolKind_Constant);
    ASSERT(sym->decl->start.offset == 0);
    ASSERT(sym->val.u64 == 8);
}

void test_checkConstantUnaryExpressions() {
    REINIT_COMPILER();
    Stmt *stmt;
    ExprInfo info;
    Type *type;
#define checkUnary(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    info = (ExprInfo){ .scope = pkg.scope }; \
    type = checkExprUnary((Expr *) stmt, &info, &pkg)

    checkUnary("-100");
    ASSERT(type == UntypedIntType);
    ASSERT(info.isConstant);
    ASSERT(info.val.i64 == -100);

    checkUnary("!false");
    ASSERT(type == BoolType);
    ASSERT(info.isConstant);
    ASSERT(info.val.b32 == true);

    checkUnary("!!false");
    ASSERT(type == BoolType);
    ASSERT(info.isConstant);
    ASSERT(info.val.b32 == false);

    checkUnary("~0xffff");
    ASSERT(type == UntypedIntType);
    ASSERT(info.isConstant);
    ASSERT(info.val.u64 == ~0xffff);

#undef checkUnary
}

void test_checkConstantBinaryExpressions() {
    REINIT_COMPILER();
    Stmt *stmt;
    ExprInfo info;
    Type *type;
#define checkBinary(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    info = (ExprInfo){ .scope = pkg.scope }; \
    type = checkExprBinary((Expr *) stmt, &info, &pkg)

    checkBinary("1 + 2");
    ASSERT(type == UntypedIntType);
    ASSERT(info.isConstant);
    ASSERT(info.val.i64 == 3);

    checkBinary("1 + 2.0");
    ASSERT(type == UntypedFloatType);
    ASSERT(info.isConstant);
    ASSERT(info.val.f64 == 3.0);

    checkBinary("1 + 2.0 - 3");
    ASSERT(type == UntypedFloatType);
    ASSERT(info.isConstant);
    ASSERT(info.val.f64 == 0.f);

    checkBinary("1 / 0");
    ASSERT(ArrayLen(pkg.diagnostics.errors) == 1);
    ArrayFree(pkg.diagnostics.errors);

    checkBinary("-1 + -8");
    ASSERT(IsInteger(type));
    ASSERT(info.isConstant);
    ASSERT(info.val.i64 == -9);
}

void test_checkConstantTernaryExpression() {
    REINIT_COMPILER();
    Stmt *stmt;
    ExprInfo info;
    Type *type;
#define checkTernary(_CODE) \
    stmt = resetAndParseSingleStmt(_CODE); \
    info = (ExprInfo){ .scope = pkg.scope }; \
    type = checkExprTernary((Expr *) stmt, &info, &pkg)

    checkTernary("true ? 1 : 2");
    ASSERT(type == UntypedIntType);
    ASSERT(info.isConstant);
    ASSERT(info.val.i64 == 1);

    checkTernary("false ? 1.5 : 2.5");
    ASSERT(type == UntypedFloatType);
    ASSERT(info.isConstant);
    ASSERT(info.val.f64 == 2.5);
}

#undef pkg
#endif
