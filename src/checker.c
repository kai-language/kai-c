
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

void storeInfoConstant(Package *pkg, Decl *decl, Symbol *symbol, b8 isGlobal) {
    ASSERT(decl->kind == DeclKind_Constant);
    CheckerInfo info = {CheckerInfoKind_Constant, .Constant.symbol = symbol, .Constant.isGlobal = isGlobal};
    pkg->checkerInfo[decl->id] = info;
}

void storeInfoVariable(Package *pkg, Decl *decl, Symbol **symbols, b8 isGlobal) {
    ASSERT(decl->kind == DeclKind_Variable);
    CheckerInfo info = {CheckerInfoKind_Variable, .Variable.symbols = symbols, .Variable.isGlobal = isGlobal};
    pkg->checkerInfo[decl->id] = info;
}

void storeInfoIdent(Package *pkg, Expr *expr, Symbol *symbol) {
    ASSERT(expr->kind == ExprKind_Ident);
    CheckerInfo info = {CheckerInfoKind_Ident, .Ident.symbol = symbol};
    pkg->checkerInfo[expr->id] = info;
}

void storeInfoBasicExpr(Package *pkg, Expr *expr, Type *type) {
    ASSERT(expr->kind == ExprKind_LitInt);
    CheckerInfo info = {CheckerInfoKind_BasicExpr, .BasicExpr.type = type};
    pkg->checkerInfo[expr->id] = info;
}

b32 declareSymbol(Package *pkg, Scope *scope, const char *name, Symbol **symbol, u64 declId, Position *decl) {
    Symbol *old = Lookup(scope, name);
    if (old) {
        ReportError(pkg, RedefinitionError, *decl, "Duplicate definition of symbol %s", name);
        ReportNote(pkg, *old->decl, "Previous definition of %s", name);
        *symbol = old;
        return true;
    }

    Symbol *sym = ArenaAlloc(&pkg->arena, sizeof(Symbol));
    sym->name = name;
    sym->kind = SymbolKind_Invalid;
    sym->state = SymbolState_Resolving;
    sym->decl = decl;
    sym->declId = declId;

    MapSet(&scope->members, name, sym);

    *symbol = sym;
    
    return false;
}

b32 expectType(Package *pkg, Type *type, ExprInfo *info, Position pos) {
    if (info->mode != ExprMode_Type) {
        ReportError(pkg, InvalidMetatypeError, pos, "%s cannot be used as a type", DescribeTypeKind(type->kind));
    }
    return info->mode != ExprMode_Type;
}

Type *baseType(Type *type) {
repeat:
    if (type->kind == TypeKind_Alias) {
        type = type->Alias.symbol->type;
        goto repeat;
    }

    return type;
}

b32 isBoolean(Type *type) {
    type = baseType(type);
    return type->kind == TypeKind_Bool;
}

b32 isInteger(Type *type) {
    type = baseType(type);
    return type->kind == TypeKind_Int;
}

b32 isFloat(Type *type) {
    type = baseType(type);
    return type->kind == TypeKind_Float;
}

b32 isNumeric(Type *type) {
    type = baseType(type);
    return isInteger(type) || isFloat(type);
}

b32 isPointer(Type *type) {
    type = baseType(type);
    return type->kind == TypeKind_Pointer;
}

b32 isNilable(Type *type) {
    return baseType(type)->kind == TypeKind_Pointer;
}

b32 isNumericOrPointer(Type *type) {
    return isNumeric(type) || isPointer(type);
}

b32 isEnum(Type *type) {
    return baseType(type)->kind == TypeKind_Enum;
}

b32 isEnumFlags(Type *type) {
    return isEnum(type) && (baseType(type)->Flags & TypeFlag_EnumFlags) != 0;
}

b32 isBitbag(Type *type) {
    return isInteger(type) || isEnumFlags(type);
}

b32 isComparable(Type *type) {
    return isInteger(type) || isFloat(type) || isEnum(type);
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

// FIXME(vdka): We will need this now. We stopped this because we needed to pass a package, and updating the type was awkward.
//void convertUntyped(Package *pkg, Type **type, Type *target) {
//    if (*type == InvalidType || target == InvalidType) return;
//    if (((*type)->Flags & TypeFlag_Untyped) == 0) return;
//
//    if (target->Flags & TypeFlag_Untyped) {
//        // Both are untyped
//        i32 rank = TypeRank(*type);
//        i32 targetRank = TypeRank(target);
//        if (isNumeric(*type) && isNumeric(target)) {
//            if (rank < targetRank) {
//                *type = target;
//
//            }
//        } else if (rank != targetRank) {
//            goto error;
//        }
//    }
//
//error:
//    ReportError(pkg, TODOError, <#Position pos#>, <#const char *msg, ...#>)
//}

b32 (*unaryPredicates[NUM_TOKEN_KINDS])(Type *) = {
    [TK_Add] = isNumeric,
    [TK_Sub] = isNumeric,
    [TK_BNot] = isInteger,
    [TK_Not] = isNumericOrPointer,
    [TK_Lss] = isPointer,
};

b32 (*binaryPredicates[NUM_TOKEN_KINDS])(Type *) = {
    [TK_Add] = isNumeric,
    [TK_Sub] = isNumeric,
    [TK_Mul] = isNumeric,
    [TK_Div] = isNumeric,
    [TK_Rem] = isInteger,

    [TK_And] = isBitbag,
    [TK_Or]  = isBitbag,
    [TK_Xor] = isBitbag,
    [TK_Shl] = isBitbag,
    [TK_Shr] = isBitbag,

    [TK_Eql] = isEquatable,
    [TK_Neq] = isEquatable,

    [TK_Lss] = isComparable,
    [TK_Gtr] = isComparable,
    [TK_Leq] = isComparable,
    [TK_Geq] = isComparable,

    [TK_Land] = isBoolean,
    [TK_Lor]  = isBoolean,
};

b32 convert(Type *type, Type *target) {
    if (type == UntypedIntType) {
        return target == UntypedIntType || (target->kind == TypeKind_Int);
    }

    if (type == UntypedFloatType) {
        return target == UntypedFloatType || target->kind == TypeKind_Float;
    }

    // FIXME: Brett, this doesn't work for aliased types
    return type == target;
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
    ASSERT(scope);
    do {
        Symbol *symbol = MapGet(&scope->members, name);
        if (symbol) {
            return symbol;
        }

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
            if (symbol == TrueSymbol || symbol == FalseSymbol) {
                exprInfo->mode = ExprMode_Computed;
                break;
            }
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
        if (isInteger(exprInfo->desiredType) || isPointer(exprInfo->desiredType)) {
            exprInfo->val.u64 = lit.val;
        } else if (isFloat(exprInfo->desiredType)) {
            exprInfo->val.f64 = (f64)lit.val;
        } else {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Unable to convert type %s to expected type type %s", DescribeType(UntypedIntType), DescribeType(exprInfo->desiredType));
            goto error;
        }

        type = exprInfo->desiredType;
    }

    exprInfo->mode = ExprMode_Computed;
    storeInfoBasicExpr(pkg, expr, type);
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitFloat(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    Expr_LitFloat lit = expr->LitFloat;

    Type *type = UntypedFloatType;
    if (exprInfo->desiredType) {
        if (isFloat(exprInfo->desiredType)) {
            exprInfo->val.f64 = lit.val;
        } else {
            ReportError(pkg, InvalidConversionError, expr->start,
                        "Unable to convert type %s to expected type type %s", DescribeType(UntypedFloatType), DescribeType(exprInfo->desiredType));
            goto error;
        }

        type = exprInfo->desiredType;
    }

    exprInfo->mode = ExprMode_Computed;
    storeInfoBasicExpr(pkg, expr, type);
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitNil(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    exprInfo->mode = ExprMode_Nil;
    Type *desiredType = exprInfo->desiredType;

    if (!desiredType) {
        ReportError(pkg, TODOError, expr->start, "'nil' used without a contextual type");
        goto error;
    }

    if (!isNilable(desiredType)) {
        ReportError(pkg, NotNilableError, expr->start, "'nil' is not convertable to '%s'", DescribeType(exprInfo->desiredType));
        goto error;
    }

    storeInfoBasicExpr(pkg, expr, desiredType);
    exprInfo->mode = ExprMode_Nil; // TODO: Is a nil mode something that makes sense? Should it just be any other non addressable value?
    return desiredType;

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

    ForEach(func.result) {
        Type *type = checkExpr(pkg, it, &info);
        if (!expectType(pkg, type, &info, it->start)) isInvalid = true;
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
    ForEach(func.type->TypeFunction.params) {
        if (!it->key || it->key->kind != ExprKind_Ident) {
            ReportError(pkg, ParamNameMissingError, it->start, "Parameters for a function literal must be named");
            continue;
        }

        Symbol *symbol;
        declareSymbol(pkg, scope, it->key->Ident.name, &symbol, 0, &it->start);
        ArrayPush(paramSymbols, symbol);

        Type *type = checkExpr(pkg, it->value, &funcInfo);
        if (!expectType(pkg, type, &funcInfo, it->value->start)) continue;

        typeFlags |= type->Flags & TypeFlag_Variadic;
        typeFlags |= type->Flags & TypeFlag_CVargs;
        ArrayPush(paramTypes, type);
    }

    ForEach(func.type->TypeFunction.result) {
        Type *type = checkExpr(pkg, it, &funcInfo);
        if (!expectType(pkg, type, &funcInfo, it->start)) continue;
        ArrayPush(resultTypes, type);
    }

    UNIMPLEMENTED();
    return NULL;
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
                exprInfo->mode = ExprMode_Invalid;
                type = InvalidType;
                goto error;
            }

            if (expr->Unary.op == TK_Not) {
                type = BoolType;
            } else if (expr->Unary.op == TK_Lss) {
                type = type->Pointer.pointeeType;
            }
    }

    storeInfoBasicExpr(pkg, expr, type);
    exprInfo->mode = ExprMode_Computed;
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprBinary(Expr *expr, ExprInfo *exprInfo, Package *pkg) {
    ExprInfo lhsInfo = *exprInfo;
    ExprInfo rhsInfo = *exprInfo;
    Type *lhs = checkExpr(pkg, expr->Binary.lhs, &lhsInfo);
    Type *rhs = checkExpr(pkg, expr->Binary.rhs, &rhsInfo);

    if (lhsInfo.mode == ExprMode_Invalid || rhsInfo.mode == ExprMode_Invalid) {
        goto error;
    }

    if (lhsInfo.mode == ExprMode_Nil) {
        if (!isNilable(rhs)) {
            ReportError(pkg, TODOError, expr->start, "Cannot infer type for %s", DescribeExpr(expr->Binary.lhs));
        } else {
            GetExprInfo(pkg, expr->Binary.lhs)->BasicExpr.type = GetExprInfo(pkg, expr->Binary.rhs)->BasicExpr.type;
            lhs = rhs;
        }
    } else if (rhsInfo.mode == ExprMode_Nil) {
        if (!isNilable(rhs)) {
            ReportError(pkg, TODOError, expr->Binary.rhs->start, "Cannot infer type for %s", DescribeExpr(expr->Binary.rhs));
        } else {
            GetExprInfo(pkg, expr->Binary.rhs)->BasicExpr.type = GetExprInfo(pkg, expr->Binary.lhs)->BasicExpr.type;
            rhs = lhs;
        }
    }

    // FIXME: Handle untyped types

    if (convert(lhs, rhs)) {
        lhs = rhs;
    } else if (convert(rhs, lhs)) {
        rhs = lhs;
    }

    if (lhs != rhs) {
        if (lhs == InvalidType || rhs == InvalidType) goto error;

        ReportError(pkg, TypeMismatchError, expr->start, "Mismatched types %s and %s", DescribeExpr(expr->Binary.lhs), DescribeExpr(expr->Binary.rhs));
        goto error;
    }

    if (!binaryPredicates[expr->Binary.op](lhs)) {
        ReportError(pkg, InvalidBinaryOperationError, expr->Binary.pos, "Operation '%s' undefined for type %s", DescribeTokenKind(expr->Binary.op), DescribeExpr(expr->Binary.lhs));
        goto error;
    }

    Type *type = NULL;
    switch (expr->Binary.op) {
        case TK_Eql:
        case TK_Neq:
        case TK_Leq:
        case TK_Geq:
        case TK_Lss:
        case TK_Gtr:
        case TK_Lor:
        case TK_Land:
            if (exprInfo->desiredType && isBoolean(exprInfo->desiredType) && convert(lhs, exprInfo->desiredType)) {
                type = exprInfo->desiredType;
            } else {
                type = BoolType;
            }
            break;
        default:
            type = lhs;
    }

    if ((expr->Binary.op == TK_Div || expr->Binary.op == TK_Rem) && rhsInfo.isConstant && rhsInfo.val.i64 == 0) {
        ReportError(pkg, DivisionByZeroError, expr->Binary.rhs->start, "Division by zero");
    }

    storeInfoBasicExpr(pkg, expr, type);
    return type;

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

//        default:
//            break;
    }

    return type;
}

b32 checkDeclConstant(Package *pkg, Scope *scope, b32 isGlobal, Decl *declStmt) {
    Decl_Constant decl = declStmt->Constant;

    if (ArrayLen(decl.names) != 1) {
        ReportError(
            pkg, MultipleConstantDeclError, decl.start,
            "Constant declarations must declare at most one item"
        );

        if (ArrayLen(decl.names) > 0) {
            For (decl.names) {
                const char *name = decl.names[i]->name;
                Symbol *symbol = MapGet(&pkg->symbolMap, name);
                markSymbolInvalid(symbol);
            }
        }

        return false;
    }

    if (ArrayLen(decl.values) > 1) {
        ReportError(
            pkg, ArityMismatchError, decl.start,
            "Constant declarations only allow for a single value, but got %zu",
            ArrayLen(decl.values)
        );
        return false;
    }

    Type *expectedType = NULL;

    if (decl.type) {
        ExprInfo info = { .scope = scope };
        expectedType = checkExpr(pkg, decl.type, &info);
        if (info.mode == ExprMode_Unresolved) return true;

        expectType(pkg, expectedType, &info, decl.type->start);
    }

    Expr_Ident *name = decl.names[0];
    Expr *value = decl.values[0];

    Symbol *symbol;
    if (isGlobal) {
        symbol = MapGet(&pkg->symbolMap, name->name);
    } else {
        declareSymbol(pkg, scope, name->name, &symbol, declStmt->id, &declStmt->start);
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
    }

    ExprInfo info = {.scope = scope, .desiredType = expectedType};
    Type *type = checkExpr(pkg, value, &info);
    if (info.mode == ExprMode_Unresolved) {
        return true;
    }

    if (expectedType) {
        if (!convert(type, expectedType)) {
            ReportError(
                pkg, InvalidConversionError, value->start,
                "Unable to convert type %s to expected type type %s",
                DescribeType(type), DescribeType(expectedType)
            );
            markSymbolInvalid(symbol);
        }
    }

    markSymbolResolved(symbol, type);
    storeInfoConstant(pkg, declStmt, symbol, isGlobal);
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

    if (isGlobal) {
        For (var.names) {
            Symbol *symbol = MapGet(&pkg->symbolMap, var.names[i]->name);
            ArrayPush(symbols, symbol);
        }
    } else {
        For (var.names) {
            Symbol *symbol;
            // FIXME(Brett): figure out how I want to recover from a duplicate
            declareSymbol(pkg, scope, var.names[i]->name, &symbol, declStmt->id, &declStmt->start);
            ArrayPush(symbols, symbol);
        }
    }

    // NOTE: decl like `x, y: i32`
    if (ArrayLen(var.values) == 0) {
        ASSERT(expectedType);
        For (symbols) {
            symbols[i]->type = expectedType;
            symbols[i]->state = SymbolState_Resolved;
        }

        if (expectedType->kind == TypeKind_Array && expectedType->Array.length == -1) {
            ReportError(
                pkg, UninitImplicitArrayError, var.type->start, 
                "Implicit-length array must have an initial value"
            );
        }

        if (expectedType->kind == TypeKind_Function) {
            ReportError(
                pkg, UninitFunctionTypeError, var.type->start, 
                "Variables of a function type must be initialized"
            );
            ReportNote(pkg, var.type->start, "If you want an uninitialized function pointer use *%s instead", DescribeType(expectedType));
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

            if (expectedType && !convert(type, expectedType)) {
                ReportError(
                    pkg, InvalidConversionError, var.values[i]->start,
                    "Unable to convert type %s to expected type type %s",
                    DescribeType(type), DescribeType(expectedType)
                );
                markSymbolInvalid(symbols[i]);
            }

            if (info.mode == ExprMode_Type) {
                ReportError(pkg, MetatypeNotAnExprError, var.values[i]->start,
                    "Metatype is not a valid expression");
                markSymbolInvalid(symbols[i]);
                continue;
            }

            symbols[i]->type = expectedType ? expectedType : type;
            symbols[i]->kind = SymbolKind_Variable;
            symbols[i]->state = SymbolState_Resolved;
        }
    }

    storeInfoVariable(pkg, declStmt, symbols, isGlobal);

    return false;
}

b32 checkImportDecl(Package *pkg, Decl *declStmt) {
//    Decl_Import import = declStmt->Import;
    UNIMPLEMENTED();
    return false;
}

b32 check(Package *pkg, Stmt *stmt) {
    b32 shouldRequeue;

    Scope *scope = pkg->globalScope;

    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        shouldRequeue = checkDeclConstant(pkg, scope, true, (Decl *)stmt);
    } break;

    case StmtDeclKind_Variable: {
        shouldRequeue = checkDeclVariable(pkg, scope, true, (Decl *)stmt);
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
