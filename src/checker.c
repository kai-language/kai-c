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
    Val val;
};

void invalidateSymbol(Symbol *symbol) {
    if (symbol) {
        symbol->state = SymbolState_Resolved;
        symbol->kind = SymbolKind_Invalid;
    }
}

Inline
void resolveSymbol(Symbol *symbol, Type *type) {
    symbol->state = SymbolState_Resolved;
    symbol->type = type;
}

Inline
void storeDeclInfo(Package *pkg, Decl *decl, CheckerInfo info) {
    ASSERT_MSG_VA(decl->id, "AST of type %s doesn't store checker info. Maybe the macro in ast.h needs to be updated?", AstDescriptions[decl->kind]);
    pkg->checkerInfo[decl->id] = info;
}

Inline
void storeExprInfo(Package *pkg, Expr *expr, CheckerInfo info) {
    ASSERT_MSG_VA(expr->id, "AST of type %s doesn't store checker info. Maybe the macro in ast.h needs to be updated?", AstDescriptions[expr->kind]);
    pkg->checkerInfo[expr->id] = info;
}

void StoreInfoIdent(Package *pkg, Expr *expr, Symbol *symbol) {
    ASSERT(expr->kind == ExprKind_Ident);
    CheckerInfo info = {CheckerInfoKind_Ident, .Ident.symbol = symbol};
    pkg->checkerInfo[expr->id] = info;
}

void StoreInfoBasicExpr(Package *pkg, Expr *expr, Type *type) {
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

b32 (*unaryPredicates[NUM_TOKEN_KINDS])(Type *) = {
    [TK_Add] = isNumeric,
    [TK_Sub] = isNumeric,
    [TK_BNot] = isInteger,
    [TK_Not] = isNumericOrPointer,
    [TK_Lss] = isPointer,
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

    StoreInfoIdent(pkg, expr, symbol);

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
    StoreInfoBasicExpr(pkg, expr, type);
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
    StoreInfoBasicExpr(pkg, expr, type);
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

    StoreInfoBasicExpr(pkg, expr, desiredType);
    exprInfo->mode = ExprMode_Nil; // TODO: Is a nil mode something that makes sense? Should it just be any other non addressable value?
    return desiredType;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkVariadicType(Package *pkg, Expr *expr, ExprInfo *info) {
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

Type *checkTypeFunction(Package *pkg, Expr *expr, ExprInfo *exprInfo) {
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

    StoreInfoBasicExpr(pkg, expr, type);
    exprInfo->mode = ExprMode_Type;
    return type;

error:
    exprInfo->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkLitFunction(Package *pkg, Expr *funcExpr, ExprInfo *exprInfo) {
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

Type *checkExpr(Package *pkg, Expr *expr, ExprInfo *exprInfo) {
    Type *type = NULL;
    switch (expr->kind) {
        case ExprKind_Ident:
            type = checkExprIdent(expr, exprInfo, pkg);
            break;

        case ExprKind_LitInt:
            type = checkExprLitInt(expr, exprInfo, pkg);
            break;

        case ExprKind_LitFloat: {
            Expr_LitFloat lit = expr->LitFloat;

            Type *type = InvalidType;

            if (exprInfo->desiredType) {
                if (isInteger(exprInfo->desiredType)) {
                    exprInfo->val.u64 = (u64)lit.val;
                } else if (isFloat(exprInfo->desiredType)) {
                    exprInfo->val.f64 = lit.val;
                } else {
                    ReportError(pkg, InvalidConversionError, expr->start,
                                "Unable to convert type %s to expected type type %s",
                                DescribeType(UntypedFloatType), DescribeType(exprInfo->desiredType));
                    return InvalidType;
                }

                type = exprInfo->desiredType;
            } else {
                type = UntypedFloatType;
            }

            exprInfo->mode = ExprMode_Computed;
            StoreInfoBasicExpr(pkg, expr, type);
            return type;
        } break;

        case ExprKind_LitNil:
            type = checkExprLitNil(expr, exprInfo, pkg);
            break;

        case ExprKind_LitFunction:
            type = checkLitFunction(pkg, expr, exprInfo);
            break;

        case ExprKind_TypeVariadic:
            type = checkVariadicType(pkg, expr, exprInfo);
            break;

        case ExprKind_TypePointer: {
            Type *desiredType = exprInfo->desiredType;
            exprInfo->desiredType = isPointer(exprInfo->desiredType) ? exprInfo->desiredType->Pointer.pointeeType : NULL;
            Type *type = checkExpr(pkg, expr->TypePointer.type, exprInfo);
            expectType(pkg, type, exprInfo, expr->TypePointer.type->start);
            exprInfo->desiredType = NULL;

            if (exprInfo->mode == ExprMode_Unresolved) return InvalidType;
            expectType(pkg, type, exprInfo, expr->TypePointer.type->start);
            if (exprInfo->mode != ExprMode_Type) {
                ReportError(pkg, InvalidPointeeTypeError, expr->start,
                            "'%s' is not a valid pointee type", DescribeType(type));
                goto exprIsInvalid;
            } else if (type == VoidType) {
                ReportError(pkg, TODOError, expr->TypePointer.type->start, "Kai does not use void * for raw pointers instead use rawptr");
            }

            if (desiredType == RawptrType) type = RawptrType;

            type = NewTypePointer(TypeFlag_None, type);
            exprInfo->mode = ExprMode_Type;

            return type;
    } break;

    case ExprKind_Unary: {
        Type *type = checkExpr(pkg, expr->Unary.expr, exprInfo);
        if (exprInfo->mode == ExprMode_Unresolved) return InvalidType;

        switch (expr->Unary.op) {
            case TK_And: {
                if (exprInfo->mode != ExprMode_Addressable) {
                    ReportError(pkg, AddressOfNonAddressableError, expr->start,
                                "Cannot take address of %s", DescribeExpr(expr->Unary.expr));
                    exprInfo->mode = ExprMode_Invalid;
                    type = InvalidType;
                    goto exprIsInvalid;
                }
                type = NewTypePointer(TypeFlag_None, type);
                break;
            }
            default: {
                if (!unaryPredicates[expr->Unary.op]) {
                    ReportError(pkg, InvalidUnaryOperationError, expr->start,
                                "Operation '%s' undefined for %s", DescribeTokenKind(expr->Unary.op), DescribeExpr(expr->Unary.expr));
                    exprInfo->mode = ExprMode_Invalid;
                    type = InvalidType;
                    goto exprIsInvalid;
                }

                switch (expr->Unary.op) {
                    case TK_Not:
                        type = BoolType;
                    case TK_Lss: {
                        type = NewTypePointer(TypeFlag_None, type);
                    default:
                        break;
                    }
                }
            }
        }

        StoreInfoBasicExpr(pkg, expr, type);
        exprInfo->mode = ExprMode_Computed;

        return type;
    } break;

    default:
        break;
    }

    return type;

exprIsInvalid:
    return InvalidType;
}

b32 checkConstDecl(Package *pkg, Scope *scope, b32 isGlobal, Decl *declStmt) {
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
                invalidateSymbol(symbol);
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
        Type *type = checkTypeFunction(pkg, func.type, &info);
        if (info.mode == ExprMode_Unresolved) return true;

        expectType(pkg, type, &info, func.type->start);

        resolveSymbol(symbol, type);
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
            invalidateSymbol(symbol);
        }
    }

    resolveSymbol(symbol, type);

    CheckerInfo solve = {
        .kind = CheckerInfoKind_Decl,
        .Decl.symbol = symbol,
        .Decl.isGlobal = isGlobal
    };
    storeDeclInfo(pkg, declStmt, solve);

    return false;
}

b32 checkVarDecl(Package *pkg, Scope *scope, b32 isGlobal, Decl *declStmt) {
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
    }
    
    else {
        if (ArrayLen(var.values) != ArrayLen(var.names)) {
            // TODO: ensure that this is a function call otherwise report this error
            ReportError(
                pkg, ArityMismatchError, var.start, 
                "The amount of identifiers (%zu) doesn't match the amount of values (%zu)", 
                ArrayLen(var.names), ArrayLen(var.values)
            );

            For (symbols) {
                invalidateSymbol(symbols[i]);
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
                invalidateSymbol(symbols[i]);
            }

            if (type->kind == TypeKind_Metatype) {
                ReportError(
                    pkg, MetatypeNotAnExprError, var.values[i]->start, 
                    "Metatype is not a valid expression"
                );
                invalidateSymbol(symbols[i]);
                continue;
            }

            symbols[i]->type = expectedType ? expectedType : type;
            symbols[i]->kind = SymbolKind_Variable;
            symbols[i]->state = SymbolState_Resolved;
        }
    }

    CheckerInfo solve = {
        .kind = CheckerInfoKind_DeclList,
        .DeclList.symbols = symbols,
        .DeclList.isGlobal = isGlobal   
    };
    storeDeclInfo(pkg, declStmt, solve);

    return false;
}

b32 checkImportDecl(Package *pkg, Decl *declStmt) {
    Decl_Import import = declStmt->Import;
    UNIMPLEMENTED();
    return false;
}

b32 check(Package *pkg, Stmt *stmt) {
    b32 shouldRequeue;

    Scope *scope = pkg->globalScope;

    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        shouldRequeue = checkConstDecl(pkg, scope, true, (Decl *)stmt);
    } break;

    case StmtDeclKind_Variable: {
        shouldRequeue = checkVarDecl(pkg, scope, true, (Decl *)stmt);
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
