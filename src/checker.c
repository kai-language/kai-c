
typedef u8 ExprMode;
enum Enum_ExprMode {
    // Signals to the caller in the checker
    ExprMode_Invalid,
    ExprMode_Unresolved,

    // Non values
    ExprMode_Import,
    ExprMode_Library,

    // Valid values
    ExprMode_Type,
    ExprMode_Value,

    // What C would refer to as lvalues
    ExprMode_Addressable,

    // NOTE: Order matters if anything is above addressable must also be addressable
};

typedef u8 CheckerContextFlag;
#define CheckerContextFlag_Constant     0x01
#define CheckerContextFlag_LoopClosest  0x02
#define CheckerContextFlag_UnresolvedOk 0x04
#define CheckerContextFlag_ArrayLength  0x08

typedef struct CheckerContext CheckerContext;
struct CheckerContext {
    Scope *const scope;
    Type *desiredType;
    ExprMode mode;

    Stmt *swtch;
    Stmt *nextCase;
    Stmt *loop;

    CheckerContextFlag flags;

    Val val;
};

b32 IsConstant(CheckerContext *ctx) {
    return (ctx->flags & CheckerContextFlag_Constant) != 0;
}

ExprMode exprModeForSymbol(Symbol *symbol) {
    static ExprMode table[SYMBOL_KIND_COUNT] = {
        [SymbolKind_Invalid] = ExprMode_Invalid,
        [SymbolKind_Import] = ExprMode_Import,
        [SymbolKind_Library] = ExprMode_Library,
        [SymbolKind_Label] = ExprMode_Invalid, // TODO: @goto
        [SymbolKind_Type] = ExprMode_Type,
        [SymbolKind_Constant] = ExprMode_Value,
        [SymbolKind_Variable] = ExprMode_Addressable,
    };

    return table[symbol->kind];
}

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
    if (symbol->kind == SymbolKind_Type) {
        type->Symbol = symbol;
    }
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

void storeInfoForeign(Package *pkg, Decl *decl, Symbol *symbol) {
    ASSERT(decl->kind == DeclKind_Foreign);
    CheckerInfo info = {CheckerInfoKind_Foreign, .Foreign.symbol = symbol};
    pkg->checkerInfo[decl->id] = info;
}

void storeInfoIdent(Package *pkg, Expr *expr, Symbol *symbol) {
    ASSERT(expr->kind == ExprKind_Ident);
    CheckerInfo info = {CheckerInfoKind_Ident, .Ident.symbol = symbol};
    pkg->checkerInfo[expr->id] = info;
}

void storeInfoBasicExpr(Package *pkg, Expr *expr, Type *type, CheckerContext *ctx) {
    CheckerInfo info = {CheckerInfoKind_BasicExpr, .BasicExpr.type = type};
    info.BasicExpr.isConstant = IsConstant(ctx);
    info.BasicExpr.val = ctx->val;
    pkg->checkerInfo[expr->id] = info;
}

void storeInfoSelector(Package *pkg, Expr *expr, Type *type, SelectorKind kind, SelectorValue value, CheckerContext *ctx) {
    CheckerInfo info = {CheckerInfoKind_Selector, .Selector.type = type};
    info.Selector.kind = kind;
    info.Selector.value = value;
    info.Selector.isConstant = IsConstant(ctx);
    info.Selector.val = ctx->val;
    pkg->checkerInfo[expr->id] = info;
}

void storeInfoLabel(Package *pkg, Stmt *stmt, Symbol *symbol) {
    ASSERT(stmt->kind == StmtKind_Label);
    CheckerInfo info = {CheckerInfoKind_Label, .Label.symbol = symbol};
    pkg->checkerInfo[stmt->id] = info;
}

void storeInfoGoto(Package *pkg, Stmt *stmt, Symbol *target) {
    ASSERT(stmt->kind == StmtKind_Goto);
    CheckerInfo info = {CheckerInfoKind_Goto, .Goto.target = target};
    pkg->checkerInfo[stmt->id] = info;
}

void storeInfoFor(Package *pkg, Stmt *stmt, Symbol *continueTarget, Symbol *breakTarget) {
    ASSERT(stmt->kind == StmtKind_For);
    CheckerInfo info = {CheckerInfoKind_For, .For.continueTarget = continueTarget, .For.breakTarget = breakTarget};
    pkg->checkerInfo[stmt->id] = info;
}

void storeInfoSwitch(Package *pkg, Stmt *stmt, Symbol *breakTarget) {
    ASSERT(stmt->kind == StmtKind_Switch);
    CheckerInfo info = {CheckerInfoKind_Switch, .Switch.breakTarget = breakTarget};
    pkg->checkerInfo[stmt->id] = info;
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

b32 declareSymbol(Package *pkg, Scope *scope, const char *name, Symbol **symbol, Decl *decl) {
    Symbol *old = Lookup(scope, name);
    if (old) {
        ReportError(pkg, RedefinitionError, decl->start, "Duplicate definition of symbol %s", name);
        ReportNote(pkg, old->decl->start, "Previous definition of %s", name);
        if (symbol) *symbol = old;
        return true;
    }

    Symbol *sym = ArenaAlloc(&pkg->arena, sizeof(Symbol));
    sym->name = name;
    sym->kind = SymbolKind_Invalid;
    sym->state = SymbolState_Resolving;
    sym->decl = decl;

    MapSet(&scope->members, name, sym);

    if (symbol) *symbol = sym;
    
    return false;
}

Symbol *declareResolvedSymbol(Package *pkg, Scope *scope, Type *type, const char *name, Decl *decl) {
    Symbol *symbol;
    declareSymbol(pkg, scope, name, &symbol, decl);
    markSymbolResolved(symbol, type);
    return symbol;
}

Symbol *declareLabelSymbol(Package *pkg, Scope *scope, const char *name) {
    Symbol *symbol;
    declareSymbol(pkg, scope, name, &symbol, NULL);
    symbol->kind = SymbolKind_Label;
    markSymbolResolved(symbol, RawptrType);
    return symbol;
}

b32 expectType(Package *pkg, Type *type, CheckerContext *ctx, Position pos) {
    if (ctx->mode != ExprMode_Type) {
        ReportError(pkg, NotATypeError, pos, "%s cannot be used as a type", DescribeTypeKind(type->kind));
    }
    return true;
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

b32 isFunction(Type *type) {
    return type->kind == TypeKind_Function;
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

b32 isTuple(Type *type) {
    return type->kind == TypeKind_Tuple;
}

b32 isArray(Type *type) {
    return type->kind == TypeKind_Array;
}

b32 isSlice(Type *type) {
    return type->kind == TypeKind_Slice;
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

b32 canCoerce(Type *type, Type *target, CheckerContext *ctx) {
    if (TypesIdentical(type, target)) return true;
    if (target->kind == TypeKind_Any) return true;

    if (isTuple(type) && ArrayLen(type->Tuple.types) == 1) {
        return canCoerce(type->Tuple.types[0], target, ctx);
    }

    // Any numeric or pointer type can coerce to Bool type
    if (isNumericOrPointer(type) && isBoolean(target)) return true;

    if (IsInteger(target) && IsInteger(type)) {
        if (IsSigned(type) && !IsSigned(target)) {
            // Signed to Unsigned
            // Source is a positive constant less than Target's Max Value
            if (IsConstant(ctx) && ctx->val.u64 <= MaxValueForIntOrPointerType(target)) {
                return SignExtend(type, target, ctx->val) >= 0;
            }
            // Coercion from a signed integer to an unsigned requires a cast.
            return false;
        }

        if (!IsSigned(type) && IsSigned(target)) {
            // Unsigned to Signed
            // Source is a constant less than Target's Max Value
            if (IsConstant(ctx) && ctx->val.u64 <= MaxValueForIntOrPointerType(target)) return true;
            // Target's Max Value is larger than the Source's
            return type->Width < target->Width;
        }

        // The two integer types have the same signedness. The target must be the same or a larger size.
        return type->Width <= target->Width;
    }

    if (type->kind == TypeKind_Enum || IsInteger(target)) {
        return canCoerce(type->Enum.backingType, target, ctx);
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
#endif

b32 representValueSilently(CheckerContext *ctx, Expr *expr, Type *type, Type *target, Package *pkg) {
    // TODO: Implement this properly
    // Also tests for it.
    return true;
}

Conversion conversion(Type *type, Type *target) {
    Conversion result = 0;
    if (type->kind == target->kind) {
        result |= ConversionKind_Same;
        switch (type->kind) {
            case TypeKind_Float:
                result |= ConversionFlag_Float;
                if (type->Width < target->Width) result |= ConversionFlag_Extend;
                return result;

            case TypeKind_Int:
                if (type->Width < target->Width) result |= ConversionFlag_Extend;
                if (IsSigned(type)) result |= ConversionFlag_Signed;
                return result;

            default:
                return result;
        }
    }

    if (type->kind == TypeKind_Tuple && ArrayLen(type->Tuple.types) == 1) {
        return conversion(type->Tuple.types[0], target);
    }

    if (isBoolean(target)) {
        result |= ConversionKind_Bool;
        return result;
    }

    // TODO: All to Union

    if (IsInteger(type) && IsFloat(target)) {
        result |= ConversionKind_ItoF;
        if (IsSigned(type)) result |= ConversionFlag_Signed;
        return result;
    }

    if (IsFloat(type) && IsInteger(target)) {
        result |= ConversionKind_FtoI & ConversionFlag_Float;
        if (IsSigned(type)) result |= ConversionFlag_Signed;
        return result;
    }

    if (isPointer(type) && IsInteger(target)) {
        result |= ConversionKind_PtoI;
        return result;
    }

    if (IsInteger(target) && isPointer(type)) {
        result |= ConversionKind_ItoP;
        return result;
    }

    // TODO: Integer to enum and vica versa

    // TODO: function to pointer and vica versa

    PANIC("Unhandled or prohibited conversion");
    return ConversionKind_None;
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

b32 coerceTypeSilently(Expr *expr, CheckerContext *ctx, Type **type, Type *target, Package *pkg) {
    if (*type == InvalidType || target == InvalidType)
        return false;
    if (TypesIdentical(*type, target))
        return true;

    if (!canCoerce(*type, target, ctx))
        return false;

    if (IsConstant(ctx))
        convertValue(*type, target, &ctx->val);

    changeTypeOrMarkConversionForExpr(expr, *type, target, pkg);

    *type = target;

    return true;
}

b32 coerceType(Expr *expr, CheckerContext *ctx, Type **type, Type *target, Package *pkg) {

    b32 success = coerceTypeSilently(expr, ctx, type, target, pkg);
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

    if ((isFunction(source) || isFunction(target)) && (isPointer(source) || isPointer(target))) return true;
    // TODO: Enum <-> Integer

    return false;
}

b32 cast(Type *source, Type *target, CheckerContext *ctx) {
    if (source == InvalidType || target == InvalidType) return false;

    if (!canCast(source, target)) return false;

    if (IsConstant(ctx)) {
        convertValue(source, target, &ctx->val);
        ctx->val.u64 &= BITMASK(u64, target->Width);
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

Type *TypeFromCheckerInfo(CheckerInfo info) {
    switch (info.kind) {
        case CheckerInfoKind_BasicExpr:
            return info.BasicExpr.type;
        case CheckerInfoKind_Ident:
            return info.Ident.symbol->type;
        case CheckerInfoKind_Selector:
            return info.Selector.type;
        default:
            return NULL;
    }
}

Type *checkExpr(Expr *expr, CheckerContext *ctx, Package *pkg);
void  checkStmt(Stmt *stmt, CheckerContext *ctx, Package *pkg);

Type *checkExprIdent(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Ident);
    Expr_Ident ident = expr->Ident;
    Symbol *symbol = Lookup(ctx->scope, ident.name);
    if (!symbol) {
        ReportError(pkg, UndefinedIdentError, expr->start, "Use of undefined identifier '%s'", ident.name);
        ctx->mode = ExprMode_Invalid;
        return InvalidType;
    }

    symbol->used = true;
    switch (symbol->state) {
        case SymbolState_Unresolved:
            // TODO: Context switch to the declaration's context

            ASSERT_MSG(symbol->decl->owningScope, "Unresolved non top level symbol");
            CheckerContext declCtx = { .scope = symbol->decl->owningScope };
            checkStmt((Stmt *) symbol->decl, &declCtx, pkg);
            break;

        case SymbolState_Resolving:
            // TODO: For cyclic types we need a ctx->hasIndirection to check and see if
            // We have a cyclic reference error.
            ReportError(pkg, ReferenceToDeclarationError, expr->start,
                        "Declaration initial value refers to itself");
            break;

        case SymbolState_Resolved:
            break;
    }

    storeInfoIdent(pkg, expr, symbol);

    switch (symbol->kind) {
        case SymbolKind_Invalid:
            ctx->mode = ExprMode_Invalid;
            break;

        case SymbolKind_Import:
            ctx->mode = ExprMode_Import;
            break;

        case SymbolKind_Library:
            ctx->mode = ExprMode_Library;
            break;

        case SymbolKind_Label:
            UNIMPLEMENTED();
            break;

        case SymbolKind_Type:
            ctx->mode = ExprMode_Type;
            break;

        case SymbolKind_Constant:
            ctx->mode = ExprMode_Value;
            ctx->flags |= CheckerContextFlag_Constant;
            break;

        case SymbolKind_Variable:
            ctx->mode = ExprMode_Addressable;
            break;
    }

    ctx->val = symbol->val;
    return symbol->type;

unresolved:
    ctx->mode = ExprMode_Unresolved;
    return NULL;
}

Type *checkExprLitInt(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_LitInt);
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
        }
    } else {
        ctx->val.u64 = lit.val;
    }

    ctx->mode = ExprMode_Value;
    ctx->flags |= CheckerContextFlag_Constant;
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitFloat(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_LitFloat);
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
            // TODO: Check if it's possible through casting and add note that you can cast to make the conversion occur @ErrorQuality
            goto error;
        }

        type = ctx->desiredType;
    } else {
        ctx->val.f64 = lit.val;
    }

    ctx->mode = ExprMode_Value;
    ctx->flags |= CheckerContextFlag_Constant;
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitNil(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_LitNil);
    if (ctx->desiredType && !isNilable(ctx->desiredType)) {
        ReportError(pkg, NotNilableError, expr->start,
                    "'nil' is not convertable to '%s'", DescribeType(ctx->desiredType));
        goto error;
    }

    Type *type = ctx->desiredType;
    if (!type) type = RawptrType;

    ctx->mode = ExprMode_Value;
    ctx->flags |= CheckerContextFlag_Constant;
    ctx->val = (Val){ .u64 = 0 };

    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLitString(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_LitString);
    Type *type = StringType;
    if (TypesIdentical(RawptrType, ctx->desiredType)) {
        type = ctx->desiredType;
    }
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->flags |= CheckerContextFlag_Constant;
    ctx->mode = ExprMode_Value;
    ctx->val.ptr = (uintptr_t) expr->LitString.val;
    return type;
}

Type *checkExprTypeVariadic(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypeVariadic);
    Type *type = checkExpr(expr->TypeVariadic.type, ctx, pkg);
    if (!expectType(pkg, type, ctx, expr->TypeVariadic.type->start)) goto error;
    TypeFlag flags = expr->TypeVariadic.flags & TypeVariadicFlag_CVargs ? TypeFlag_CVargs : TypeFlag_None;
    type = NewTypeSlice(flags, type);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTypeEnum(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypeEnum);
    Expr_TypeEnum enm = expr->TypeEnum;

    Type *backingType = NULL;
    if (enm.explicitType) {
        Type *type = checkExpr(enm.explicitType, ctx, pkg);
        expectType(pkg, type, ctx, enm.explicitType->start);
        if (IsInteger(type)) {
            backingType = type;
        } else {
            ReportError(
                pkg, TypeMismatchError, enm.explicitType->start,
                "Enum backing type must be an integer. Got: %s",
                DescribeType(type)
            );
        }
    }

    b32 firstValue = true;
    b32 hasMinMax = false;
    u64 maxValue;

    if (backingType) {
        // TODO: flags
        maxValue = MaxValueForIntOrPointerType(backingType);
        hasMinMax = true;
    }

    DynamicArray(EnumField) fields = NULL;
    ArrayFit(fields, ArrayLen(enm.items));

    u64 currentValue = 0;
    u64 largestValue = 0;

    For(enm.items) {
        EnumItem item = enm.items[i];

        if (item.init) {
            CheckerContext itemCtx = {.scope = ctx->scope, .desiredType = backingType};
            Type *type = checkExpr(item.init, &itemCtx, pkg);

            if (!IsConstant(&itemCtx)) {
                ReportError(
                    pkg, TODOError, item.init->start,
                    "Enum cases must be a constant value");
                continue;
            }

            if (backingType && !canCoerce(type, backingType, &itemCtx)) {
                ReportError(
                    pkg, InvalidConversionError, item.init->start,
                    "Cannot convert %s to type %s",
                    DescribeExpr(item.init), DescribeType(backingType)
                );
                continue;
            }

            u64 val = itemCtx.val.u64;
            currentValue = val;
            largestValue = MAX(largestValue, val);
        }

        if (hasMinMax && currentValue > maxValue) {
            printf("oops!\n");
            continue;
        }

        ArrayPush(fields, (EnumField){.name = item.name, .val = currentValue});
        currentValue++;
    }

    backingType = backingType ? backingType : SmallestIntTypeForPositiveValue(largestValue);
    Type *type = NewTypeEnum(TypeFlag_None, backingType, fields);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Type;
    return type;
}

Type *checkExprTypeFunction(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypeFunction);
    Expr_TypeFunction func = expr->TypeFunction;
    TypeFlag flags = TypeFlag_None;

    b32 isInvalid = false;

    DynamicArray(Type *) params = NULL;
    ArrayFit(params, ArrayLen(func.params));

    CheckerContext paramCtx = { pushScope(pkg, ctx->scope) };
    For (func.params) {
        Type *type = checkExpr(func.params[i]->value, &paramCtx, pkg);
        if (!expectType(pkg, type, &paramCtx, func.params[i]->start)) isInvalid = true;

        flags |= type->Flags & TypeFlag_Variadic;
        flags |= type->Flags & TypeFlag_CVargs;
        ArrayPush(params, type);
    }

    DynamicArray(Type *) returnTypes = NULL;
    ArrayFit(returnTypes, ArrayLen(func.result));

    ForEach(func.result, Expr *) {
        Type *type = checkExpr(it, &paramCtx, pkg);
        if (!expectType(pkg, type, &paramCtx, it->start)) {
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

Type *checkExprLitFunction(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_LitFunction);
    Expr_LitFunction func = expr->LitFunction;
    Scope *parameterScope = pushScope(pkg, ctx->scope);
    CheckerContext paramCtx = { parameterScope };

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
        declareSymbol(pkg, parameterScope, it->key->Ident.name, &symbol, (Decl *) it);
        symbol->kind = SymbolKind_Variable;
        ArrayPush(paramSymbols, symbol);

        Type *type = checkExpr(it->value, &paramCtx, pkg);
        if (!expectType(pkg, type, &paramCtx, it->value->start)) continue;
        markSymbolResolved(symbol, type);

        // TODO: Support unnamed parameters ($0, $1, $2)
        storeInfoIdent(pkg, it->key, symbol);

        typeFlags |= type->Flags & TypeFlag_Variadic;
        typeFlags |= type->Flags & TypeFlag_CVargs;
        ArrayPush(paramTypes, type);
    }

    ForEach(func.type->TypeFunction.result, Expr *) {
        Type *type = checkExpr(it, &paramCtx, pkg);
        if (!expectType(pkg, type, &paramCtx, it->start)) continue;
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

    CheckerContext bodyCtx = { bodyScope, .desiredType = tuple };
    ForEach(func.body->stmts, Stmt *) {
        checkStmt(it, &bodyCtx, pkg);
        if (bodyCtx.mode == ExprMode_Unresolved) goto unresolved;
    }

    Type *type = NewTypeFunction(typeFlags, paramTypes, resultTypes);
    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Type;
    return type;

unresolved:
    ctx->mode = ExprMode_Unresolved;
    return NULL;
}

STATIC_ASSERT(offsetof(Type_Array, elementType) == offsetof(Type_Slice, elementType), "elementTypes must be at equal offset");
Type *checkExprLitCompound(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_LitCompound);
    Type *type = ctx->desiredType;

    if (expr->LitCompound.type) {
        type = checkExpr(expr->LitCompound.type, ctx, pkg);
        if (ctx->mode == ExprMode_Unresolved) return NULL;
        expectType(pkg, type, ctx, expr->LitCompound.type->start);
    }

    size_t maxIndex = 0;
    size_t currentIndex = 0;
    ForEach(expr->LitCompound.elements, Expr_KeyValue *) {
        Type *expectedValueType = NULL;
        if (it->key) {
            switch (type->kind) {
                case TypeKind_Array:
                case TypeKind_Slice:
                    if (!(it->flags & KeyValueFlag_Index)) {
                        ReportError(pkg, ArrayCompoundMissingIndexError, it->key->start,
                                    "Array or Slice key should be surrounded in `[]`");
                        goto checkValue;
                    }

                    CheckerContext indexCtx = { ctx->scope, .desiredType = U64Type };

                    Type *indexType = checkExpr(it->key, &indexCtx, pkg);
                    coerceType(it->key, &indexCtx, &indexType, U64Type, pkg);

                    // TODO: Should we support non constants?
                    // On one hand if we have literals for dictionaries we would expect runtime keys, and then
                    //  you'd expect them here too.
                    // On the other hand you would go from static values where the KeyValue pair after a pair with [4]
                    //  would be 5 if no key is specified. Would it be wise to make this true for runtime values too?
                    if (!(indexCtx.flags & CheckerContextFlag_Constant)) {
                        UNIMPLEMENTED();
                    }
                    currentIndex = (size_t) indexCtx.val.u64;

                    // Allow 0 length arrays to be indexed freely
                    if (type->kind == TypeKind_Array && type->Array.length && type->Array.length <= currentIndex) {
                        ReportError(pkg, TODOError, it->value->start,
                                    "Array index %llu is beyond the max index of %llu for type %s",
                                    currentIndex, type->Array.length - 1, DescribeType(type));
                    }

                    expectedValueType = type->Slice.elementType;
                    it->info = (void *) currentIndex;
                    break;

                case TypeKind_Struct:
                    if (it->flags & KeyValueFlag_Index) {
                        ReportError(pkg, TODOError, it->key->start, "Cannot initalize struct members using index");
                        goto checkValue;
                    }

                    if (it->key->kind != ExprKind_Ident) {
                        ReportError(pkg, TODOError, it->key->start,
                                    "Expected identifier for field name for type %s", DescribeType(type));
                        goto checkValue;
                    }

                    const char *fieldName = it->key->Ident.name;

                    StructFieldLookupResult result = StructFieldLookup(type->Struct, fieldName);
                    if (!result.field) {
                        ReportError(pkg, TODOError, it->key->start,
                                    "Type %s has no field named %s", DescribeType(type), fieldName);
                    }

                    it->info = result.field;
                    expectedValueType = result.field->type;
                    break;
            }
        } else {
            switch (type->kind) {
                case TypeKind_Struct: {
                    TypeField *field = type->Struct.members[currentIndex];
                    it->info = field;
                    expectedValueType = field->type;
                    break;
                case TypeKind_Array:
                    // Allow 0 length arrays to be indexed freely
                    if (type->Array.length && type->Array.length <= currentIndex) {
                        ReportError(pkg, TODOError, it->value->start,
                                    "Array index %llu is beyond the max index of %llu for type %s",
                                    currentIndex, type->Array.length - 1, DescribeType(type));
                    }
                    // fallthrough
                case TypeKind_Slice:
                    it->info = (void *) currentIndex;
                    break;
                }
            }
        }

    checkValue:;
        Type *previousDesiredType = ctx->desiredType;
        ctx->desiredType = expectedValueType ? expectedValueType : type->Slice.elementType;
        Type *valueType = checkExpr(it->value, ctx, pkg);
        coerceType(it->value, ctx, &valueType, ctx->desiredType, pkg);
        ctx->desiredType = previousDesiredType;
        maxIndex = MAX(maxIndex, currentIndex);
        currentIndex += 1;
    }

    ctx->mode = ExprMode_Value; // TODO: We want to make &Foo{} possible, does that mean we should make this addressable
    ctx->flags &= ~CheckerContextFlag_Constant; // NOTE: Currently constant composite literals are unsupported
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return type;
}

Type *checkExprTypePointer(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypePointer);
    // TODO: Should we be doing desired type things for types themselves?
    Type *desiredType = ctx->desiredType;
    if (desiredType && isPointer(desiredType)) {
        ctx->desiredType = desiredType->Pointer.pointeeType;
    }
    CheckerContextFlag prevFlags = ctx->flags;
    ctx->flags |= CheckerContextFlag_UnresolvedOk;
    Type *type = checkExpr(expr->TypePointer.type, ctx, pkg);

    expectType(pkg, type, ctx, expr->TypePointer.type->start);
    if (ctx->mode != ExprMode_Type) {
        ReportError(pkg, InvalidPointeeTypeError, expr->start,
                    "'%s' is not a valid pointee type", DescribeType(type));
        goto error;
    } else if (type == VoidType) {
        ReportError(pkg, InvalidPointeeTypeError, expr->TypePointer.type->start,
                    "Kai does not use void * for raw pointers instead use rawptr");
    }

    if (desiredType == RawptrType) type = RawptrType;

    ctx->flags = prevFlags;

    type = NewTypePointer(TypeFlag_None, type);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTypeArray(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypeArray);

    // NOTE: Later on we will check ctx->Flags & CheckerContextFlag_ValIsArrayLength
    u64 length = ctx->val.u64;

    Type *type = checkExpr(expr->TypeArray.type, ctx, pkg);
    expectType(pkg, type, ctx, expr->TypeArray.type->start);
    if (ctx->mode != ExprMode_Type) {
        goto error;
    } else if (type->Width == 0) {
        ReportError(pkg, ZeroWidthArrayElementError, expr->TypeArray.type->start,
                    "Arrays of zero width elements are not permitted");
    }

    TypeFlag flags = TypeFlag_None;
    if (expr->TypeArray.length) {
        Type *previousDesiredType = ctx->desiredType;
        ctx->desiredType = U64Type;
        Type *lengthType = checkExpr(expr->TypeArray.length, ctx, pkg);
        ctx->desiredType = previousDesiredType;
        coerceType(expr->TypeArray.length, ctx, &lengthType, U64Type, pkg);
        length = ctx->val.u64;

        if (!(ctx->flags & CheckerContextFlag_Constant)) {
            ReportError(pkg, NonConstantArrayLengthError, expr->TypeArray.length->start,
                        "An arrays length must be a constant value");
            ReportNote(pkg, expr->start, "To infer a length for the array type from the context use `..` as the length");
        }
    } else {
        // NOTE: We need someway to pass down the length from the caller because we cannot create and later on update
        //  a Type. We must also add no flag to indicate the array's ImplicitLength to it's Type as we wish the type of
        //  [..]u8{1} to be equal to [1]u8.
        //  The reason this wasn't implemented at the time of the comment was because in order to determine the length
        //  of the array you must first check all of a composite literals KeyValue pairs (as the key's can make the
        //  length != ArrayLen(LitComposite.elements)). This meant that checkExprLitComposite had to awkwardly check the
        //  array's element type & then check all of the KeyValue pairs to determine a length then pass this length down
        //  to a call to checkExprTypeArray. This alone is fine for expressions of the form
        //    [..]u8{1, 2}
        //  but in the form
        //    array: [..]u8 = {1, 2, 3, 4}
        //  this approach would not work and the checkDeclVariable (and checkDeclConstant) functions would also need to
        //  implement this sort of special logic.
        UNIMPLEMENTED();
    }

    type = NewTypeArray(flags, length, type);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return type;
}

Type *checkExprTypeSlice(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypeSlice);
    Type *type = checkExpr(expr->TypeSlice.type, ctx, pkg);
    expectType(pkg, type, ctx, expr->TypeSlice.type->start);
    if (ctx->mode != ExprMode_Type) {
        goto error;
    } if (type->Width == 0) {
        ReportError(pkg, ZeroWidthSliceElementError, expr->TypeSlice.type->start,
                    "Slices of zero width elements are not permitted");
    }

    type = NewTypeSlice(TypeFlag_None, type);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return type;
}

Type *checkExprTypeStruct(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_TypeStruct);

    DynamicArray(TypeField *) fields = NULL;
    ArrayFit(fields, ArrayLen(expr->TypeStruct.items));

    u32 align = 0;
    u32 width = 0;
    for (size_t i = 0; i < ArrayLen(expr->TypeStruct.items); i++) {
        AggregateItem item = expr->TypeStruct.items[i];

        Type *type = checkExpr(item.type, ctx, pkg);
        align = MAX(align, type->Align);
        for (size_t j = 0; j < ArrayLen(item.names); j++) {
            // TODO: Check for duplicate names!
            // TODO: Check the max alignment of the Target Arch and cap alignment requirements to that
            TypeField *field = AllocAst(pkg, sizeof(TypeField));
            ArrayPush(fields, field);

            field->name = item.names[j];
            field->type = type;
            field->offset = ALIGN_UP(width, type->Align);
            width = field->offset + type->Width;
        }
    }

    Type *type = NewTypeStruct(align, width, TypeFlag_None, fields);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Type;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprParen(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Paren);
    Type *type = checkExpr(expr->Paren.expr, ctx, pkg);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    return type;
}

Type *checkExprUnary(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Unary);
    Type *type = checkExpr(expr->Unary.expr, ctx, pkg);
    if (ctx->mode == ExprMode_Unresolved) return InvalidType;

    switch (expr->Unary.op) {
        case TK_And:
            if (ctx->mode < ExprMode_Addressable) {
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

    ctx->mode = ExprMode_Value;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprBinary(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Binary);
    CheckerContext lhsCtx = { ctx->scope };
    CheckerContext rhsCtx = { ctx->scope };
    Type *lhs = checkExpr(expr->Binary.lhs, &lhsCtx, pkg);
    Type *rhs = checkExpr(expr->Binary.rhs, &rhsCtx, pkg);
    if (lhsCtx.mode == ExprMode_Invalid || rhsCtx.mode == ExprMode_Invalid) goto error;

    // TODO: clean this up
    if (!(coerceTypeSilently(expr->Binary.lhs, &lhsCtx, &lhs, rhs, pkg) ||
          coerceTypeSilently(expr->Binary.rhs, &rhsCtx, &rhs, lhs, pkg))) {
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

    if ((expr->Binary.op == TK_Div || expr->Binary.op == TK_Rem) && IsConstant(&rhsCtx) && rhsCtx.val.i64 == 0) {
        ReportError(pkg, DivisionByZeroError, expr->Binary.rhs->start, "Division by zero");
    }

    ctx->flags |= lhsCtx.flags & rhsCtx.flags & CheckerContextFlag_Constant;
    b32 isConstantNegative;
    if (evalBinary(expr->Binary.op, type, lhsCtx.val, rhsCtx.val, ctx, &isConstantNegative)) {
        changeTypeOrRecordCoercionIfNeeded(&type, expr, ctx, isConstantNegative, pkg);
    }
    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Value;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprTernary(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Ternary);
    CheckerContext condCtx = { ctx->scope, .desiredType = BoolType };
    Type *cond = checkExpr(expr->Ternary.cond, &condCtx, pkg);

    Type *pass = cond;
    CheckerContext passCtx = condCtx;
    if (expr->Ternary.pass) {
        passCtx.desiredType = ctx->desiredType;
        pass = checkExpr(expr->Ternary.pass, &passCtx, pkg);
    }

    CheckerContext failCtx = { ctx->scope, .desiredType = ctx->desiredType };
    Type *fail = checkExpr(expr->Ternary.fail, &failCtx, pkg);
    if (failCtx.mode == ExprMode_Unresolved)

    if (condCtx.mode != ExprMode_Invalid && !isBoolean(cond) && !isNumericOrPointer(cond)) {
        ReportError(pkg, BadConditionError, expr->start,
                    "Expected a numeric or pointer type to act as a condition in the ternary expression");
        goto error;
    }

    Type *type = fail;
    // This check bypasses the type compatibility check?
    //  Should that be allowed just because we know the result at compile time?
    if (condCtx.flags & passCtx.flags & failCtx.flags & CheckerContextFlag_Constant) {
        ctx->flags |= CheckerContextFlag_Constant;
        ctx->val = condCtx.val.u64 ? passCtx.val : failCtx.val;
        type = condCtx.val.u64 ? pass : fail;
    } else {
        // NOTE: If we coerce the type before doing handling constant evaluation, we lose signedness information.
        if (!coerceType(expr->Ternary.fail, &failCtx, &fail, pass, pkg)) {
            ReportError(pkg, TypeMismatchError, expr->Ternary.fail->start,
                        "Expected type %s got type %s", DescribeType(pass ? pass : cond), DescribeType(fail));
            goto error;
        }
    }
    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Value;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprLocationDirective(Expr *expr, CheckerContext *ctx, Package *pkg) {
    Type *type = InvalidType;
    ctx->flags |= CheckerContextFlag_Constant;
    if (expr->LocationDirective.name == internLine) {
        type = U32Type;
        ctx->val.u32 = expr->start.line;
    } else if (expr->LocationDirective.name == internFile) {
        // TODO: @Strings @Builtins
    } else if (expr->LocationDirective.name == internLocation) {
        // TODO: @Structs @Builtins
    } else if (expr->LocationDirective.name == internFunction) {
        // TODO: @Strings
    }

    return type;
}

Type *checkExprCast(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Cast);
    CheckerContext targetCtx = { ctx->scope };
    Type *type = checkExpr(expr->Cast.type, &targetCtx, pkg);

    if (targetCtx.mode != ExprMode_Type) {
        ReportError(pkg, NotATypeError, expr->start,
                    "Cannot cast to non type %s", DescribeExpr(expr->Cast.type));
        goto error;
    }

    Type *callersDesiredType = ctx->desiredType;

    ctx->desiredType = type;
    Type *exprType = checkExpr(expr->Cast.expr, ctx, pkg);
    if (ctx->mode == ExprMode_Invalid) goto error;

    if (!cast(exprType, type, ctx)) {
        ReportError(pkg, InvalidConversionError, expr->start,
                    "Unable to cast type %s to type %s", DescribeType(exprType), DescribeType(type));
        goto error;
    }

    // Should we let the caller do this?...
    if (callersDesiredType) coerceType(expr, ctx, &type, callersDesiredType, pkg);

    storeInfoBasicExpr(pkg, expr, type, ctx);

    ctx->mode = ExprMode_Value;
    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprAutocast(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Autocast);
    if (!ctx->desiredType) {
        ReportError(pkg, AutocastExpectsDesiredTypeError, expr->start,
                    "Autocast expression requires a contextual type to convert to");
        goto error;
    }

    Type *type = checkExpr(expr->Autocast.expr, ctx, pkg);
    if (ctx->mode == ExprMode_Invalid) goto error;

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

Type *checkExprSubscript(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Subscript);
    CheckerContext targetCtx = { ctx->scope };
    CheckerContext indexCtx = { ctx->scope };
    Type *recv = checkExpr(expr->Subscript.expr, &targetCtx, pkg);
    if (targetCtx.mode == ExprMode_Invalid) goto error;

    Type *index = checkExpr(expr->Subscript.index, &indexCtx, pkg);
    if (indexCtx.mode == ExprMode_Invalid) goto error;

    if (!IsInteger(index)) {
        ReportError(pkg, InvalidSubscriptIndexTypeError, expr->Subscript.index->start,
                    "Cannot subscript with non-integer type '%s'",
                    DescribeType(index));
        goto error;
    }

    // TODO(Brett): constant folding of constant arrays and constant indices
    Type *type;
    switch (recv->kind) {
    case TypeKind_Array: {
        type = recv->Array.elementType;
        if (IsConstant(&indexCtx)) {
            i64 i = indexCtx.val.i64;
            if (i < 0 || (u64)i >= recv->Array.length) {
                const char *message = i < 0 ? "before the start" : "after the end of";
                ReportError(pkg, OutOfBoundsError, expr->Subscript.index->start,
                            "Index %d is %s of the array (%lu elements)",
                            i, message, recv->Array.length);
                goto error;
            }
        }
    } break;

    case TypeKind_Slice: {
        type = recv->Slice.elementType;
    } break;

    case TypeKind_Pointer: {
        type = recv->Pointer.pointeeType;
    } break;

    default:
        if (recv != InvalidType) {
            ReportError(pkg, UnsupportedSubscriptError, expr->Subscript.expr->start,
                        "Unable to subscript type '%s'",
                        DescribeType(recv));
        }

        goto error;
    }

    ctx->mode = ExprMode_Addressable;
    storeInfoBasicExpr(pkg, expr, type, ctx);

    return type;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprSelector(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Selector);
    
    Type *type;
    Type *base = checkExpr(expr->Selector.expr, ctx, pkg);
    if (ctx->mode == ExprMode_Invalid) goto error;
    if (ctx->mode == ExprMode_Unresolved) goto unresolved;

    if (base == FileType) goto TypeKind_File;

    switch (base->kind) {
        case TypeKind_Struct: {
            StructFieldLookupResult result = StructFieldLookup(base->Struct, expr->Selector.name);
            if (!result.field) {
                ReportError(pkg, TODOError, expr->Selector.start, "Struct %s has no member %s",
                            DescribeType(base), expr->Selector.name);
                goto error;
            }
            SelectorValue val = {.Struct.index = result.index, .Struct.offset = result.field->offset};
            type = result.field->type;
            storeInfoSelector(pkg, expr, type, SelectorKind_Struct, val, ctx);
            ctx->mode = ExprMode_Addressable;
            ctx->flags &= ~CheckerContextFlag_Constant;
            // TODO: Constant evaluation for struct types.
            break;
        }

        case TypeKind_Enum: {
            if (ctx->mode == ExprMode_Type) {
                EnumFieldLookupResult result = EnumFieldLookup(base->Enum, expr->Selector.name);
                if (!result.field) {
                    ReportError(pkg, TODOError, expr->Selector.start, "Enum %s has no member named %s",
                        DescribeType(base), expr->Selector.name);
                    goto error;
                }

                ctx->val.u64 = result.field->val;
                SelectorValue val = {.Enum.index = result.index};
                storeInfoSelector(pkg, expr, base, SelectorKind_Enum, val, ctx);
                type = base;
                ctx->mode = ExprMode_Addressable;
                ctx->flags |= CheckerContextFlag_Constant;
                break;
            }
        } 

        TypeKind_File: {
            Symbol *file = pkg->checkerInfo[expr->Selector.expr->id].Ident.symbol;
            Package *import = (Package *) file->backendUserdata;

            if (file->state == SymbolState_Unresolved) {
                ctx->mode = ExprMode_Unresolved;
                return NULL;
            }

            Symbol *symbol = Lookup(import->scope, expr->Selector.name);
            if (!symbol) {
                ReportError(pkg, TODOError, expr->Selector.start, "File %s has no member %s",
                            DescribeExpr(expr->Selector.expr), expr->Selector.name);
                goto error;
            }

            switch (symbol->state) {
                case SymbolState_Unresolved:
                    ctx->mode = ExprMode_Unresolved;
                    return NULL;
                case SymbolState_Resolving:
                    // TODO: For cyclic types we need a ctx->hasIndirection to check and see if
                    // We have a cyclic reference error.
                    ReportError(pkg, ReferenceToDeclarationError, expr->start,
                                "Declaration initial value refers to itself");
                    break;
                case SymbolState_Resolved:
                    break;
            }
            type = symbol->type;
            SelectorValue val = {.Import.symbol = symbol, .Import.package = import};
            storeInfoSelector(pkg, expr, type, SelectorKind_Import, val, ctx);
            ctx->mode = exprModeForSymbol(symbol);
            // Constants?
            break;
        }

        default: {
            ReportError(pkg, TODOError, expr->start, "%s has no member '%s'", DescribeExpr(expr->Selector.expr), expr->Selector.name);
            goto error;
        }
    }

    return type;

unresolved:
    ctx->mode = ExprMode_Unresolved;
    return NULL;
    
error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExprCall(Expr *expr, CheckerContext *ctx, Package *pkg) {
    ASSERT(expr->kind == ExprKind_Call);
    CheckerContext calleeCtx = { ctx->scope };
    Type *calleeType = checkExpr(expr->Call.expr, &calleeCtx, pkg);
    if (calleeCtx.mode == ExprMode_Invalid) goto error;
    if (calleeCtx.mode == ExprMode_Unresolved) goto unresolved;

    if (calleeCtx.mode == ExprMode_Type) {

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

    CheckerContext argCtx = { ctx->scope };
    ForEachWithIndex(expr->Call.args, i, Expr_KeyValue *, arg) {
        argCtx.desiredType = calleeType->Function.params[i];
        Type *type = checkExpr(arg->value, &argCtx, pkg);
        if (argCtx.mode == ExprMode_Unresolved) goto unresolved;
        if (argCtx.mode == ExprMode_Invalid) goto error;

        coerceType(arg->value, &argCtx, &type, calleeType->Function.params[i], pkg);
    }
    // TODO: Implement checking for calls
    Type *type = NewTypeTuple(TypeFlag_None, calleeType->Function.results);
    storeInfoBasicExpr(pkg, expr, type, ctx);
    ctx->mode = ExprMode_Value;
    return type;

unresolved:
    ctx->mode = ExprMode_Unresolved;
    return NULL;

error:
    ctx->mode = ExprMode_Invalid;
    return InvalidType;
}

Type *checkExpr(Expr *expr, CheckerContext *ctx, Package *pkg) {
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

        case ExprKind_LitString:
            type = checkExprLitString(expr, ctx, pkg);
            break;

        case ExprKind_LitFunction:
            type = checkExprLitFunction(expr, ctx, pkg);
            break;

        case ExprKind_LitCompound:
            type = checkExprLitCompound(expr, ctx, pkg);
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

        case ExprKind_TypeArray:
            type = checkExprTypeArray(expr, ctx, pkg);
            break;

        case ExprKind_TypeSlice:
            type = checkExprTypeSlice(expr, ctx, pkg);
            break;

        case ExprKind_TypeStruct:
            type = checkExprTypeStruct(expr, ctx, pkg);
            break;

        case ExprKind_TypeEnum:
            type = checkExprTypeEnum(expr, ctx, pkg);
            break;

        case ExprKind_TypeUnion:
        case ExprKind_TypePolymorphic:
            UNIMPLEMENTED();
            break;

        case ExprKind_Paren:
            type = checkExprParen(expr, ctx, pkg);
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

        case ExprKind_LocationDirective:
            type = checkExprLocationDirective(expr, ctx, pkg);
            break;

        case ExprKind_Call:
            type = checkExprCall(expr, ctx, pkg);
            break;

        case ExprKind_Cast:
            type = checkExprCast(expr, ctx, pkg);
            break;

        case ExprKind_Selector:
            type = checkExprSelector(expr, ctx, pkg);
            break;

        case ExprKind_Subscript:
            type = checkExprSubscript(expr, ctx, pkg);
            break;

        case ExprKind_Slice:
            UNIMPLEMENTED();
            break;

        default:
            break;
    }

    return type;
}

void checkDeclConstant(Decl *decl, CheckerContext *ctx, Package *pkg) {
    ASSERT(decl->kind == DeclKind_Constant);
    ASSERT(decl->owningPackage == pkg);
    Decl_Constant constant = decl->Constant;

    if (ArrayLen(constant.names) != 1) {
        ReportError(pkg, MultipleConstantDeclError, constant.start,
            "Constant declarations must declare at most one item");

        ForEach (constant.names, Expr_Ident *) {
            Symbol *symbol = Lookup(pkg->scope, it->name);
            markSymbolInvalid(symbol);
        }
        return;
    }

    if (ArrayLen(constant.values) > 1) {
        ReportError(pkg, ArityMismatchError, constant.start,
                    "Constant declarations only allow for a single value, but got %zu", ArrayLen(constant.values));
        return;
    }

    Type *expectedType = NULL;

    if (constant.type) {
        expectedType = checkExpr(constant.type, ctx, pkg);
        if (ctx->mode == ExprMode_Unresolved) return;

        expectType(pkg, expectedType, ctx, constant.type->start);
    }

    Expr_Ident *ident = constant.names[0];
    Expr *value = constant.values[0];

    Symbol *symbol;
    if (ctx->scope == pkg->scope) {
        symbol = MapGet(&ctx->scope->members, ident->name);
        ASSERT_MSG(symbol, "Symbols in the file scope should be declared in the Parser");
    } else {
        declareSymbol(pkg, ctx->scope, ident->name, &symbol, decl);
    }

    symbol->state = SymbolState_Resolving;

    switch (value->kind) {
        case ExprKind_LitFunction: {
            Expr_LitFunction func = value->LitFunction;
            Type *type = checkExprTypeFunction(func.type, ctx, pkg);
            if (ctx->mode == ExprMode_Unresolved) return;

            expectType(pkg, type, ctx, func.type->start);

            markSymbolResolved(symbol, type);
            symbol->kind = SymbolKind_Constant;
        } break;

        case ExprKind_TypeStruct: {
            symbol->state = SymbolState_Resolving;
            symbol->kind = SymbolKind_Type;
            break;
        }

        case ExprKind_TypeEnum: {
            symbol->state = SymbolState_Resolving;
            symbol->kind = SymbolKind_Type;
        } break;

        case ExprKind_TypeUnion: {
            UNIMPLEMENTED();
        } break;

        default: {
            symbol->kind = SymbolKind_Constant;
            break;
        }
    }

    CheckerContext exprCtx = { ctx->scope, .desiredType = expectedType };
    Type *type = checkExpr(value, &exprCtx, pkg);
    if (exprCtx.mode == ExprMode_Unresolved) goto unresolved;

    if (exprCtx.mode < ExprMode_Type || (((exprCtx.flags & CheckerContextFlag_Constant) == 0) && (exprCtx.mode != ExprMode_Type))) {
        // The above checks that the expression is constant
        // TODO: Simplify the above ... possibly by shifting around the ExprMode orders so that Addressable is lesser then Value.
        ReportError(pkg, NotAValueError, value->start,
                    "Expected a type or constant value but got %s (type %s)", DescribeExpr(value), DescribeType(type));
    }

    symbol->val = exprCtx.val;

    if (expectedType && !coerceType(value, &exprCtx, &type, expectedType, pkg)) {
        ReportError(pkg, InvalidConversionError, value->start,
                    "Unable to convert type %s to expected type type %s", DescribeType(type), DescribeType(expectedType));
        markSymbolInvalid(symbol);
        return;
    }

    markSymbolResolved(symbol, type);
    storeInfoConstant(pkg, decl, symbol);
    return;

unresolved:
    ctx->mode = ExprMode_Unresolved;
    return;
}

void checkDeclVariable(Decl *decl, CheckerContext *ctx, Package *pkg) {
    ASSERT(decl->kind == DeclKind_Variable);
    ASSERT(decl->owningPackage == pkg);
    Decl_Variable var = decl->Variable;

    Type *expectedType = NULL;

    if (var.type) {
        expectedType = checkExpr(var.type, ctx, pkg);
        if (ctx->mode == ExprMode_Unresolved) return;

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
            declareSymbol(pkg, ctx->scope, it->name, &symbol, decl);
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
            return;
        }

        // TODO(Brett): check for multi-value call

        if (var.values && var.values[0]->kind == ExprKind_Call) {
            UNIMPLEMENTED();
            return;
        }

        CheckerContext exprCtx = { ctx->scope, .desiredType = expectedType };
        For (var.names) {
            Type *type = checkExpr(var.values[i], &exprCtx, pkg);
            if (exprCtx.mode == ExprMode_Unresolved) goto unresolved;
            if (exprCtx.mode == ExprMode_Invalid) {
                markSymbolInvalid(symbols[i]);
                continue;
            };

            if (exprCtx.mode < ExprMode_Value) {
                ReportError(pkg, NotAValueError, var.values[i]->start,
                            "Expected a value but got %s (type %s)", DescribeExpr(var.values[i]), DescribeType(type));
            }

            if (expectedType && !coerceType(var.values[i], &exprCtx, &type, expectedType, pkg)) {
                ReportError(pkg, InvalidConversionError, var.values[i]->start,
                            "Unable to convert type %s to expected type type %s",
                            DescribeType(type), DescribeType(expectedType));
                markSymbolInvalid(symbols[i]);
                return;
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

    storeInfoVariable(pkg, decl, symbols);
    return;

unresolved:
    ctx->mode = ExprMode_Unresolved;
    return;
}

void checkDeclForeign(Decl *decl, CheckerContext *ctx, Package *pkg) {
    ASSERT(decl->kind == DeclKind_Foreign);

    Type *type = checkExpr(decl->Foreign.type, ctx, pkg);
    if (ctx->mode == ExprMode_Unresolved) return;

    expectType(pkg, type, ctx, decl->Foreign.type->start);

    Symbol *symbol;
    if (ctx->scope == pkg->scope) {
        symbol = MapGet(&ctx->scope->members, decl->Foreign.name);
        ASSERT_MSG(symbol, "Symbols in the file scope should be declared in the Parser");
    } else {
        declareSymbol(pkg, ctx->scope, decl->Foreign.name, &symbol, decl);
    }

    symbol->externalName = decl->Foreign.linkname;

    markSymbolResolved(symbol, type);
    storeInfoForeign(pkg, decl, symbol);
}

void checkDeclForeignBlock(Decl *decl, CheckerContext *ctx, Package *pkg) {
    ASSERT(decl->kind == DeclKind_ForeignBlock);

    for (size_t i = 0; i < ArrayLen(decl->ForeignBlock.members); i++) {
        Decl_ForeignBlockMember it = decl->ForeignBlock.members[i];

        Type *type = checkExpr(it.type, ctx, pkg);
        if (ctx->mode == ExprMode_Unresolved) return;

        expectType(pkg, type, ctx, it.type->start);

        Symbol *symbol = it.symbol;
        symbol->externalName = it.linkname;
        markSymbolResolved(symbol, type);
    }
}

void checkDeclImport(Decl *decl, CheckerContext *ctx, Package *pkg) {
    ASSERT(decl->kind == DeclKind_Import);

    ctx->desiredType = StringType;
    Type *type = checkExpr(decl->Import.path, ctx, pkg);
    if (ctx->mode == ExprMode_Invalid) goto error;
    if (ctx->mode == ExprMode_Unresolved) return;

    if (!TypesIdentical(type, StringType) || !IsConstant(ctx)) {
        ReportError(pkg, TODOError, decl->start,
                    "Could not resolve path %s to string constant", DescribeExpr(decl->Import.path));
        goto error;
    }

    // TODO: Path stuff

    Symbol *symbol = decl->Import.symbol;

    const char *path = (const char *) ctx->val.ptr;
    Package *import = ImportPackage(path, pkg);
    symbol->backendUserdata = import;

    return;

error:
    return;
}

void checkStmtLabel(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    ASSERT(stmt->kind == StmtKind_Label);
    Symbol *sym = declareLabelSymbol(pkg, ctx->scope, stmt->Label.name);
    storeInfoLabel(pkg, stmt, sym);
}

void checkStmtAssign(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    ASSERT(stmt->kind == StmtKind_Assign);
    Stmt_Assign assign = stmt->Assign;

    DynamicArray(Type *) lhsTypes = NULL;

    ForEach(assign.lhs, Expr *) {
        Type *type = checkExpr(it, ctx, pkg);
        ArrayPush(lhsTypes, type);
        if (ctx->mode < ExprMode_Addressable && ctx->mode != ExprMode_Invalid) {
            ReportError(pkg, ValueNotAssignableError, it->start,
                        "Cannot assign to value %s of type %s", DescribeExpr(it), DescribeType(type));
        }
    }

    if (assign.rhs[0]->kind == ExprKind_Call) {
        // TODO: Handle void calls (empty tuple) (error)
        // TODO: Handle single returns
        // TODO: Handle multiple returns
        UNIMPLEMENTED();
        return;
    }

    Type *prevDesiredType = ctx->desiredType;
    ForEachWithIndex(assign.rhs, i, Expr *, expr) {
        if (i >= ArrayLen(lhsTypes)) {
            break;
        }

        ctx->desiredType = lhsTypes[i];
        Type *type = checkExpr(expr, ctx, pkg);
        if (ctx->mode < ExprMode_Value) {
            ReportError(pkg, NotAValueError, expr->start,
                        "Expected a value but got %s (type %s)", DescribeExpr(expr), DescribeType(type));
        }
        if (!coerceType(expr, ctx, &type, lhsTypes[i], pkg)) {
            ReportError(pkg, TypeMismatchError, expr->start,
                        "Cannot assign %s to value of type %s", DescribeType(type), DescribeType(lhsTypes[i]));
        }
    }

    ctx->desiredType = prevDesiredType;

    if (ArrayLen(assign.rhs) != ArrayLen(assign.lhs)) {
        ReportError(pkg, AssignmentCountMismatchError, stmt->start,
                    "Left side has %zu values while right side %zu values", ArrayLen(assign.lhs), ArrayLen(assign.rhs));
    }
}

void checkStmtReturn(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    ASSERT(stmt->kind == StmtKind_Return);
    ASSERT(ctx->desiredType && ctx->desiredType->kind == TypeKind_Tuple);

    size_t nTypes = ArrayLen(ctx->desiredType->Tuple.types);
    size_t nExprs = ArrayLen(stmt->Return.exprs);
    // FIXME: What about returns that are tuples? We need a nice splat helper

    if (nExprs != nTypes) {
        ReportError(pkg, WrongNumberOfReturnsError, stmt->start,
                    "Wrong number of return expressions, expected %zu, got %zu",
                    ArrayLen(ctx->desiredType->Tuple.types), ArrayLen(stmt->Return.exprs));
    }
    for (size_t i = 0; i < MIN(nTypes, nExprs); i++) {
        Expr *expr = stmt->Return.exprs[i];
        Type *expectedType = ctx->desiredType->Tuple.types[i];
        CheckerContext exprCtx = { ctx->scope, .desiredType = expectedType };
        Type *type = checkExpr(expr, &exprCtx, pkg);
        if (exprCtx.mode == ExprMode_Invalid) continue;
        coerceType(expr, &exprCtx, &type, expectedType, pkg);
    }
}

void checkStmtDefer(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    ASSERT(stmt->kind == StmtKind_Defer);

    // NOTE: We set desiredType so that we can return within a `defer`
    // TODO: Determine if we actually want to enable this sort of behaviour
    CheckerContext deferCtx = { pushScope(pkg, ctx->scope), .desiredType = ctx->desiredType };
    checkStmt(stmt->Defer.stmt, &deferCtx, pkg);
    ctx->mode = deferCtx.mode;
}

void checkStmtGoto(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    ASSERT(stmt->kind == StmtKind_Goto);

    if (stmt->Goto.keyword == Keyword_break && !(ctx->loop || ctx->swtch)) {
        ReportError(pkg, BreakNotPermittedError, stmt->start,
                    "Break is not permitted outside of a switch or loop body");
        goto error;
    } else if (stmt->Goto.keyword == Keyword_continue && !ctx->loop) {
        ReportError(pkg, ContinueNotPermittedError, stmt->start,
                    "Continue is not permitted outside of a loop body");
        goto error;
    } else if (stmt->Goto.keyword == Keyword_fallthrough) {
        if (stmt->Goto.target) {
            ReportError(pkg, FallthroughWithTargetError, stmt->start,
                        "Fallthrough statements cannot provide a target to fall through too");
            goto error;
        } else if (!ctx->swtch) {
            ReportError(pkg, FallthroughNotPermittedError, stmt->start,
                        "Fallthrough is not permitted outside of a switch body");
            goto error;
        } else if (!ctx->nextCase) {
            ReportError(pkg, FallthroughWithoutNextCaseError, stmt->start,
                        "Cannot fallthrough from here. There is no next case");
            goto error;
        }
    }

    if (stmt->Goto.target) {
        Type *type = checkExpr(stmt->Goto.target, ctx, pkg);
        coerceType(stmt->Goto.target, ctx, &type, RawptrType, pkg);
        // NULL indicates to the backend that there is an target and to emit it and branch there
        storeInfoGoto(pkg, stmt, NULL);
    } else if (stmt->Goto.keyword == Keyword_continue) {
        Symbol *target = GetStmtInfo(pkg, ctx->loop)->For.continueTarget;
        storeInfoGoto(pkg, stmt, target);
    } else if (stmt->Goto.keyword == Keyword_fallthrough) {
        Symbol *target = GetStmtInfo(pkg, ctx->nextCase)->Case.fallthroughTarget;
        storeInfoGoto(pkg, stmt, target);
    } else if (stmt->Goto.keyword == Keyword_break) {
        Symbol *target;
        if (ctx->flags & CheckerContextFlag_LoopClosest) {
            target = GetStmtInfo(pkg, ctx->loop)->For.breakTarget;
        } else {
            target = GetStmtInfo(pkg, ctx->swtch)->Switch.breakTarget;
        }
        storeInfoGoto(pkg, stmt, target);
    } else {
        PANIC("Either no target expression for `goto` or unrecognized keyword");
    }

error:
    return;
}

void checkStmtBlock(Stmt *stmt, CheckerContext *ctx, Package *pkg) {

    CheckerContext blockCtx = {
        .scope = pushScope(pkg, ctx->scope),
        .desiredType = ctx->desiredType,
        .swtch = ctx->swtch,
        .nextCase = ctx->nextCase,
        .loop = ctx->loop,
        .flags = ctx->flags & ~CheckerContextFlag_Constant,
    };

    ForEach(stmt->Block.stmts, Stmt *) {
        checkStmt(it, &blockCtx, pkg);
    }
}

void checkStmtIf(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    CheckerContext ifCtx = {
        .scope = pushScope(pkg, ctx->scope),
        .desiredType = BoolType,
        .swtch = ctx->swtch,
        .nextCase = ctx->nextCase,
        .loop = ctx->loop,
        .flags = ctx->flags & ~CheckerContextFlag_Constant,
    };

    Type *type = checkExpr(stmt->If.cond, &ifCtx, pkg);
    if (!coerceType(stmt->If.cond, &ifCtx, &type, BoolType, pkg)) {
        ReportError(pkg, TypeMismatchError, stmt->If.cond->start,
                    "Expected type bool got type %s", DescribeType(type));
    }
    ifCtx.desiredType = ctx->desiredType;
    checkStmt(stmt->If.pass, &ifCtx, pkg);
    if (stmt->If.fail) checkStmt(stmt->If.fail, &ifCtx, pkg);
}

void checkStmtFor(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    CheckerContext forCtx = {
        .scope = pushScope(pkg, ctx->scope),
        .swtch = ctx->swtch,
        .nextCase = ctx->nextCase,
        .loop = ctx->loop,
        .flags = ctx->flags & ~CheckerContextFlag_Constant,
    };

    // NOTE: The following 2 strings aren't interned and that's fine, they are just placeholder anyway
    GetStmtInfo(pkg, stmt)->For.breakTarget = declareLabelSymbol(pkg, ctx->scope, "$break");
    GetStmtInfo(pkg, stmt)->For.continueTarget = declareLabelSymbol(pkg, ctx->scope, "$continue");

    if (stmt->For.init) checkStmt(stmt->For.init, &forCtx, pkg);
    if (stmt->For.step) checkStmt(stmt->For.step, &forCtx, pkg);
    forCtx.desiredType = BoolType;
    if (stmt->For.cond) {
        Type *type = checkExpr(stmt->For.cond, &forCtx, pkg);
        coerceType(stmt->For.cond, &forCtx, &type, BoolType, pkg);
    }

    forCtx.loop = stmt;
    forCtx.flags |= CheckerContextFlag_LoopClosest;

    forCtx.desiredType = NULL;
    ForEach(stmt->For.body->stmts, Stmt *) {
        checkStmt(it, &forCtx, pkg);
    }
}

void checkStmtForIn(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    CheckerContext forCtx = {
        .scope = pushScope(pkg, ctx->scope),
        .swtch = ctx->swtch,
        .nextCase = ctx->nextCase,
        .loop = ctx->loop,
        .flags = ctx->flags & ~CheckerContextFlag_Constant,
    };

    // NOTE: The following 2 strings aren't interned and that's fine, they are just placeholder anyway
    GetStmtInfo(pkg, stmt)->For.breakTarget = declareLabelSymbol(pkg, ctx->scope, "$break");
    GetStmtInfo(pkg, stmt)->For.continueTarget = declareLabelSymbol(pkg, ctx->scope, "$continue");

    Type *type = checkExpr(stmt->ForIn.aggregate, ctx, pkg);
    if (isArray(type)) {
        type = type->Array.elementType;
    } else if (isSlice(type)) {
        type = type->Slice.elementType;
    } else {
        ReportError(pkg, CannotIterateError, stmt->ForIn.aggregate->start,
                    "Cannot iterate over %s (type %s) ", DescribeExpr(stmt->ForIn.aggregate), DescribeType(type));
        return;
    }

    if (stmt->ForIn.valueName) declareResolvedSymbol(pkg, forCtx.scope, type, stmt->ForIn.valueName->name, NULL);
    if (stmt->ForIn.valueName) declareResolvedSymbol(pkg, forCtx.scope, type, stmt->ForIn.indexName->name, NULL);

    forCtx.loop = stmt;
    forCtx.flags |= CheckerContextFlag_LoopClosest;

    forCtx.desiredType = NULL;
    ForEach(stmt->For.body->stmts, Stmt *) {
        checkStmt(it, &forCtx, pkg);
    }
}

void checkStmtSwitch(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    CheckerContext switchCtx = {
        .scope = pushScope(pkg, ctx->scope),
        .swtch = stmt,
        .nextCase = NULL,
        .loop = ctx->loop,
        .flags = ctx->flags & ~CheckerContextFlag_Constant &  ~CheckerContextFlag_LoopClosest,
    };

    Type *switchType = BoolType;
    if (stmt->Switch.match) {
        switchType = checkExpr(stmt->Switch.match, &switchCtx, pkg);
        if (!isNumericOrPointer(switchType) && !isBoolean(switchType)) {
            ReportError(pkg, CannotSwitchError, stmt->Switch.match->start,
                        "Cannot switch on value of type %s", DescribeType(switchType));
        }
    }

    Symbol *breakTarget = declareLabelSymbol(pkg, switchCtx.scope, "$break");
    storeInfoSwitch(pkg, stmt, breakTarget);

    ForEachWithIndex(stmt->Switch.cases, i, Stmt *, switchCase) {
        ForEach(switchCase->SwitchCase.matches, Expr *) {
            Type *type = checkExpr(it, &switchCtx, pkg);
            if (!coerceType(it, &switchCtx, &type, switchType, pkg)) {
                ReportError(pkg, TypeMismatchError, it->start, "Cannot convert %s to type %s",
                            DescribeType(type), DescribeType(switchType));
            }
        }
        if (!switchCase->SwitchCase.matches && i + 1 != ArrayLen(stmt->Switch.cases)) {
            ReportError(pkg, DefaultSwitchCaseNotLastError, switchCase->start,
                        "The default switch case must be the final case");
        }

        CheckerContext caseCtx = {
            .scope = pushScope(pkg, switchCtx.scope),
            .swtch = stmt,
            .desiredType = ctx->desiredType,
            .nextCase = NULL,
            .loop = switchCtx.loop,
            .flags = switchCtx.flags & ~CheckerContextFlag_Constant,
        };

        // Create a target for fallthough
        if (i + 1 < ArrayLen(stmt->Switch.cases)) {
            CheckerInfo_Case *nextCase = &GetStmtInfo(pkg, stmt->Switch.cases[i + 1])->Case;
            nextCase->fallthroughTarget = declareLabelSymbol(pkg, caseCtx.scope, "$fallthrough");
            caseCtx.nextCase = stmt->Switch.cases[i + 1];
        }

        ForEach(switchCase->SwitchCase.block->stmts, Stmt *) {
            checkStmt(it, &caseCtx, pkg);
        }
    }
}

void checkStmt(Stmt *stmt, CheckerContext *ctx, Package *pkg) {
    switch (stmt->kind) {
        case StmtDeclKind_Constant:
            checkDeclConstant((Decl *) stmt, ctx, pkg);
            break;

        case StmtDeclKind_Variable:
            checkDeclVariable((Decl *) stmt, ctx, pkg);
            break;

        case StmtDeclKind_Foreign:
            checkDeclForeign((Decl *) stmt, ctx, pkg);
            break;

        case StmtDeclKind_ForeignBlock:
            checkDeclForeignBlock((Decl *) stmt, ctx, pkg);
            break;

        case StmtDeclKind_Import:
            checkDeclImport((Decl *) stmt, ctx, pkg);
            break;

        case StmtKind_Label:
            checkStmtLabel(stmt, ctx, pkg);
            break;

        case StmtKind_Assign:
            checkStmtAssign(stmt, ctx, pkg);
            break;

        case StmtKind_Return:
            checkStmtReturn(stmt, ctx, pkg);
            break;

        case StmtKind_Defer:
            checkStmtDefer(stmt, ctx, pkg);
            break;

        case StmtKind_Goto:
            checkStmtGoto(stmt, ctx, pkg);
            break;

        case StmtKind_Block:
            checkStmtBlock(stmt, ctx, pkg);
            break;

        case StmtKind_If:
            checkStmtIf(stmt, ctx, pkg);
            break;

        case StmtKind_For:
            checkStmtFor(stmt, ctx, pkg);
            break;

        case StmtKind_ForIn:
            checkStmtForIn(stmt, ctx, pkg);
            break;

        case StmtKind_Switch:
            checkStmtSwitch(stmt, ctx, pkg);
            break;

        default:
            if (isExpr(stmt)) {
                checkExpr((Expr *) stmt, ctx, pkg);
                break;
            }
            ASSERT_MSG_VA(false, "Statement of type '%s' went unchecked", AstDescriptions[stmt->kind]);
    }
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
    REINIT_COMPILER();
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
    ASSERT(sym->decl->start.offset == 0);
    ASSERT(sym->val.u64 == 8);
}

#define RESET_CONTEXT(_CTX) \
memset(((u8*) &_CTX) + sizeof(_CTX.scope), 0, sizeof(_CTX) - sizeof(_CTX.scope)); \
memcpy((u8*) &ctx.scope, &pkg.scope, sizeof(pkg.scope));


void test_coercionsAreMarked() {
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    ASSERT(ArrayLen(type->Function.params) == 0);
    ASSERT(ArrayLen(type->Function.results) == 1);

    checkTypeFunction("fn (u8, u8, u8, u8) -> (u8, u8, u8, u8)");
    ASSERT(type->kind == TypeKind_Function);
    ASSERT(info.type == type);
    ASSERT(ArrayLen(type->Function.params) == 4);
    ASSERT(ArrayLen(type->Function.results) == 4);
}

void test_checkTypePointer() {
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    ASSERT(!type->Struct.members);
    ASSERT(!pkg.diagnostics.errors);

    checkTypeStruct("struct {a: u8}");
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Struct.members[0]->type == U8Type);
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 8);
    ASSERT(type->Width == 8);
    ASSERT(info.type == type);

    checkTypeStruct("struct {a: u8; b: u16}");
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Struct.members[0]->type == U8Type);
    ASSERT(type->Struct.members[1]->type == U16Type);
    ASSERT_MSG(type->Struct.members[1]->offset == 16, "Fields should be aligned to at least their size");
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 16);
    ASSERT(type->Width == 32);
    ASSERT(info.type == type);

    checkTypeStruct("struct {a: u8; b: u8; b: u16; c: u32; d: u32}");
    ASSERT(type->kind == TypeKind_Struct);
    ASSERT(type->Struct.members[0]->type == U8Type);
    ASSERT(type->Struct.members[1]->type == U8Type);
    ASSERT(type->Struct.members[2]->type == U16Type);
    ASSERT(type->Struct.members[3]->type == U32Type);
    ASSERT(type->Struct.members[4]->type == U32Type);

    ASSERT_MSG(type->Struct.members[0]->offset == 0, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[1]->offset == 8, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[2]->offset == 16, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[3]->offset == 32, "Fields should be aligned to at least their size");
    ASSERT_MSG(type->Struct.members[4]->offset == 64, "Fields should be aligned to at least their size");
    ASSERT(type->Struct.Flags == TypeFlag_None);
    ASSERT(type->Align == 32);
    ASSERT(type->Width == 96);
    ASSERT(info.type == type);
}

void test_checkConstantUnaryExpressions() {
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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

void test_checkExprSelector() {
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
//    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
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
    REINIT_COMPILER();
    Stmt *stmt;
    CheckerContext ctx = { pkg.scope };
    DynamicArray(Type *) types = NULL;

#define checkReturn(_CODE) \
stmt = resetAndParseReturningLastStmt(_CODE); \
checkStmtReturn(stmt, &ctx, &pkg); \
RESET_CONTEXT(ctx); \
ArrayClear(types)

    ArrayPush(types, I64Type);
    ctx.desiredType = NewTypeTuple(TypeFlag_None, types);
    checkReturn("return 42");
    ASSERT(!pkg.diagnostics.errors);

    ArrayPush(types, I64Type);
    ArrayPush(types, I64Type);
    ArrayPush(types, F64Type);
    ctx.desiredType = NewTypeTuple(TypeFlag_None, types);
    checkReturn("return 1, 2, 6.28");
    ASSERT(!pkg.diagnostics.errors);
}

void test_checkStmtSwitch() {
    REINIT_COMPILER();
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




