
i64 evalUnarySigned(TokenKind op, Val val);
u64 evalUnaryUnsigned(TokenKind op, Val val);
f64 evalUnaryFloat(TokenKind op, Val val);

b32 evalUnary(TokenKind op, Type *type, CheckerContext *ctx, b32 *isNegative) {
    if (!ctx->isConstant) return false;

    if (IsInteger(type)) {
        if (IsSigned(type)) {
            ctx->val.i64 = evalUnarySigned(op, ctx->val);
            *isNegative = ctx->val.i64 < 0;
        } else {
            ctx->val.u64 = evalUnaryUnsigned(op, ctx->val);
            *isNegative = op == TK_Sub;
        }
    } else if (IsFloat(type)) {
        ctx->val.f64 = evalUnaryFloat(op, ctx->val);
    } else if (isBoolean(type)) {
        ctx->val.u64 = evalUnaryUnsigned(op, ctx->val);
    } else {
        ctx->val.u64 = 0;
        ctx->isConstant = false;
    }

    if (op == TK_BNot) ctx->val.u64 &= BITMASK(u64, type->Width);

    return ctx->isConstant;
}

i64 evalBinarySigned(TokenKind op, i64 left, i64 right);
u64 evalBinaryUnsigned(TokenKind op, u64 left, u64 right);
f64 evalBinaryFloat(TokenKind op, f64 left, f64 right);

b32 evalBinary(TokenKind op, Type *type, Val lhsValue, Val rhsValue, CheckerContext *ctx, b32 *isNegative) {
    if (!ctx->isConstant) return false;

    if (IsInteger(type)) {
        if (IsSigned(type)) {
            ctx->val.i64 = evalBinarySigned(op, lhsValue.i64, rhsValue.i64);
            *isNegative = ctx->val.i64 < 0;
        } else {
            ctx->val.u64 = evalBinaryUnsigned(op, lhsValue.u64, rhsValue.u64);
            *isNegative = false;
        }
    } else if (IsFloat(type)) {
        ctx->val.f64 = evalBinaryFloat(op, lhsValue.f64, rhsValue.f64);
    } else if (isBoolean(type)) {
        ctx->val.u64 = evalBinaryUnsigned(op, lhsValue.u64, rhsValue.u64);
    } else {
        ctx->val.f64 = 0.f;
        ctx->isConstant = false;
    }

    return ctx->isConstant;
}

i64 evalUnarySigned(TokenKind op, Val val) {
    switch (op) {
        case TK_Add:
            return +val.i64;
        case TK_Sub:
            return -val.i64;
        case TK_BNot:
            return ~val.i64;
        case TK_Not:
            return !val.i64;
        case TK_Lss:
        default:
            ASSERT(false);
    }
    return 0;
}

u64 evalUnaryUnsigned(TokenKind op, Val val) {
    switch (op) {
        case TK_Add:
            return +val.u64;
        case TK_Sub:
            return 0ull - val.u64;
        case TK_BNot:
            return ~val.u64;
        case TK_Not:
            return !val.u64;
        case TK_Lss:
        default:
            ASSERT(false);
    }
    return 0;
}

f64 evalUnaryFloat(TokenKind op, Val val) {
    switch (op) {
        case TK_Add:
            return +val.f64;
        case TK_Sub:
            return -val.f64;
        default:
            ASSERT(false);
    }
    return 0;
}

i64 evalBinarySigned(TokenKind op, i64 left, i64 right) {
    switch (op) {
        case TK_Mul:
            return left * right;
        case TK_Div:
            return right != 0 ? left / right : 0;
        case TK_Rem:
            return right != 0 ? left % right : 0;
        case TK_And:
            return left & right;
        case TK_Shl:
            return left << right;
        case TK_Shr:
            return left >> right;
        case TK_Add:
            return left + right;
        case TK_Sub:
            return left - right;
        case TK_Or:
            return left | right;
        case TK_Xor:
            return left ^ right;
        case TK_Eql:
            return left == right;
        case TK_Neq:
            return left != right;
        case TK_Lss:
            return left < right;
        case TK_Leq:
            return left <= right;
        case TK_Gtr:
            return left > right;
        case TK_Geq:
            return left >= right;
        default:
            PANIC("Invalid binary operation in constant evaluation");
            break;
    }
    return 0;
}

u64 evalBinaryUnsigned(TokenKind op, u64 left, u64 right) {
    switch (op) {
        case TK_Mul:
            return left * right;
        case TK_Div:
            return right != 0 ? left / right : 0;
        case TK_Rem:
            return right != 0 ? left % right : 0;
        case TK_And:
            return left & right;
        case TK_Shl:
            return left << right;
        case TK_Shr:
            return left >> right;
        case TK_Add:
            return left + right;
        case TK_Sub:
            return left - right;
        case TK_Or:
            return left | right;
        case TK_Xor:
            return left ^ right;
        case TK_Eql:
            return left == right;
        case TK_Neq:
            return left != right;
        case TK_Lss:
            return left < right;
        case TK_Leq:
            return left <= right;
        case TK_Gtr:
            return left > right;
        case TK_Geq:
            return left >= right;
        case TK_Land:
            return left && right;
        case TK_Lor:
            return left || right;
        default:
            PANIC("Invalid binary operation in constant evaluation");
            break;
    }
    return 0;
}

f64 evalBinaryFloat(TokenKind op, f64 left, f64 right) {
    switch (op) {
        case TK_Mul:
            return left * right;
        case TK_Div:
            return right != 0 ? left / right : 0;
        case TK_Rem:
            return right != 0 ? fmod(left, right) : 0;
        case TK_Add:
            return left + right;
        case TK_Sub:
            return left - right;
        case TK_Eql:
            return left == right;
        case TK_Neq:
            return left != right;
        case TK_Lss:
            return left < right;
        case TK_Leq:
            return left <= right;
        case TK_Gtr:
            return left > right;
        case TK_Geq:
            return left >= right;
        default:
            PANIC("Invalid binary operation in constant evaluation");
            break;
    }
    return 0;
}
