
i64 evalUnarySigned(TokenKind op, Val val);
u64 evalUnaryUnsigned(TokenKind op, Val val);
f64 evalUnaryFloat(TokenKind op, Val val);

b32 evalUnary(TokenKind op, Type *type, Val *val) {
    ASSERT(val);
    b32 isConstant;
    if (isInteger(type)) {
        if (type->Flags & TypeFlag_Signed) {
            val->i64 = evalUnarySigned(op, *val);
        } else {
            val->u64 = evalUnaryUnsigned(op, *val);
        }
        isConstant = true;
    } else if (isFloat(type)) {
        val->f64 = evalUnaryFloat(op, *val);
        isConstant = true;
    } else {
        val->u64 = 0;
        isConstant = false;
    }
    return isConstant;
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
