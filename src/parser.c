
i32 PrecedenceForTokenKind[NUM_TOKEN_KINDS] = {
    0,
    [TK_Question] = 1,
    [TK_Lor] = 1,
    [TK_Land] = 2,
    [TK_Eql] = 3,
    [TK_Neq] = 3,
    [TK_Lss] = 3,
    [TK_Leq] = 3, 
    [TK_Gtr] = 3, 
    [TK_Geq] = 3,
    [TK_Add] = 4,
    [TK_Sub] = 4, 
    [TK_Or]  = 4, 
    [TK_Xor] = 4,
    [TK_Mul] = 5,
    [TK_Div] = 5,
    [TK_Rem] = 5,
    [TK_Shl] = 5, 
    [TK_Shr] = 5, 
    [TK_And] = 5,
    [TK_Assign] = 0,
};

typedef struct Parser Parser;
struct Parser {
    Lexer lexer;
    Position prevStart;
    Position prevEnd;
    Token tok;
    Package *package;
};

#define nextToken() \
    p->prevStart = p->tok.pos; \
    p->prevEnd = p->lexer.pos; \
    p->tok = NextToken(&p->lexer)

#define CurrentToken p->tok

b32 isToken(Parser *p, TokenKind kind) {
    return p->tok.kind == kind;
}

b32 isTokenEof(Parser *p) {
    return p->tok.kind == TK_Eof;
}

b32 isTokenIdent(Parser *p, const char *ident) {
    return p->tok.kind == TK_Ident && p->tok.val.s == ident;
}

b32 isDirective(Parser *p, const char *ident) {
    return isToken(p, TK_Directive);
}

b32 isKeyword(Parser *p, const char *ident) {
    return isToken(p, TK_Keyword) && p->tok.val.s == ident;
}

b32 isKeywordBranch(Parser *p, const char *ident) {
    return p->tok.kind == TK_Keyword &&
    (p->tok.val.s == Keyword_goto ||
     p->tok.val.s == Keyword_break ||
     p->tok.val.s == Keyword_continue ||
     p->tok.val.s == Keyword_fallthrough);
}

b32 isNotRbraceOrEOF(Parser *p) {
    if (p->tok.kind == TK_Rbrace) return false;
    else if (p->tok.kind == TK_Eof) return false;
    return true;
}

b32 matchKeyword(Parser *p, const char *ident) {
    if (isKeyword(p, ident)) {
        nextToken();
        return true;
    }
    return false;
}

b32 matchDirective(Parser *p, const char *ident) {
    if (isDirective(p, ident)) {
        nextToken();
        return true;
    }
    return false;
}

b32 matchToken(Parser *p, TokenKind kind) {
    if (isToken(p, kind)) {
        nextToken();
        return true;
    }
    return false;
}

b32 expectToken(Parser *p, TokenKind kind) {
    if (matchToken(p, kind)) return true;
    ReportError(p->package, SyntaxError, p->tok.pos, "Expected token %s, got %s", DescribeTokenKind(kind), DescribeToken(p->tok));
    return false;
}

b32 expectTerminator(Parser *p) {
    if (matchToken(p, TK_Terminator) || isToken(p, TK_Eof)) return true;
    ReportError(p->package, SyntaxError, p->tok.pos, "Expected terminator, got %s", DescribeToken(p->tok));
    return false;
}

const char *parseIdent(Parser *p) {
    const char *ident = p->tok.val.ident;
    expectToken(p, TK_Ident);
    return ident;
}

DynamicArray(const char *) parseIdentList(Parser *p) {
    DynamicArray(const char *) names = NULL;

    do  {
        ArrayPush(names, parseIdent(p));
    } while (matchToken(p, TK_Comma));

    return names;
}

Expr *parseType(Parser *p);
Expr *parseExpr(Parser *p, b32 noCompoundLiteral);
Expr *parseFunctionType(Parser *p);
Expr_KeyValue *parseExprCompoundField(Parser *p);
DynamicArray(Expr *) parseExprList(Parser *p, b32 noCompoundLiteral);
Stmt_Block *parseBlock(Parser *p);
Stmt *parseStmt(Parser *p);

Expr *parseExprAtom(Parser *p) {
    Package *pkg = p->package;
    switch (p->tok.kind) {
        case TK_Ident: {
            Expr *e = NewExprIdent(pkg, p->tok.pos, p->tok.val.ident);
            nextToken();
            if (p->tok.kind == TK_Dot) {  // package.Member
                nextToken();
                const char *ident = parseIdent(p);
                return NewExprSelector(pkg, e, ident, p->prevEnd);
            }
            return e;
        }

        case TK_Int: {
            Expr *e = NewExprLitInt(pkg, p->tok.pos, p->tok.val.i);
            nextToken();
            return e;
        }

        case TK_Float: {
            Expr *e = NewExprLitFloat(pkg, p->tok.pos, p->tok.val.f);
            nextToken();
            return e;
        }

        case TK_String: {
            Expr *e = NewExprLitString(pkg, p->tok.pos, p->tok.val.s);
            nextToken();
            return e;
        }

        case TK_Lparen: {
            Position start = p->tok.pos;
            nextToken();
            Expr *expr = parseExpr(p, false);
            expectToken(p, TK_Rparen);
            return NewExprParen(pkg, expr, start, p->prevStart);
        }

        case TK_Lbrack: {
            Position start = p->tok.pos;
            nextToken();
            if (matchToken(p, TK_Rbrack)) {
                Expr *type = parseType(p);
                return NewExprTypeSlice(pkg, start, type);
            }
            Expr *length = NULL;
            if (!matchToken(p, TK_Ellipsis)) {
                length = parseExpr(p, false);
            }
            expectToken(p, TK_Rbrack);
            Expr *type = parseType(p);
            return NewExprTypeArray(pkg, start, length, type);
        }

        case TK_Lbrace: {
            Position start = p->tok.pos;
            nextToken();
            DynamicArray(Expr_KeyValue *) elements = NULL;
            if (!isToken(p, TK_Rbrace)) {
                ArrayPush(elements, parseExprCompoundField(p));
                while (matchToken(p, TK_Comma)) {
                    if (isToken(p, TK_Rbrace)) break;
                    ArrayPush(elements, parseExprCompoundField(p));
                }
            }
            expectToken(p, TK_Rbrace);
            return NewExprLitCompound(pkg, start, NULL, elements, p->prevEnd);
        }


        case TK_Dollar: {
            Position start = p->tok.pos;
            nextToken();
            const char *name = parseIdent(p);
            return NewExprTypePolymorphic(pkg, start, name);
        }

        caseEllipsis:
        case TK_Ellipsis: {
            u8 flags = 0;
            flags |= matchDirective(p, internCVargs) ? TypeVariadicFlagCVargs : 0;
            Position start = p->tok.pos;
            expectToken(p, TK_Ellipsis); // NOTE: We must expect here because we handle the case of having #cvargs prior
            return NewExprTypeVariadic(pkg, start, parseType(p), TypeVariadicFlagCVargs);
        }

        case TK_Mul: {
            Position start = p->tok.pos;
            nextToken();
            Expr *type = parseType(p);
            return NewExprTypePointer(pkg, start, type);
        }

        case TK_Directive: {
            if (p->tok.val.ident == internLocation || p->tok.val.ident == internFile || p->tok.val.ident == internLine || p->tok.val.ident == internFunction) {
                Expr *expr = NewExprLocationDirective(pkg, p->tok.pos, p->tok.val.ident);
                nextToken();
                return expr;
            } else if (p->tok.val.ident == internCVargs) {
                goto caseEllipsis;
            }
            ReportError(p->package, SyntaxError, p->tok.pos, "Unexpected directive '%s'", p->tok.val.ident);
            break;
        }

        case TK_Keyword: {
            if (p->tok.val.ident == Keyword_fn) {
                return parseFunctionType(p);
            } else if (p->tok.val.ident == Keyword_nil) {
                Expr *expr = NewExprLitNil(pkg, p->tok.pos);
                nextToken();
                return expr;
            } else if (p->tok.val.ident == Keyword_struct) {
                goto caseStruct;
            } else if (p->tok.val.ident == Keyword_union) {
                goto caseUnion;
            } else if (p->tok.val.ident == Keyword_enum) {
                goto caseEnum;
            }
            ReportError(p->package, SyntaxError, p->tok.pos, "Unexpected keyword '%s'", p->tok.val.ident);
            break;
        }

        caseStruct: {
            Position start = p->tok.pos;
            nextToken();

            // TODO(Brett, vdka): directives

            if (isToken(p, TK_Lparen)) {
                // TODO(Brett, vdka): polymorphic structs
                UNIMPLEMENTED();
            }

            expectToken(p, TK_Lbrace);

            DynamicArray(AggregateItem) items = NULL;

            while (!isToken(p, TK_Rbrace)) {
                Position start = p->tok.pos;

                DynamicArray(const char *) names = parseIdentList(p);

                expectToken(p, TK_Colon);

                Expr *type = parseType(p);

                AggregateItem item = {.start = start, .names = names, .type = type};
                ArrayPush(items, item);

                if (isToken(p, TK_Rbrace)) {
                    break;
                }

                expectTerminator(p);
            }

            expectToken(p, TK_Rbrace);

            return NewExprTypeStruct(pkg, start, items);
        }

        caseUnion: {
            Position start = p->tok.pos;
            nextToken();

            // TODO(Brett, vdka): directives

            expectToken(p, TK_Lbrace);

            DynamicArray(AggregateItem) items = NULL;

            while (!isToken(p, TK_Rbrace)) {
                Position start = p->tok.pos;

                DynamicArray(const char *) names = parseIdentList(p);

                expectToken(p, TK_Colon);

                Expr *type = parseType(p);

                AggregateItem item = {.start = start, .names = names, .type = type};
                ArrayPush(items, item);

                if (isToken(p, TK_Rbrace)) {
                    break;
                }

                expectTerminator(p);
            }

            expectToken(p, TK_Rbrace);

            return NewExprTypeUnion(pkg, start, items);
        }

        caseEnum: {
            Position start = p->tok.pos;
            Expr *explicitType = NULL;

            nextToken();

            if (!isToken(p, TK_Lbrace) && !isToken(p, TK_Directive)) {
                explicitType = parseType(p);
            }

            expectToken(p, TK_Lbrace);

            DynamicArray(EnumItem) items = NULL;
            while (!isToken(p, TK_Rbrace)) {
                Position start = p->tok.pos;
                const char *name = parseIdent(p);
                Expr *init = NULL;

                if (matchToken(p, TK_Assign)) {
                    ReportError(p->package, SyntaxError, p->tok.pos, "Enum values are established at compile time and declared using '::'");

                    // TODO(Brett): discard remaining stmt
                    continue;
                }

                if (matchToken(p, TK_Colon)) {
                    expectToken(p, TK_Colon);
                    init = parseExpr(p, true);
                }

                EnumItem item = {.start = start, .name = name, .init = init};
                ArrayPush(items, item);

                if (isToken(p, TK_Rbrace)) {
                    break;
                }

                expectTerminator(p);
            }

            expectToken(p, TK_Rbrace);

            return NewExprTypeEnum(pkg, start, explicitType, items);
        }

        default:
            ReportError(p->package, SyntaxError, p->tok.pos, "Unexpected token '%s'", DescribeToken(p->tok));
    }

    Position start = p->tok.pos;
    nextToken();
    return NewExprInvalid(pkg, start, p->tok.pos);
}

Expr *parseExprPrimary(Parser *p, b32 noCompoundLiteral) {
    Package *pkg = p->package;
    Expr *x = parseExprAtom(p);
    for (;;) {
        switch (p->tok.kind) {
            case TK_Dot: {
                nextToken();
                x = NewExprSelector(pkg, x, parseIdent(p), p->prevEnd);
                continue;
            }

            case TK_Lbrack: {
                nextToken();
                if (matchToken(p, TK_Colon)) {
                    if (matchToken(p, TK_Rbrack)) {
                        x = NewExprSlice(pkg, x, NULL, NULL, p->prevEnd);
                        continue;
                    }
                    Expr *hi = parseExpr(p, noCompoundLiteral);
                    expectToken(p, TK_Rbrack);
                    x = NewExprSlice(pkg, x, NULL, hi, p->prevEnd);
                    continue;
                }
                Expr *index = parseExpr(p, noCompoundLiteral);
                if (matchToken(p, TK_Colon)) {
                    if (matchToken(p, TK_Rbrack)) {
                        x = NewExprSlice(pkg, x, index, NULL, p->prevEnd);
                        continue;
                    }
                    Expr *hi = parseExpr(p, noCompoundLiteral);
                    expectToken(p, TK_Rbrack);
                    x = NewExprSlice(pkg, x, index, hi, p->prevEnd);
                    continue;
                }
                expectToken(p, TK_Rbrack);
                x = NewExprSubscript(pkg, x, index, p->prevEnd);
                continue;
            }

            case TK_Lparen: {
                nextToken();
                DynamicArray(Expr_KeyValue *) args = NULL;
                if (!isToken(p, TK_Rparen)) {
                    Expr_KeyValue *arg = AllocAst(pkg, sizeof(Expr_KeyValue));
                    arg->start = p->tok.pos;
                    arg->value = parseExpr(p, noCompoundLiteral);
                    if (isToken(p, TK_Colon) && arg->value->kind == ExprKind_Ident) {
                        arg->key = arg->value;
                        arg->value = parseExpr(p, noCompoundLiteral);
                    }
                    ArrayPush(args, arg);
                    while (matchToken(p, TK_Comma)) {
                        if (isToken(p, TK_Rparen)) break; // Allow trailing comma in argument list

                        arg->start = p->tok.pos;
                        arg->value = parseExpr(p, noCompoundLiteral);
                        if (isToken(p, TK_Colon) && arg->value->kind == ExprKind_Ident) {
                            arg->key = arg->value;
                            arg->value = parseExpr(p, noCompoundLiteral);
                        }
                        ArrayPush(args, arg);
                    }
                }
                expectToken(p, TK_Rparen);
                x = NewExprCall(pkg, x, args, p->prevEnd);
                continue;
            }

            case TK_Lbrace: {
                if (x->kind == ExprKind_TypeFunction) {
                    Stmt_Block *body = parseBlock(p);
                    x = NewExprLitFunction(pkg, x, body, 0);
                    continue;
                }
                if (noCompoundLiteral) return x;
                nextToken();
                DynamicArray(Expr_KeyValue *) elements = NULL;
                if (!isToken(p, TK_Rbrace)) {
                    ArrayPush(elements, parseExprCompoundField(p));
                    while (matchToken(p, TK_Comma)) {
                        if (isToken(p, TK_Rbrace)) break;
                        ArrayPush(elements, parseExprCompoundField(p));
                    }
                }
                expectToken(p, TK_Rbrace);
                x = NewExprLitCompound(pkg, x->start, x, elements, p->prevEnd);
                continue;
            }

            default:
                return x;
        }
    }
}

Expr *parseExprUnary(Parser *p, b32 noCompoundLiteral) {
    if (matchToken(p, TK_Mul)) {
        return NewExprTypePointer(p->package, p->prevStart, parseType(p));
    }
    switch (p->tok.kind) {
        case TK_Add: case TK_Sub: case TK_Not: case TK_BNot: case TK_Xor: case TK_And: case TK_Lss: {
            TokenKind op = p->tok.kind;
            nextToken();
            return NewExprUnary(p->package, p->prevStart, op, parseExprPrimary(p, noCompoundLiteral));
        }
        default:
            return parseExprPrimary(p, noCompoundLiteral);
    }
}

Expr *parseExprBinary(Parser *p, i32 prec1, b32 noCompoundLiteral) {
    Expr *lhs = parseExprUnary(p, noCompoundLiteral);
    for (;;) {
        TokenKind op = p->tok.kind;
        Position pos = p->tok.pos;
        i32 precedence = PrecedenceForTokenKind[op];
        if (precedence < prec1) return lhs;
        nextToken();
        if (op == TK_Question) {
            Expr *pass = NULL;
            if (!isToken(p, TK_Colon)) {
                pass = parseExpr(p, noCompoundLiteral);
            }
            expectToken(p, TK_Colon);
            Expr *fail = parseExpr(p, noCompoundLiteral);
            return NewExprTernary(p->package, lhs, pass, fail);
        }
        Expr *rhs = parseExprBinary(p, precedence + 1, noCompoundLiteral);
        lhs = NewExprBinary(p->package, op, pos, lhs, rhs);
    }
}

Expr *parseExpr(Parser *p, b32 noCompoundLiteral) {
    return parseExprBinary(p, 1, noCompoundLiteral);
}

Expr_KeyValue *parseExprCompoundField(Parser *p) {
    Expr_KeyValue *field = AllocAst(p->package, sizeof(Expr_KeyValue));
    field->start = p->tok.pos;
    if (matchToken(p, TK_Lbrack)) {
        field->flags = KeyValueFlagIndex;
        field->key = parseExpr(p, false);
        expectToken(p, TK_Rbrack);
        expectToken(p, TK_Colon);
        field->value = parseExpr(p, false);
        return field;
    } else {
        field->value = parseExpr(p, false);
        if (matchToken(p, TK_Colon)) {
            field->key = field->value;
            if (field->key->kind != ExprKind_Ident) {
                ReportError(p->package, SyntaxError, p->prevStart, "Named initializer value must be an identifier or surrounded in '[]'");
            }
            field->value = parseExpr(p, false);
            return field;
        }
        return field;
    }
}

Expr *parseType(Parser *p) {
    // NOTE: Right now parseType passes right onto parseAtom but, if types can be returned from
    //   functions calls in the future (CTE) then we will want this.
    // It is also clearer for intent
    return parseExprAtom(p);
}

Expr_KeyValue *parseFunctionParam(Parser *p, u32 *nVarargs) {
    Expr_KeyValue *kv = AllocAst(p->package, sizeof(Expr_KeyValue));
    kv->start = p->tok.pos;
    if (isToken(p, TK_Ident)) {
        const char *name = parseIdent(p);
        expectToken(p, TK_Colon);
        kv->key = NewExprIdent(p->package, kv->start, name);
    }
    kv->value = parseType(p);
    if (kv->value->kind == ExprKind_TypeVariadic) {
        if (nVarargs) *nVarargs += 1;
    }
    return kv;
}

Expr *parseFunctionType(Parser *p) {
    Position start = p->tok.pos;
    nextToken();
    expectToken(p, TK_Lparen);
    DynamicArray(Expr_KeyValue *) params = NULL;
    u32 nVarargs = 0;
    b32 namedParameters = false;
    do {
        if (isToken(p, TK_Rparen)) break; // allow trailing ',' in parameter list
        DynamicArray(Expr *) exprs = parseExprList(p, false);
        Expr_KeyValue *kv = AllocAst(p->package, sizeof(Expr_KeyValue));
        if (matchToken(p, TK_Colon)) {
            namedParameters = true;
            Expr *type = parseType(p);
            for (size_t i = 0; i < ArrayLen(exprs); i++) {
                if (exprs[i]->kind != ExprKind_Ident) {
                    ReportError(p->package, SyntaxError, exprs[i]->start, "Expected identifier");
                    continue;
                }
                kv->key = exprs[i];
                kv->value = type;
                ArrayPush(params, kv);
            }
        } else if (nVarargs <= 1) {
            if (namedParameters) {
                ReportError(p->package, SyntaxError, exprs[0]->start, "Mixture of named and unnamed parameters is unsupported");
            }
            // The parameters are unnamed and the user may have entered a second variadic
            for (size_t i = 0; i < ArrayLen(exprs); i++) {
                if (exprs[i]->kind == ExprKind_TypeVariadic) {
                    nVarargs += 1;
                    if (nVarargs == 2) {
                        ReportError(p->package, SyntaxError, exprs[i]->start, "Expected at most 1 Variadic as the final parameter");
                    }
                }
                kv->value = exprs[i];
                ArrayPush(params, kv);
            }
        }
    } while (matchToken(p, TK_Comma));
    expectToken(p, TK_Rparen);
    expectToken(p, TK_RetArrow);
    DynamicArray(Expr *) results = NULL;
    if (matchToken(p, TK_Lparen)) {
        nVarargs = 0;
        namedParameters = false;
        do {
            if (isToken(p, TK_Rparen) && results != NULL) break; // allow trailing ',' in parameter list
            DynamicArray(Expr *) exprs = parseExprList(p, false);
            if (matchToken(p, TK_Colon)) {
                namedParameters = true;
                Expr *type = parseType(p);
                for (size_t i = 0; i < ArrayLen(exprs); i++) {
                    if (exprs[i]->kind != ExprKind_Ident) {
                        ReportError(p->package, SyntaxError, exprs[i]->start, "Expected identifier");
                        continue;
                    }
                    ArrayPush(results, type);
                }
            } else if (nVarargs <= 1) {
                if (namedParameters) {
                    ReportError(p->package, SyntaxError, exprs[0]->start, "Mixture of named and unnamed parameters is unsupported");
                }
                for (size_t i = 0; i < ArrayLen(exprs); i++) {
                    if (exprs[i]->kind == ExprKind_TypeVariadic) {
                        nVarargs += 1;
                        if (nVarargs == 1) {
                            ReportError(p->package, SyntaxError, exprs[i]->start, "Variadics are only valid in a functions parameters");
                        }
                    }
                    ArrayPush(results, exprs[i]);
                }
            }
        } while (matchToken(p, TK_Comma));
    } else {
        ArrayPush(results, parseType(p));
        while (matchToken(p, TK_Comma)) {
            ArrayPush(results, parseType(p));
        }
    }
    return NewExprTypeFunction(p->package, start, params, results);
}

DynamicArray(Expr *) parseExprList(Parser *p, b32 noCompoundLiteral) {
    DynamicArray(Expr *) exprs = NULL;
    ArrayPush(exprs, parseExpr(p, noCompoundLiteral));
    while (matchToken(p, TK_Comma)) {
        ArrayPush(exprs, parseExpr(p, noCompoundLiteral));
    }
    return exprs;
}

Stmt_Block *parseBlock(Parser *p) {
    Position start = p->tok.pos;
    expectToken(p, TK_Lbrace);
    DynamicArray(Stmt *) stmts = NULL;
    while (isNotRbraceOrEOF(p)) {
        ArrayPush(stmts, parseStmt(p));
    }
    expectToken(p, TK_Rbrace);
    Stmt_Block *block = AllocAst(p->package, sizeof(Stmt_Block));
    block->start = start;
    block->stmts = stmts;
    block->end = p->prevEnd;
    return block;
}

// isIdentList being non NULL indicates that an ident list is permitted (for ... in)
Stmt *parseSimpleStmt(Parser *p, b32 noCompoundLiteral, b32 *isIdentList) {
    Package *pkg = p->package;
    Position start = p->tok.pos;

    DynamicArray(Expr *) exprs = parseExprList(p, noCompoundLiteral);
    switch (p->tok.kind) {
        case TK_Assign: {
            nextToken();
            DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
            return NewStmtAssign(pkg, start, exprs, rhs);
        }

        case TK_AddAssign: case TK_SubAssign: case TK_MulAssign: case TK_DivAssign:
        case TK_RemAssign: case TK_AndAssign: case TK_OrAssign:
        case TK_XorAssign: case TK_ShlAssign: case TK_ShrAssign: {
            Position pos = p->tok.pos;
            TokenKind op = TokenAssignOffset(p->tok.kind);
            nextToken();
            DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
            if (ArrayLen(rhs) > 1) {
                ReportError(p->package, SyntaxError, pos, "Only regular assignment may have multiple left or right values");
            }
            rhs[0] = NewExprBinary(pkg, op, pos, exprs[0], rhs[0]);
            return NewStmtAssign(pkg, start, exprs, rhs);
        }

        case TK_Colon: {
            nextToken();
            if (ArrayLen(exprs) == 1 && exprs[0]->kind == ExprKind_Ident && (matchToken(p, TK_Terminator) || isTokenEof(p))) {
                return NewStmtLabel(pkg, start, exprs[0]->Ident.name);
            }
            DynamicArray(Expr_Ident *) idents = NULL;
            for (size_t i = 0; i < ArrayLen(exprs); i++) {
                if (exprs[i]->kind != ExprKind_Ident) {
                    ReportError(p->package, SyntaxError, exprs[i]->start, "Expected identifier");
                }
                ArrayPush(idents, &exprs[i]->Ident);
            }
            ArrayFree(exprs);
            if (matchToken(p, TK_Assign)) {
                DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
                return (Stmt *) NewDeclVariable(pkg, start, idents, NULL, rhs);
            }
            if (matchToken(p, TK_Colon)) {
                DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
                return (Stmt *) NewDeclConstant(pkg, start, idents, NULL, rhs);
            }
            Expr *type = parseExpr(p, noCompoundLiteral);
            if (matchToken(p, TK_Assign)) {
                DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
                return (Stmt *) NewDeclVariable(pkg, start, idents, type, rhs);
            }
            if (matchToken(p, TK_Colon)) {
                DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
                return (Stmt *) NewDeclConstant(pkg, start, idents, type, rhs);
            }
            return (Stmt *) NewDeclVariable(pkg, start, idents, type, NULL);
        }

        default:
            break;
    }

    if (ArrayLen(exprs) > 1 && isIdentList) {
        *isIdentList = true;
        DynamicArray(Expr_Ident *) idents = NULL;
        for (size_t i = 0; i < ArrayLen(exprs); i++) {
            if (exprs[i]->kind != ExprKind_Ident) {
                ReportError(p->package, SyntaxError, exprs[i]->start, "Expected identifier");
            }
            ArrayPush(idents, &exprs[i]->Ident);
        }
        ArrayFree(exprs);
        return (Stmt *) idents;
    } else if (ArrayLen(exprs) > 1) {
        ReportError(p->package, SyntaxError, exprs[1]->start, "Expected single expression");
    }

    return (Stmt *) exprs[0];
}

Stmt *parseStmt(Parser *p) {
    Package *pkg = p->package;
    Position start = p->tok.pos;

    switch (p->tok.kind) {
        exprStart:
        case TK_Ident: case TK_Int: case TK_Float: case TK_String: case TK_Lparen:
        case TK_Mul: case TK_Lbrack: case TK_Ellipsis: case TK_Dollar: // Beginning of Atom's
        case TK_Add: case TK_Sub: case TK_Not: case TK_BNot: case TK_Xor: case TK_And: case TK_Lss: // Unary's
        {
            Stmt *s = parseSimpleStmt(p, false, NULL);
            expectTerminator(p);
            return s;
        }

        case TK_Lbrace: {
            Stmt_Block *block = parseBlock(p);
            return NewStmtBlock(pkg, block->start, block->stmts, block->end);
        }

        case TK_Directive: {
            if (isDirective(p, internFile) || isDirective(p, internLine) || isDirective(p, internLocation) || isDirective(p, internFunction)) goto exprStart;
            else if (isDirective(p, internCVargs)) goto exprStart;
            UNIMPLEMENTED();
            return NULL;
        }

        case TK_Keyword: {
            if (isKeyword(p, Keyword_struct) || isKeyword(p, Keyword_union) ||
                isKeyword(p, Keyword_enum) || isKeyword(p, Keyword_fn) || isKeyword(p, Keyword_nil)) goto exprStart;

            if (matchKeyword(p, Keyword_if)) {
                Expr *cond = parseExpr(p, true);
                Stmt *pass = parseStmt(p);
                Stmt *fail = NULL;
                if (matchKeyword(p, Keyword_else)) {
                    fail = parseStmt(p);
                }
                return NewStmtIf(pkg, start, cond, pass, fail);
            }
            if (matchKeyword(p, Keyword_defer)) {
                return NewStmtDefer(pkg, start, parseStmt(p));
            }
            if (isKeywordBranch(p, p->tok.val.s)) {
                const char *keyword = p->tok.val.s;
                nextToken();
                if (matchToken(p, TK_Terminator)) {
                    return NewStmtGoto(pkg, start, keyword, NULL);
                }
                if (keyword == Keyword_fallthrough) {
                    expectTerminator(p);
                    return NewStmtGoto(pkg, start, keyword, NULL);
                }
                Expr_Ident *ident = AllocAst(pkg, sizeof(Expr_Ident));
                ident->start = p->tok.pos;
                ident->name = parseIdent(p);
                return NewStmtGoto(pkg, start, keyword, ident);
            }
            if (matchKeyword(p, Keyword_for)) {
                if (isToken(p, TK_Lbrace)) {
                    return NewStmtFor(pkg, start, NULL, NULL, NULL, parseBlock(p));
                }
                Stmt *s1, *s2, *s3;
                s1 = s2 = s3 = NULL;
                if (!isToken(p, TK_Lbrace) && !isToken(p, TK_Terminator)) {
                    b32 isIdentList = false;
                    s2 = parseSimpleStmt(p, true, &isIdentList);
                    if (isIdentList) {
                        DynamicArray(Expr_Ident *) idents = (DynamicArray(Expr_Ident *)) s2;
                        Expr *aggregate = NULL;
                        if (isTokenIdent(p, internIn)) {
                            nextToken();
                            Expr_Ident *valueName = NULL;
                            Expr_Ident *indexName = NULL;
                            if (ArrayLen(idents) > 0) valueName = idents[0];
                            if (ArrayLen(idents) > 1) indexName = idents[1];
                            if (ArrayLen(idents) > 2) {
                                ReportError(p->package, SyntaxError, idents[2]->start, "For in iteration must provide at most 2 names to assign (value, index)");
                            }
                            aggregate = parseExpr(p, true);
                            Stmt_Block *body = parseBlock(p);
                            return NewStmtForIn(pkg, start, valueName, indexName, aggregate, body);
                        } else {
                            ReportError(p->package, SyntaxError, p->tok.pos, "Expected single expression or 'in' for iterator");
                        }
                    }
                }
                if (matchToken(p, TK_Terminator)) {
                    s1 = s2;
                    s2 = NULL;
                    if (!isToken(p, TK_Terminator)) {
                        s2 = parseSimpleStmt(p, true, NULL);
                    }
                    expectToken(p, TK_Terminator);
                    if (!isToken(p, TK_Rbrace) && !isToken(p, TK_Terminator)) {
                        s3 = parseSimpleStmt(p, true, NULL);
                    }
                }
                if (!isExpr(s2)) {
                    ReportError(p->package, SyntaxError, s2->start, "Expected expression, got '%s'", AstDescriptions[s2->kind]);
                }

                Stmt_Block *body = parseBlock(p);
                return NewStmtFor(pkg, start, s1, (Expr *) s2, s3, body);
            }
            if (matchKeyword(p, Keyword_return)) {
                DynamicArray(Expr *) exprs = NULL;
                if (!isToken(p, TK_Terminator) && !isToken(p, TK_Rbrace)) {
                    exprs = parseExprList(p, false);
                }
                expectToken(p, TK_Terminator);
                return NewStmtReturn(pkg, start, exprs);
            }
            if (matchKeyword(p, Keyword_switch)) UNIMPLEMENTED();
            if (matchKeyword(p, Keyword_using)) UNIMPLEMENTED();


            // Report an error
            return NULL;
        }
        default:
            UNIMPLEMENTED();
            return NULL;
    }

    return NewStmtInvalid(pkg, start, start);
}

DynamicArray(Stmt *) parseStmts(Parser *p) {
    DynamicArray(Stmt *) stmts = NULL;
    while (!isTokenEof(p)) {
        ArrayPush(stmts, parseStmt(p));
    }
    return stmts;
}

void parsePackage(Package *package) {
    const char *code = ReadEntireFile(package->fullPath);
    if (!code) {
        ReportError(package, FatalError, (Position){ .name = package->path }, "Failed to read source file");
        return;
    }
    Lexer lexer = MakeLexer(code, package->path);
    Token tok = NextToken(&lexer);
    Parser parser = {lexer, .tok = tok, package};
    package->stmts = parseStmts(&parser);

    for (size_t i = 0; i < ArrayLen(package->stmts); i++) {
        CheckerWork *work = ArenaAlloc(&checkingQueue.arena, sizeof(CheckerWork));
        work->package = package;
        work->stmt = package->stmts[i];
        QueueEnqueue(&checkingQueue, work);
    }
}

#undef NextToken

#if TEST

Package testPackage = {0};
Parser newTestParser(const char *stream) {
    Lexer lex = MakeLexer(stream, NULL);
    Token tok = NextToken(&lex);
    ArenaFree(&testPackage.arena);
    ArrayFree(testPackage.diagnostics.errors);
    ArenaFree(&testPackage.diagnostics.arena);
    Parser p = {lex, .tok = tok, &testPackage};
    return p;
}
#endif

#if TEST
void test_parseExprAtom() {
#define ASSERT_EXPR_KIND(expected) \
    expr = parseExprAtom(&p); \
    ASSERT(expr->kind == expected); \
    ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a 1 1.0 #line nil");
    ASSERT_EXPR_KIND(ExprKind_Ident);
    ASSERT_EXPR_KIND(ExprKind_LitInt);
    ASSERT_EXPR_KIND(ExprKind_LitFloat);
    ASSERT_EXPR_KIND(ExprKind_LocationDirective);
    ASSERT_EXPR_KIND(ExprKind_LitNil);

    p = newTestParser("fn () -> a []a [2]a *a ..a $a struct { anA: a } enum { A }");
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeSlice);
    ASSERT_EXPR_KIND(ExprKind_TypeArray);
    ASSERT_EXPR_KIND(ExprKind_TypePointer);
    ASSERT_EXPR_KIND(ExprKind_TypeVariadic);
    ASSERT_EXPR_KIND(ExprKind_TypePolymorphic);
    ASSERT_EXPR_KIND(ExprKind_TypeStruct);
    ASSERT_EXPR_KIND(ExprKind_TypeEnum);

    p = newTestParser("fn (a, b: u32, c: $T, d: #cvargs ..any) -> (a: u32, b: u32)"
                      "fn (a, b: $T) -> (b, a: T)");
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);

#undef ASSERT_EXPR_KIND
}
#endif

#if TEST
void test_parseExprPrimary() {
#define ASSERT_EXPR_KIND(expected) \
    expr = parseExprPrimary(&p, false); \
    ASSERT(expr->kind == expected); \
    ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a.a.a.a.a.a.a.a.a.a.a.a");
    ASSERT_EXPR_KIND(ExprKind_Selector);

    p = newTestParser("a[:] a[5:] a[:5] a[5:5] a[5]");
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT(expr->Slice.lo != NULL);
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT(expr->Slice.hi != NULL);
    ASSERT_EXPR_KIND(ExprKind_Slice);
    ASSERT(expr->Slice.lo != NULL && expr->Slice.hi != NULL);
    ASSERT_EXPR_KIND(ExprKind_Subscript);

    p = newTestParser("a() a(a,\n) a(a, b, c,)");
    ASSERT_EXPR_KIND(ExprKind_Call);
    ASSERT_EXPR_KIND(ExprKind_Call);
    ASSERT(ArrayLen(expr->Call.args) == 1);
    ASSERT_EXPR_KIND(ExprKind_Call);
    ASSERT(ArrayLen(expr->Call.args) == 3);

    p = newTestParser("A{} A{1, a: 2, 3,} A{[23]: 1, 2, 3}");
    ASSERT_EXPR_KIND(ExprKind_LitCompound);
    ASSERT_EXPR_KIND(ExprKind_LitCompound);
    ASSERT(ArrayLen(expr->LitCompound.elements) == 3);
    ASSERT_EXPR_KIND(ExprKind_LitCompound);
    ASSERT(ArrayLen(expr->LitCompound.elements) == 3);
    ASSERT(expr->LitCompound.elements[0]->flags & KeyValueFlagIndex);

#undef ASSERT_EXPR_KIND
}
#endif

#if TEST
void test_parseExprUnary() {
#define ASSERT_EXPR_KIND(expected) \
expr = parseExprUnary(&p, false); \
ASSERT(expr->kind == expected); \
ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("+5 -5 ~a ^a !a &a <a");
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);

#undef ASSERT_EXPR_KIND
}
#endif

#if TEST
void test_parseExprBinary() {
#define ASSERT_EXPR_KIND(expected) \
expr = parseExprBinary(&p, 1, false); \
ASSERT(expr->kind == expected); \
ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a + b * c");
    ASSERT_EXPR_KIND(ExprKind_Binary);
    ASSERT(expr->Binary.op == TK_Add);

    p = newTestParser("(a + b) * c");
    ASSERT_EXPR_KIND(ExprKind_Binary);
    ASSERT(expr->Binary.op == TK_Mul);

#undef ASSERT_EXPR_KIND
}
#endif

#if TEST
void test_parseExprTernary() {
#define ASSERT_EXPR_KIND(expected) \
expr = parseExprBinary(&p, 1, false); \
ASSERT(expr->kind == expected); \
ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a ? b : c");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

    p = newTestParser("a ?: b");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

#undef ASSERT_EXPR_KIND
}
#endif

#if TEST
void test_parseSimpleStmt() {
#define ASSERT_STMT_KIND(expected) \
stmt = parseSimpleStmt(&p, false, NULL); \
ASSERT(stmt->kind == expected); \
ASSERT(!testPackage.diagnostics.errors)

    Stmt *stmt;

    Parser p = newTestParser("a := b");
    ASSERT_STMT_KIND(DeclKind_Variable);

#undef ASSERT_STMT_KIND
}
#endif

#if TEST
void test_parseStmt() {
#define ASSERT_STMT_KIND(expected) \
stmt = parseStmt(&p); \
ASSERT(stmt->kind == expected); \
ASSERT(!testPackage.diagnostics.errors)

    Stmt *stmt;
    Parser p;
    
    p = newTestParser("a := b");
    ASSERT_STMT_KIND(DeclKind_Variable);

    p = newTestParser("a :: b");
    ASSERT_STMT_KIND(DeclKind_Constant);

    p = newTestParser("a:");
    ASSERT_STMT_KIND(StmtKind_Label);

    p = newTestParser("for a; b; c {}");
    ASSERT_STMT_KIND(StmtKind_For);

    p = newTestParser("for a, b in foo {}");
    ASSERT_STMT_KIND(StmtKind_ForIn);

#undef ASSERT_STMT_KIND
}
#endif

#if TEST
void test_parseStruct() {
#define ASSERT_EXPR_KIND(expected) \
    expr = parseExprAtom(&p); \
    ASSERT(expr->kind == expected); \
    ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;
    Parser p;

    /*
     *  A mini-test-suite for structs
     */

    p = newTestParser("struct { a, b, c: u32 }");
    ASSERT_EXPR_KIND(ExprKind_TypeStruct);
}
#endif

#if TEST
void test_parseUnion() {
#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!testPackage.diagnostics.errors)

    Expr *expr;
    Parser p;

    /*
     *  A mini-test-suite for unions
     */

    p = newTestParser("union { a, b, c: u32 }");
    ASSERT_EXPR_KIND(ExprKind_TypeUnion);
}
#endif
