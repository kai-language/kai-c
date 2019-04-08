
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

SourceRange rangeFromTokens(Token start, Token end) {
    u32 endOffset = (u32) (end.end - start.start) + start.pos.offset;
    SourceRange range = {
        start.pos.name,
        start.pos.offset, endOffset,
        start.pos.line, start.pos.column
    };
    return range;
}

SourceRange rangeFromTokenToEndOffset(Token token, u32 endOffset) {
    SourceRange range = {
        token.pos.name,
        token.pos.offset, endOffset,
        token.pos.line, token.pos.column,
    };
    return range;
}

SourceRange rangeFromNodes(AstNode start, AstNode end) {
    Stmt *s = (Stmt*) start;
    Stmt *e = (Stmt*) end;
    SourceRange range = {
        s->pos.name,
        s->pos.offset, e->pos.endOffset,
        s->pos.line, s->pos.column,
    };
    return range;
}

SourceRange rangeFromTokenToEndOfNode(Token token, AstNode node) {
    Stmt *stmt = (Stmt *) node;
    SourceRange range = {
        token.pos.name,
        token.pos.offset, stmt->pos.endOffset,
        token.pos.line, token.pos.column,
    };
    return range;
}

typedef struct Parser Parser;
struct Parser {
    Lexer lexer;
    Position prevStart;
    Position prevEnd;
    Token tok;
    Package *package;

    const char *callingConvention;
    const char *linkPrefix;
};

#define nextToken() \
    p->prevStart = p->tok.pos; \
    p->prevEnd = p->lexer.pos; \
    p->tok = NextToken(&p->lexer)

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
    return isToken(p, TK_Directive) && p->tok.val.ident == ident;
}

b32 isPrefixOrLoneDirective(Parser *p) {
    if (p->tok.val.ident == internCallConv) return true;
    if (p->tok.val.ident == internLinkPrefix) return true;
    if (p->tok.val.ident == internForeign) return true;
    if (p->tok.val.ident == internImport) return true;
    return false;
}

b32 isSuffixDirective(Parser *p) {
    if (p->tok.val.ident == internLinkName) return true;
    return false;
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
    ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Expected token %s, got %s", DescribeTokenKind(kind), DescribeToken(p->tok));
    nextToken();
    return false;
}

b32 expectTerminator(Parser *p) {
    if (matchToken(p, TK_Terminator) || isToken(p, TK_Rbrace) || isToken(p, TK_Eof)) return true;
    ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Expected terminator, got %s", DescribeToken(p->tok));
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
KeyValue parseExprCompoundField(Parser *p);
DynamicArray(Expr *) parseExprList(Parser *p, b32 noCompoundLiteral);
Stmt *parseBlock(Parser *p);
Stmt *parseStmt(Parser *p);

Expr *parseExprAtom(Parser *p) {
    Package *pkg = p->package;
    Token start = p->tok;

    switch (p->tok.kind) {
        case TK_Ident: {
            SourceRange range = rangeFromTokens(start, start);
            Expr *e = NewExprIdent(pkg, range, p->tok.val.ident);
            nextToken();
            if (p->tok.kind == TK_Dot) {  // package.Member
                nextToken();
                SourceRange range = rangeFromTokens(start, p->tok);
                const char *ident = parseIdent(p);
                return NewExprSelector(pkg, range, e, ident);
            }
            return e;
        }

        case TK_Int: {
            SourceRange range = rangeFromTokens(start, start);
            Expr *e = NewExprLitInt(pkg, range, p->tok.val.i);
            nextToken();
            return e;
        }

        case TK_Float: {
            SourceRange range = rangeFromTokens(start, start);
            Expr *e = NewExprLitFloat(pkg, range, p->tok.val.f);
            nextToken();
            return e;
        }

        case TK_String: {
            SourceRange range = rangeFromTokens(start, start);
            Expr *e = NewExprLitString(pkg, range, p->tok.val.s);
            nextToken();
            return e;
        }

        case TK_Lparen: {
            nextToken();
            Expr *expr = parseExpr(p, false);
            Token end = p->tok;
            expectToken(p, TK_Rparen);

            SourceRange range = rangeFromTokens(start, end);
            return NewExprParen(pkg, range, expr);
        }

        case TK_Lbrack: {
            nextToken();
            if (matchToken(p, TK_Rbrack)) {
                Expr *type = parseType(p);
                SourceRange range = rangeFromTokenToEndOfNode(start, type);
                return NewExprTypeSlice(pkg, range, type);
            }
            Expr *length = NULL;
            if (!matchToken(p, TK_Ellipsis)) {
                length = parseExpr(p, false);
            }
            expectToken(p, TK_Rbrack);
            Expr *type = parseType(p);
            SourceRange range = rangeFromTokenToEndOfNode(start, type);
            return NewExprTypeArray(pkg, range, length, type);
        }

        case TK_Lbrace: {
            nextToken();

            DynamicArray(KeyValue) elements = NULL;

            if (!isToken(p, TK_Rbrace)) {
                ArrayPush(elements, parseExprCompoundField(p));
                while (matchToken(p, TK_Comma)) {
                    if (isToken(p, TK_Rbrace)) break;
                    ArrayPush(elements, parseExprCompoundField(p));
                }
            }

            SourceRange range = rangeFromTokens(start, p->tok);
            expectToken(p, TK_Rbrace);
            return NewExprLitCompound(pkg, range, NULL, elements);
        }

        case TK_Dollar: {
            nextToken();
            Token end = p->tok;
            const char *name = parseIdent(p);
            SourceRange range = rangeFromTokens(start, end);
            return NewExprTypePolymorphic(pkg, range, name);
        }

        caseEllipsis: // For `case TK_Directive` for #cvargs
        case TK_Ellipsis: {
            u8 flags = 0;
            flags |= matchDirective(p, internCVargs) ? TypeVariadicFlag_CVargs : 0;
            expectToken(p, TK_Ellipsis); // NOTE: We must expect here because we handle the case of having #cvargs prior
            Expr *type = parseType(p);
            SourceRange range = rangeFromTokenToEndOfNode(start, type);
            return NewExprTypeVariadic(pkg, range, type, flags);
        }

        case TK_Mul: {
            nextToken();
            Expr *type = parseType(p);
            SourceRange range = rangeFromTokenToEndOfNode(start, type);
            return NewExprTypePointer(pkg, range, type);
        }

        case TK_Directive: {
            if (p->tok.val.ident == internLocation || p->tok.val.ident == internFile ||
                p->tok.val.ident == internLine || p->tok.val.ident == internFunction) {
                SourceRange range = rangeFromTokens(start, start);
                Expr *expr = NewExprLocationDirective(pkg, range, p->tok.val.ident);
                nextToken();
                return expr;
            } else if (p->tok.val.ident == internCVargs) {
                goto caseEllipsis;
            }
            ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Unexpected directive '%s'", p->tok.val.ident);
            break;
        }

        case TK_Keyword: {
            const char *ident = p->tok.val.ident;
            if (ident == Keyword_fn) {
                return parseFunctionType(p);
            } else if (ident == Keyword_nil) {
                SourceRange range = rangeFromTokens(start, start);
                Expr *expr = NewExprLitNil(pkg, range);
                nextToken();
                return expr;
            } else if (ident == Keyword_struct) {
                goto caseStruct;
            } else if (ident == Keyword_union) {
                goto caseUnion;
            } else if (ident == Keyword_enum) {
                goto caseEnum;
            } else if (ident == Keyword_cast) {
                goto caseCast;
            } else if (ident == Keyword_autocast) {
                goto caseAutocast;
            }
            ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Unexpected keyword '%s'", p->tok.val.ident);
            break;
        }

        caseCast: { // See `case TK_Keyword:`
            nextToken();
            expectToken(p, TK_Lparen);
            Expr *type = parseType(p);
            expectToken(p, TK_Rparen);
            Expr *expr = parseExpr(p, false);
            SourceRange range = rangeFromTokenToEndOfNode(start, expr);
            return NewExprCast(pkg, range, type, expr);
        }

        caseAutocast: { // See `case TK_Keyword:`
            nextToken();
            Expr *expr = parseExpr(p, false);
            SourceRange range = rangeFromTokenToEndOfNode(start, expr);
            return NewExprAutocast(pkg, range, expr);
        }

        caseStruct: { // See `case TK_Keyword:`
            nextToken();

            // TODO(Brett, vdka): directives

            if (isToken(p, TK_Lparen)) {
                // TODO(Brett, vdka): polymorphic structs
                UNIMPLEMENTED();
            }

            expectToken(p, TK_Lbrace);

            DynamicArray(AggregateItem) items = NULL;

            while (!isToken(p, TK_Rbrace)) {
                Token start = p->tok;

                DynamicArray(const char *) names = parseIdentList(p);

                expectToken(p, TK_Colon);

                Expr *type = parseType(p);

                SourceRange range = rangeFromTokenToEndOfNode(start, type);
                AggregateItem item = {range, .names = names, .type = type};
                ArrayPush(items, item);

                if (isToken(p, TK_Rbrace)) {
                    break;
                }

                expectTerminator(p);
                if (isTokenEof(p)) break;
            }

            SourceRange range = rangeFromTokens(start, p->tok);
            expectToken(p, TK_Rbrace);

            return NewExprTypeStruct(pkg, range, items);
        }

        caseUnion: { // See `case TK_Keyword:`
            nextToken();

            // TODO(Brett, vdka): directives

            expectToken(p, TK_Lbrace);

            DynamicArray(AggregateItem) items = NULL;

            while (!isToken(p, TK_Rbrace)) {
                Token start = p->tok;

                DynamicArray(const char *) names = parseIdentList(p);

                expectToken(p, TK_Colon);

                Expr *type = parseType(p);
                SourceRange range = rangeFromTokenToEndOfNode(start, type);
                AggregateItem item = {range, .names = names, .type = type};
                ArrayPush(items, item);

                if (isToken(p, TK_Rbrace)) {
                    break;
                }

                expectTerminator(p);
                if (isTokenEof(p)) break;
            }

            SourceRange range = rangeFromTokens(start, p->tok);
            expectToken(p, TK_Rbrace);

            return NewExprTypeUnion(pkg, range, items);
        }

        caseEnum: { // See `case TK_Keyword:`
            nextToken();

            Expr *explicitType = NULL;
            if (!isToken(p, TK_Lbrace) && !isToken(p, TK_Directive)) {
                explicitType = parseType(p);
            }

            expectToken(p, TK_Lbrace);

            DynamicArray(EnumItem) items = NULL;
            while (!isToken(p, TK_Rbrace)) {
                Token start = p->tok;
                const char *name = parseIdent(p);
                Expr *init = NULL;

                if (matchToken(p, TK_Assign)) {
                    ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Enum values are established at compile time and declared using '::'");

                    // TODO(Brett): discard remaining stmt
                    continue;
                }

                if (matchToken(p, TK_Colon)) {
                    expectToken(p, TK_Colon);
                    init = parseExpr(p, true);
                }

                SourceRange range = init ? rangeFromTokenToEndOfNode(start, init) : rangeFromTokens(start, start);
                EnumItem item = {range, .name = name, .init = init};
                ArrayPush(items, item);

                if (isToken(p, TK_Rbrace)) {
                    break;
                }

                expectTerminator(p);
                if (isTokenEof(p)) break;
            }

            SourceRange range = rangeFromTokens(start, p->tok);
            expectToken(p, TK_Rbrace);

            return NewExprTypeEnum(pkg, range, explicitType, items);
        }

        default:
            ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Unexpected token '%s'", DescribeToken(p->tok));
    }

    SourceRange range = rangeFromTokens(start, start);
    nextToken();
    return NewExprInvalid(pkg, range);
}

Expr *parseExprPrimary(Parser *p, b32 noCompoundLiteral) {
    Package *pkg = p->package;
    Token start = p->tok;
    Expr *x = parseExprAtom(p);
    for (;;) {
        switch (p->tok.kind) {
            case TK_Dot: { // Selector
                nextToken();
                SourceRange range = rangeFromTokens(start, p->tok);
                x = NewExprSelector(pkg, range, x, parseIdent(p));
                continue;
            }

            case TK_Lbrack: { // Slice | Subscript
                nextToken();
                if (matchToken(p, TK_Colon)) {
                    Token end = p->tok;
                    if (matchToken(p, TK_Rbrack)) {
                        SourceRange range = rangeFromTokens(start, end);
                        x = NewExprSlice(pkg, range, x, NULL, NULL);
                        continue;
                    }
                    Expr *hi = parseExpr(p, noCompoundLiteral);
                    end = p->tok;
                    expectToken(p, TK_Rbrack);
                    SourceRange range = rangeFromTokens(start, end);
                    x = NewExprSlice(pkg, range, x, NULL, hi);
                    continue;
                }
                Expr *index = parseExpr(p, noCompoundLiteral);
                if (matchToken(p, TK_Colon)) {
                    Token end = p->tok;
                    if (matchToken(p, TK_Rbrack)) {
                        SourceRange range = rangeFromTokens(start, end);
                        x = NewExprSlice(pkg, range, x, index, NULL);
                        continue;
                    }
                    Expr *hi = parseExpr(p, noCompoundLiteral);
                    end = p->tok;
                    expectToken(p, TK_Rbrack);
                    SourceRange range = rangeFromTokens(start, end);
                    x = NewExprSlice(pkg, range, x, index, hi);
                    continue;
                }
                Token end = p->tok;
                expectToken(p, TK_Rbrack);
                SourceRange range = rangeFromTokens(start, end);
                x = NewExprSubscript(pkg, range, x, index);
                continue;
            }

            case TK_Lparen: { // Call Expr
                nextToken();
                DynamicArray(KeyValue) args = NULL;
                if (!isToken(p, TK_Rparen)) {
                    Token start = p->tok;

                    KeyValue arg = {0};
                    arg.value = parseExpr(p, noCompoundLiteral);
                    if (isToken(p, TK_Colon) && arg.value->kind == ExprKindIdent) {
                        arg.key = arg.value;
                        arg.value = parseExpr(p, noCompoundLiteral);
                    }
                    arg.pos = rangeFromTokenToEndOfNode(start, arg.value);
                    ArrayPush(args, arg);
                    while (matchToken(p, TK_Comma)) {
                        if (isToken(p, TK_Rparen)) break; // Allow trailing comma in argument list
                        start = p->tok;

                        KeyValue arg = {0};
                        arg.value = parseExpr(p, noCompoundLiteral);
                        if (isToken(p, TK_Colon) && arg.value->kind == ExprKindIdent) {
                            arg.key = arg.value;
                            arg.value = parseExpr(p, noCompoundLiteral);
                        }
                        arg.pos = rangeFromTokenToEndOfNode(start, arg.value);
                        ArrayPush(args, arg);
                    }
                }
                SourceRange range = rangeFromTokens(start, p->tok);
                expectToken(p, TK_Rparen);
                x = NewExprCall(pkg, range, x, args);
                continue;
            }

            case TK_Lbrace: {
                if (x->kind == ExprKindTypeFunction) {
                    Token startOfBlock = p->tok;
                    nextToken();
                    DynamicArray(Stmt *) stmts = NULL;
                    while (isNotRbraceOrEOF(p)) {
                        ArrayPush(stmts, parseStmt(p));
                        matchToken(p, TK_Terminator);
                    }
                    SourceRange blockRange = rangeFromTokens(startOfBlock, p->tok);
                    SourceRange functionRange = rangeFromTokens(start, p->tok);
                    expectToken(p, TK_Rbrace);
                    Stmt *body = NewStmtBlock(p->package, blockRange, stmts);
                    x = NewExprLitFunction(pkg, functionRange, x, body, 0);
                    continue;
                }
                if (noCompoundLiteral) return x;
                nextToken();

                matchToken(p, TK_Terminator);

                DynamicArray(KeyValue) elements = NULL;
                if (!isToken(p, TK_Rbrace)) {
                    ArrayPush(elements, parseExprCompoundField(p));
                    while (matchToken(p, TK_Comma)) {
                        if (isToken(p, TK_Rbrace)) break;
                        ArrayPush(elements, parseExprCompoundField(p));
                        matchToken(p, TK_Terminator);
                    }
                }
                SourceRange range = rangeFromTokens(start, p->tok);
                expectToken(p, TK_Rbrace);
                x = NewExprLitCompound(pkg, range, x, elements);
                continue;
            }

            default:
                return x;
        }
    }
}

Expr *parseExprUnary(Parser *p, b32 noCompoundLiteral) {
    Token start = p->tok;
    if (matchToken(p, TK_Mul)) {
        Expr *type = parseType(p);
        SourceRange range = rangeFromTokenToEndOfNode(start, type);
        return NewExprTypePointer(p->package, range, type);
    }
    switch (p->tok.kind) {
        case TK_Add: case TK_Sub: case TK_Not: case TK_BNot: case TK_Xor: case TK_And: case TK_Lss: {
            TokenKind op = p->tok.kind;
            nextToken();
            Expr *expr = parseExprUnary(p, noCompoundLiteral);
            SourceRange range = rangeFromTokenToEndOfNode(start, expr);
            return NewExprUnary(p->package, range, op, expr);
        }
        default:
            return parseExprPrimary(p, noCompoundLiteral);
    }
}

Expr *parseExprBinary(Parser *p, i32 prec1, b32 noCompoundLiteral) {
    Expr *lhs = parseExprUnary(p, noCompoundLiteral);
    for (;;) {
        Token op = p->tok;
        i32 precedence = PrecedenceForTokenKind[op.kind];
        if (precedence < prec1) return lhs;
        nextToken();
        if (op.kind == TK_Question) {
            // NOTE: Ternary supports missing pass expressions ie: `cond ?: default`
            Expr *pass = NULL;
            if (!isToken(p, TK_Colon)) {
                pass = parseExpr(p, noCompoundLiteral);
            }
            expectToken(p, TK_Colon);
            Expr *fail = parseExpr(p, noCompoundLiteral);
            SourceRange range = rangeFromNodes(lhs, fail);
            return NewExprTernary(p->package, range, lhs, pass, fail);
        }
        Expr *rhs = parseExprBinary(p, precedence + 1, noCompoundLiteral);
        SourceRange range = rangeFromNodes(lhs, rhs);
        lhs = NewExprBinary(p->package, range, op, lhs, rhs);
    }
}

Expr *parseExpr(Parser *p, b32 noCompoundLiteral) {
    return parseExprBinary(p, 1, noCompoundLiteral);
}

KeyValue parseExprCompoundField(Parser *p) {
    Token start = p->tok;
    KeyValue field = {0};
    if (matchToken(p, TK_Lbrack)) {
        field.flags = KeyValueFlag_Index;
        field.key = parseExpr(p, false);
        expectToken(p, TK_Rbrack);
        expectToken(p, TK_Colon);
        field.value = parseExpr(p, false);
        field.pos = rangeFromTokenToEndOfNode(start, field.value);
        return field;
    } else {
        field.value = parseExpr(p, false);
        if (matchToken(p, TK_Colon)) {
            field.key = field.value;
            if (field.key->kind != ExprKindIdent) {
                ReportError(p->package, SyntaxError, rangeFromPosition(p->prevStart), "Named initializer value must be an identifier or surrounded in '[]'");
            }
            field.value = parseExpr(p, false);
        }
        field.pos = rangeFromTokenToEndOfNode(start, field.value);
        return field;
    }
}

Expr *parseType(Parser *p) {
    // NOTE: Right now parseType passes right onto parseAtom but, if types can be returned from
    //   functions calls in the future (CTE) then we will want this.
    // It is also clearer for intent
    return parseExprAtom(p);
}

void parseFunctionParameters(u32 *nVarargs, b32 *namedParameters, Parser *p, DynamicArray(KeyValue) *params) {
    expectToken(p, TK_Lparen);
    *nVarargs = 0;
    *namedParameters = false;
    do {
        if (isToken(p, TK_Rparen)) break; // allow trailing ',' in parameter list
        DynamicArray(Expr *) exprs = parseExprList(p, false);
        if (matchToken(p, TK_Colon)) {
            *namedParameters = true;
            Expr *type = parseType(p);
            size_t numExprs = ArrayLen(exprs);
            for (size_t i = 0; i < numExprs; i++) {
                if (exprs[i]->kind != ExprKindIdent) {
                    ReportError(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
                    continue;
                }
                KeyValue kv = {0};
                kv.pos = exprs[i]->pos;
                kv.key = exprs[i];
                kv.value = type;
                ArrayPush(*params, kv);
            }
            if (type->kind == ExprKindTypeVariadic) {
                *nVarargs += 1;
                if (*nVarargs == 2) {
                    ReportError(p->package, SyntaxError, type->pos, "Expected at most 1 Variadic as the final parameter");
                }
            }
        } else if (*nVarargs <= 1) {
            if (*namedParameters) {
                ReportError(p->package, SyntaxError, exprs[0]->pos, "Mixture of named and unnamed parameters is unsupported");
            }
            // The parameters are unnamed and the user may have entered a second variadic
            size_t numExprs = ArrayLen(exprs);
            for (size_t i = 0; i < numExprs; i++) {
                if (exprs[i]->kind == ExprKindTypeVariadic) {
                    *nVarargs += 1;
                    if (*nVarargs == 2) {
                        ReportError(p->package, SyntaxError, exprs[i]->pos, "Expected at most 1 Variadic as the final parameter");
                    }
                }
                KeyValue kv = {0};
                kv.value = exprs[i];
                kv.pos = exprs[i]->pos;
                ArrayPush(*params, kv);
            }
        }
    } while (matchToken(p, TK_Comma));
    expectToken(p, TK_Rparen);
}

Expr *parseFunctionType(Parser *p) {
    Token start = p->tok;
    nextToken();
    DynamicArray(KeyValue) params = NULL;
    u32 nVarargs;
    b32 namedParameters;
    parseFunctionParameters(&nVarargs, &namedParameters, p, &params);
    expectToken(p, TK_RetArrow);

    SourceRange range;
    DynamicArray(Expr *) results = NULL;
    if (matchToken(p, TK_Lparen)) { // We need to handle labels in the result list eg. `(Node, ok: bool)`
        nVarargs = 0;
        namedParameters = false;
        do {
            if (isToken(p, TK_Rparen) && results != NULL) break; // allow trailing ',' in result list
            DynamicArray(Expr *) exprs = parseExprList(p, false);
            if (matchToken(p, TK_Colon)) {
                namedParameters = true;
                Expr *type = parseType(p);
                size_t numExprs = ArrayLen(exprs);
                for (size_t i = 0; i < numExprs; i++) {
                    if (exprs[i]->kind != ExprKindIdent) {
                        ReportError(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
                        continue;
                    }
                    ArrayPush(results, type);
                }
            } else if (nVarargs <= 1) {
                if (namedParameters) {
                    ReportError(p->package, SyntaxError, exprs[0]->pos, "Mixture of named and unnamed parameters is unsupported");
                }
                size_t numExprs = ArrayLen(exprs);
                for (size_t i = 0; i < numExprs; i++) {
                    if (exprs[i]->kind == ExprKindTypeVariadic) {
                        nVarargs += 1;
                        if (nVarargs == 1) {
                            ReportError(p->package, SyntaxError, exprs[i]->pos, "Variadics are only valid in a functions parameters");
                        }
                    }
                    ArrayPush(results, exprs[i]);
                }
            }
        } while (matchToken(p, TK_Comma));
        range = rangeFromTokens(start, p->tok);
        expectToken(p, TK_Rparen);
    } else { // Result list cannot have labels
        ArrayPush(results, parseType(p));
        while (matchToken(p, TK_Comma)) {
            ArrayPush(results, parseType(p));
        }
        Expr *last = results[ArrayLen(results) - 1];
        range = rangeFromTokenToEndOfNode(start, last);
    }
    return NewExprTypeFunction(p->package, range, params, results);
}

DynamicArray(Expr *) parseExprList(Parser *p, b32 noCompoundLiteral) {
    DynamicArray(Expr *) exprs = NULL;
    ArrayPush(exprs, parseExpr(p, noCompoundLiteral));
    while (matchToken(p, TK_Comma)) {
        ArrayPush(exprs, parseExpr(p, noCompoundLiteral));
    }
    return exprs;
}

Stmt *parseBlock(Parser *p) {
    Token start = p->tok;
    expectToken(p, TK_Lbrace);
    DynamicArray(Stmt *) stmts = NULL;
    while (isNotRbraceOrEOF(p)) {
        ArrayPush(stmts, parseStmt(p));
    }

    SourceRange range = rangeFromTokens(start, p->tok);
    expectToken(p, TK_Rbrace);
    matchToken(p, TK_Terminator); // consume terminator if needed `if a {} else {}
    return NewStmtBlock(p->package, range, stmts);
}

// isIdentList being non NULL indicates that an ident list is permitted (for ... in)
Stmt *parseSimpleStmt(Parser *p, b32 noCompoundLiteral, b32 *isIdentList) {
    Package *pkg = p->package;
    Token start = p->tok;

    DynamicArray(Expr *) exprs = parseExprList(p, noCompoundLiteral);
    switch (p->tok.kind) {
        case TK_Assign: {
            nextToken();
            DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
            Expr *last = rhs[ArrayLen(rhs) - 1];
            SourceRange range = rangeFromTokenToEndOfNode(start, last);
            return NewStmtAssign(pkg, range, exprs, rhs);
        }

        case TK_AddAssign: case TK_SubAssign: case TK_MulAssign: case TK_DivAssign:
        case TK_RemAssign: case TK_AndAssign: case TK_OrAssign:
        case TK_XorAssign: case TK_ShlAssign: case TK_ShrAssign: {
            Token op = p->tok;
            op.kind = TokenAssignOffset(op.kind);
            nextToken();
            DynamicArray(Expr *) rhs = parseExprList(p, noCompoundLiteral);
            if (ArrayLen(rhs) > 1) {
                ReportError(p->package, SyntaxError, rangeFromPosition(op.pos), "Only regular assignment may have multiple left or right values");
            }
            SourceRange range = rangeFromNodes(exprs[0], rhs[0]);
            rhs[0] = NewExprBinary(pkg, range, op, exprs[0], rhs[0]);
            return NewStmtAssign(pkg, range, exprs, rhs);
        }

        case TK_Colon: {
            Token colon = p->tok;
            nextToken();
            if (ArrayLen(exprs) == 1 && exprs[0]->kind == ExprKindIdent && (matchToken(p, TK_Terminator) || isTokenEof(p))) {
                SourceRange range = rangeFromTokens(start, colon);
                return NewStmtLabel(pkg, range, exprs[0]->Ident.name);
            }

            Expr *start = exprs[0];

            size_t numExprs = ArrayLen(exprs);
            for (size_t i = 0; i < numExprs; i++) {
                if (exprs[i]->kind != ExprKindIdent) {
                    ReportError(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
                }
            }

            DynamicArray(Expr *) rhs = NULL;

            if (matchToken(p, TK_Assign)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclVariable(pkg, range, exprs, NULL, rhs);
            } 
            
            if (matchToken(p, TK_Colon)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclConstant(pkg, range, exprs, NULL, rhs);
            }

            Expr *type = parseExpr(p, noCompoundLiteral);
            if (matchToken(p, TK_Assign)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclVariable(pkg, range, exprs, type, rhs);
            } 
            
            if (matchToken(p, TK_Colon)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclConstant(pkg, range, exprs, type, rhs);
            }

            SourceRange range = rangeFromNodes(start, type);
            return (Stmt *) NewDeclVariable(pkg, range, exprs, type, NULL);
        }

        default:
            break;
    }

    if (ArrayLen(exprs) > 1 && isIdentList) {
        *isIdentList = true;

        // Check that all the expresions are identifiers.
        size_t numExprs = ArrayLen(exprs);
        for (size_t i = 0; i < numExprs; i++) {
            if (exprs[i]->kind != ExprKindIdent) {
                ReportError(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
            }
        }
        // FIXME: Make a Stmt_IdentList
        return (Stmt *) exprs;
    } else if (ArrayLen(exprs) > 1) {
        ReportError(p->package, SyntaxError, exprs[1]->pos, "Expected single expression");
    }

    // FIXME: What is going on here?
    return (Stmt *) exprs[0];
}

Stmt *parseStmtFor(Parser *p, Package *pkg) {
    ASSERT(p->tok.val.s == Keyword_for);
    Token start = p->tok;
    nextToken();
    if (isToken(p, TK_Lbrace)) {
        Stmt *body = parseBlock(p);
        SourceRange range = rangeFromTokenToEndOffset(start, body->pos.endOffset);
        return NewStmtFor(pkg, range, NULL, NULL, NULL, body);
    }
    Stmt *s1, *s2, *s3;
    s1 = s2 = s3 = NULL;
    if (!isToken(p, TK_Lbrace) && !isToken(p, TK_Terminator)) {
        b32 isIdentList = false;
        s2 = parseSimpleStmt(p, true, &isIdentList);
        if (isIdentList) {
            DynamicArray(Expr *) idents = (DynamicArray(Expr *)) s2;
            Expr *aggregate = NULL;
            if (isTokenIdent(p, internIn)) {
                nextToken();
                Expr *valueName = NULL;
                Expr *indexName = NULL;
                if (ArrayLen(idents) > 0) valueName = idents[0];
                if (ArrayLen(idents) > 1) indexName = idents[1];
                if (ArrayLen(idents) > 2) {
                    ReportError(p->package, SyntaxError, idents[2]->pos, "For in iteration must provide at most 2 names to assign (value, index)");
                }
                aggregate = parseExpr(p, true);
                Stmt *body = parseBlock(p);
                SourceRange range = rangeFromTokenToEndOffset(start, body->pos.endOffset);
                return NewStmtForIn(pkg, range, valueName, indexName, aggregate, body);
            } else {
                ReportError(p->package, SyntaxError, rangeFromPosition(p->tok.pos), "Expected single expression or 'in' for iterator");
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
    if (s2 && !isExpr(s2)) {
        ReportError(p->package, SyntaxError, s2->pos, "Expected expression, got '%s'", AstDescriptions[s2->kind]);
    }

    Stmt *body = parseBlock(p);
    SourceRange range = rangeFromTokenToEndOffset(start, body->pos.endOffset);
    return NewStmtFor(pkg, range, s1, (Expr *) s2, s3, body);
}

Stmt *parseStmtSwitch(Parser *p, Package *pkg) {
    ASSERT(p->tok.val.s == Keyword_switch);
    Token start = p->tok;
    nextToken();

    Expr *match = NULL;
    if (!isToken(p, TK_Lbrace)) match = parseExpr(p, true);
    expectToken(p, TK_Lbrace);
    DynamicArray(Stmt *) cases = NULL;
    for (;;) {
        if (!matchKeyword(p, Keyword_case)) break;

        Token caseStart = p->tok;
        Token bodyStart = p->tok;
        DynamicArray(Expr *) exprs = NULL;
        if (!matchToken(p, TK_Colon)) {
            exprs = parseExprList(p, true);
            bodyStart = p->tok;
            expectToken(p, TK_Colon);
        }

        DynamicArray(Stmt *) stmts = NULL;
        while (!(isKeyword(p, Keyword_case) || isToken(p, TK_Rbrace) || isTokenEof(p))) {
            Stmt *stmt = parseStmt(p);
            ArrayPush(stmts, stmt);
        }

        SourceRange range;
        if (stmts) {
            range = rangeFromTokenToEndOfNode(bodyStart, stmts[ArrayLen(stmts) - 1]);
        } else {
            range = rangeFromTokenToEndOffset(bodyStart, p->tok.pos.offset); // from the ':' to w/e is next
        }
        Stmt *body = NewStmtBlock(p->package, range, stmts);
        range = rangeFromTokenToEndOfNode(caseStart, body);
        Stmt *scase = NewStmtSwitchCase(pkg, range, exprs, body);
        ArrayPush(cases, scase);
    }

    SourceRange range = rangeFromTokens(start, p->tok);
    expectToken(p, TK_Rbrace);

    return NewStmtSwitch(pkg, range, match, cases);
}

void parsePrefixDirectives(Parser *p) {
    while (isPrefixOrLoneDirective(p) && !isTokenEof(p)) {
        if (p->tok.val.ident == internCallConv) {
            nextToken();
            const char *val = p->tok.val.s;
            if (expectToken(p, TK_String)) {
                p->callingConvention = val;
            }
        } else if (p->tok.val.ident == internLinkPrefix) {
            nextToken();
            const char *val = p->tok.val.s;
            if (expectToken(p, TK_String)) {
                p->linkPrefix = val;
            }
        } else {
            UNIMPLEMENTED();
        }

        matchToken(p, TK_Terminator);
    }
}

typedef struct SuffixDirectives SuffixDirectives;
struct SuffixDirectives {
    const char *linkname;
    SourceRange pos;
};

SuffixDirectives parseSuffixDirectives(Parser *p) {
    SuffixDirectives val = {0};
    Token start = p->tok;
    Token end = start;
    while (isSuffixDirective(p) && !isTokenEof(p)) {
        end = p->tok;
        if (p->tok.val.ident == internLinkName) {
            nextToken();
            const char *name = p->tok.val.s;
            if (val.linkname) {
                SourceRange range = rangeFromTokens(start, end);
                ReportError(p->package, TODOError, range, "Multiple linknames provided for declaration");
            }
            if (expectToken(p, TK_String)) {
                val.linkname = name;
            }
        } else {
            UNIMPLEMENTED();
        }
    }
    val.pos = rangeFromTokens(start, end);
    return val;
}

Decl *parseForeignDecl(Parser *p, Token start, Expr *library) {
    const char *name = parseIdent(p);

    bool isConstant = false;
    expectToken(p, TK_Colon);
    if (matchToken(p, TK_Colon)) {
        isConstant = true;
    }
    Expr *type = parseType(p);

    SuffixDirectives suffixDirectives = parseSuffixDirectives(p);

    const char *linkname;
    if (suffixDirectives.linkname) {
        linkname = suffixDirectives.linkname;
    } else if (p->linkPrefix) {
        size_t prefixLen = strlen(p->linkPrefix);
        size_t nameLen = strlen(name);
        char *temp = Alloc(DefaultAllocator, prefixLen + nameLen + 1);
        temp = strncpy(temp, p->linkPrefix, prefixLen);
        temp = strncpy(temp + prefixLen, name, nameLen + 1);
        linkname = StrInternRange(temp, temp + prefixLen + nameLen);
        Free(DefaultAllocator, temp); // TODO: Some sort of scratch allocator on the package?
    } else {
        linkname = name;
    }

    SourceRange range = rangeFromTokenToEndOffset(start, suffixDirectives.pos.endOffset);
    return NewDeclForeign(p->package, range, library, isConstant, name, type, linkname, p->callingConvention);
}

Decl *parseForeignDeclBlock(Parser *p, Token start, Expr *library) {

    DynamicArray(char) tempStringBuffer = NULL;
    DynamicArray(Decl_ForeignBlockMember) members = NULL;

    while (!isToken(p, TK_Rbrace) && !isTokenEof(p)) {

        Token start = p->tok;
        const char *name = parseIdent(p);

        bool isConstant = false;
        expectToken(p, TK_Colon);
        if (matchToken(p, TK_Colon)) {
            isConstant = true;
        }
        Expr *type = parseType(p);

        // FIXME: @position If there is no suffix directive then the end used from this is the start of the next token
        SuffixDirectives suffixDirectives = parseSuffixDirectives(p);

        const char *linkname;
        if (suffixDirectives.linkname) {
            linkname = suffixDirectives.linkname;
        } else  if (p->linkPrefix) {
            size_t prefixLen = strlen(p->linkPrefix);
            size_t nameLen = strlen(name);
            tempStringBuffer = ArrayFit(tempStringBuffer, prefixLen + nameLen + 1);
            strncpy(tempStringBuffer, p->linkPrefix, prefixLen);
            strncpy(tempStringBuffer + prefixLen, name, nameLen + 1);
            linkname = StrInternRange(tempStringBuffer, tempStringBuffer + prefixLen + nameLen);
        } else {
            linkname = name;
        }

        expectTerminator(p);

        SourceRange range = rangeFromTokenToEndOffset(start, suffixDirectives.pos.endOffset);
        Decl_ForeignBlockMember member = {range, name, isConstant, type, linkname};
        ArrayPush(members, member);
    }

    SourceRange range = rangeFromTokens(start, p->tok);
    expectToken(p, TK_Rbrace);

    return NewDeclForeignBlock(p->package, range, library, p->callingConvention, members);
}

Stmt *parseStmt(Parser *p) {
    Package *pkg = p->package;
    Token start = p->tok;

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
            return parseBlock(p);
        }

        case TK_Directive: {
            if (isDirective(p, internFile) || isDirective(p, internLine) ||
                isDirective(p, internLocation) || isDirective(p, internFunction) ||
                isDirective(p, internCVargs)) goto exprStart;

            if (!isPrefixOrLoneDirective(p)) {
                // FIXME: Get the range for the directive
                ReportError(pkg, TODOError, rangeFromPosition(start.pos), "Directive #%s cannot be used lone or as a prefix", p->tok.val.ident);
                nextToken();
                return NULL;
            }

            if (isDirective(p, internForeign)) {
                // #foreign glfw #callconv "c" #linkprefix "glfw"
                nextToken();
                Expr *library = parseExpr(p, false);
                matchToken(p, TK_Terminator); // FIXME: Allow only @newlines_only

                // This will update the parser state, setting callingConvention and linkPrefix
                parsePrefixDirectives(p);

                Decl *decl;
                if (matchToken(p, TK_Lbrace)) {

                    decl = parseForeignDeclBlock(p, start, library);
                } else {
                    if (p->linkPrefix) {
                        // FIXME: Position should be the linkPrefix position
                        ReportError(pkg, TODOError, rangeFromPosition(start.pos), "Use of linkprefix directive is only valid on foreign blocks");
                    }

                    decl = parseForeignDecl(p, start, library);
                }

                // clear the prefixDirectives
                p->callingConvention = NULL;
                p->linkPrefix = NULL;
                expectTerminator(p);
                return (Stmt *) decl;
            }

            if (isDirective(p, internImport)) {
                nextToken();
                Expr *path = parseExpr(p, false);
                SourceRange range = rangeFromTokenToEndOfNode(start, path);
                const char *alias = NULL;
                if (isToken(p, TK_Ident)) {
                    range = rangeFromTokens(start, p->tok);
                    alias = parseIdent(p);
                }
                expectTerminator(p);
                return (Stmt *) NewDeclImport(pkg, range, path, alias);
            }

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
                SourceRange range = rangeFromTokenToEndOfNode(start, fail ? fail : pass);
                return NewStmtIf(pkg, range, cond, pass, fail);
            }
            if (matchKeyword(p, Keyword_defer)) {
                Stmt *stmt = parseStmt(p);
                SourceRange range = rangeFromTokenToEndOfNode(start, stmt);
                return NewStmtDefer(pkg, range, stmt);
            }
            if (isKeywordBranch(p, p->tok.val.s)) {
                const char *keyword = p->tok.val.s;
                nextToken();
                SourceRange range = rangeFromTokens(start, start);
                if (matchToken(p, TK_Terminator)) {
                    return NewStmtGoto(pkg, range, keyword, NULL);
                }
                if (keyword == Keyword_fallthrough) {
                    expectTerminator(p);
                    return NewStmtGoto(pkg, range, keyword, NULL);
                }
                if ((keyword == Keyword_break || keyword == Keyword_continue) &&
                    (matchToken(p, TK_Terminator) || isToken(p, TK_Rbrace) || isToken(p, TK_Eof))) {
                    return NewStmtGoto(pkg, range, keyword, NULL);
                }
                Expr *target = parseExpr(p, true);
                range = rangeFromTokenToEndOfNode(start, target);
                return NewStmtGoto(pkg, range, keyword, target);
            }
            if (isKeyword(p, Keyword_for)) {
                return parseStmtFor(p, pkg);
            }
            if (matchKeyword(p, Keyword_return)) {
                DynamicArray(Expr *) exprs = NULL;
                if (!isToken(p, TK_Terminator) && !isToken(p, TK_Eof) && !isToken(p, TK_Rbrace)) {
                    exprs = parseExprList(p, false);
                }
                if (p->tok.kind != TK_Rbrace) expectTerminator(p);
                SourceRange range = exprs ?
                    rangeFromTokenToEndOfNode(start, exprs[ArrayLen(exprs) - 1]) : rangeFromTokens(start, start);
                return NewStmtReturn(pkg, range, exprs);
            }
            if (isKeyword(p, Keyword_switch)) {
                return parseStmtSwitch(p, pkg);
            }
            if (matchKeyword(p, Keyword_using)) UNIMPLEMENTED();

            // Maybe the expression is the start of a stmt? Such as cast or autocast?
            return (Stmt *) parseExpr(p, false);
        }

        default:
            UNIMPLEMENTED();
            return NULL;
    }

    SourceRange range = rangeFromTokens(start, start);
    return NewStmtInvalid(pkg, range);
}

DynamicArray(Stmt *) parseStmts(Parser *p) {
    DynamicArray(Stmt *) stmts = NULL;
    while (!isToken(p, TK_Rbrace) && !isTokenEof(p)) {
        ArrayPush(stmts, parseStmt(p));
    }
    return stmts;
}

DynamicArray(Stmt *) parseStmtsUntilEof(Parser *p) {
    DynamicArray(Stmt *) stmts = p->package->stmts;

    while (!isTokenEof(p)) {
        ArrayPush(stmts, parseStmt(p));
    }
    return stmts;
}

void parseAllStmts(Package *pkg, const char *code) {
    Lexer lexer = MakeLexer(code, pkg);
    Token tok = NextToken(&lexer);
    Parser parser = {lexer, .tok = tok, pkg};

    while (!isTokenEof(&parser)) {
        Stmt *stmt = parseStmt(&parser);
        ArrayPush(pkg->stmts, stmt);
    }
}

void declareDecl(Package *pkg, Decl *decl) {
    Symbol *symbol;
    switch (decl->kind) {
        case DeclKindConstant: {
            decl->owningScope = pkg->scope;
            // Declare all names despite only supporting single declaration for Constants
            size_t numNames = ArrayLen(decl->Constant.names);
            for (size_t i = 0; i < numNames; i++) {
                declareSymbol(pkg, pkg->scope, decl->Constant.names[i]->Ident.name, &symbol, decl);
                symbol->kind = SymbolKindConstant;
                symbol->state = SymbolState_Unresolved;
                symbol->flags |= SymbolFlag_Global;
                symbol->decl = decl;
            }
            break;
        }
        case DeclKindVariable: {
            decl->owningScope = pkg->scope;
            size_t numNames = ArrayLen(decl->Variable.names);
            for (size_t i = 0; i < numNames; i++) {
                declareSymbol(pkg, pkg->scope, decl->Variable.names[i]->Ident.name, &symbol, decl);
                symbol->kind = SymbolKindVariable;
                symbol->state = SymbolState_Unresolved;
                symbol->flags |= SymbolFlag_Global;
                symbol->decl = decl;
            }
            break;
        }
        case DeclKindForeign: {
            decl->owningScope = pkg->scope;
            declareSymbol(pkg, pkg->scope, decl->Foreign.name, &symbol, decl);
            symbol->kind = decl->Foreign.isConstant ? SymbolKindConstant : SymbolKindVariable;
            symbol->state = SymbolState_Unresolved;
            symbol->flags |= SymbolFlag_Global;
            symbol->decl = decl;
            break;
        }
        case DeclKindForeignBlock: {
            decl->owningScope = pkg->scope;
            Decl_ForeignBlock block = decl->ForeignBlock;
            size_t len = ArrayLen(block.members);
            for (size_t i = 0; i < len; i++) {
                Decl_ForeignBlockMember *it = &block.members[i];
                declareSymbol(pkg, pkg->scope, it->name, &symbol, decl);
                symbol->kind = it->isConstant ? SymbolKindConstant : SymbolKindVariable;
                symbol->state = SymbolState_Unresolved;
                symbol->flags |= SymbolFlag_Global;
                symbol->decl = decl;
                it->symbol = symbol;
                symbol = NULL;
            }
            break;
        }
        case DeclKindImport: {
            decl->owningScope = pkg->scope;
            // TODO: To properly support out of order declarations things we will need to re-evaluate how we
            //  evaluate imports. Because the code will potentially have to run through the entire compiler pipeline
            //  before a constant string may be generated. Only then can we 'infer' the name from the import.
            // For now we will just use the alias
            //  -vdka August 2018
            declareSymbol(pkg, pkg->scope, decl->Import.alias, &symbol, decl);
            symbol->type = FileType;
            symbol->kind = SymbolKindImport;
            symbol->state = SymbolState_Resolved;
            symbol->flags |= SymbolFlag_Global;
            symbol->decl = decl;
            decl->Import.symbol = symbol;
            decl->Import.symbol->backendUserdata = import_package(decl->Import.path->LitString.val, pkg);
            break;
        }
        default:
            break;
    }
}

Symbol *declare_package_symbol(Package *pkg, const char *name, SymbolKind kind, SymbolState state,
                               Decl *decl)
{
    Symbol *symbol;
    decl->owningScope = pkg->scope;
    declareSymbol(pkg, pkg->scope, name, &symbol, decl);
    symbol->kind = kind;
    symbol->state = state;
    symbol->flags = SymbolFlag_Global;
    symbol->decl = decl;
    return symbol;
}

void declare_package_decls(Package *pkg) {
    u64 num_stmts = ArrayLen(pkg->stmts);
    for (u64 i = 0; i < num_stmts; i++) {
        Decl *decl = (Decl *) pkg->stmts[i];
        decl->owningScope = pkg->scope;
        switch (decl->kind) {
        case DeclKindConstant: {
            size_t numNames = ArrayLen(decl->Constant.names);
            for (size_t i = 0; i < numNames; i++) {
                const char *name = decl->Constant.names[i]->Ident.name;
                declare_package_symbol(pkg, name, SymbolKindConstant, SymbolState_Unresolved,
                                       decl);
            }
            break;
        }
        case DeclKindVariable: {
            size_t numNames = ArrayLen(decl->Constant.names);
            for (size_t i = 0; i < numNames; i++) {
                const char *name = decl->Variable.names[i]->Ident.name;
                declare_package_symbol(pkg, name, SymbolKindVariable, SymbolState_Unresolved,
                                       decl);
            }
            break;
        }
        case DeclKindForeign: {
            SymbolKind kind = decl->Foreign.isConstant ? SymbolKindConstant : SymbolKindVariable;
            declare_package_symbol(pkg, decl->Foreign.name, kind, SymbolState_Unresolved, decl);
            break;
        }
        case DeclKindForeignBlock: {
            Decl_ForeignBlock block = decl->ForeignBlock;
            size_t len = ArrayLen(block.members);
            for (size_t i = 0; i < len; i++) {
                Decl_ForeignBlockMember *it = &block.members[i];
                SymbolKind kind = it->isConstant ? SymbolKindConstant : SymbolKindVariable;
                it->symbol = declare_package_symbol(pkg, it->name, kind, SymbolState_Unresolved, decl);
            }
            break;
        }
        case DeclKindImport: {
            Symbol *symbol = declare_package_symbol(pkg, decl->Import.alias,
                                                    SymbolKindImport, SymbolState_Resolved, decl);
            symbol->type = FileType;
            decl->Import.symbol = symbol;
            const char *path = decl->Import.path->LitString.val;
            decl->Import.symbol->backendUserdata = import_package(path, pkg);
            break;
        }
        }
    }
}

void queue_checking(Package *pkg) {
    // All of the files in the package have been parsed. We are now ready for checking.
    DynamicArray(CheckerInfo) checkerInfo = NULL;
    ArrayFit(checkerInfo, pkg->astIdCount + 1);
    memset(checkerInfo, 0, sizeof(CheckerInfo) * (pkg->astIdCount + 1));
    pkg->checkerInfo = checkerInfo;

    size_t len = ArrayLen(pkg->stmts);
    for (int i = 0; i < len; i++) {
        CheckerWork *work = ArenaAlloc(&compiler.checking_queue.arena, sizeof(CheckerWork));
        work->package = pkg;
        work->stmt = pkg->stmts[i];
        QueuePushBack(&compiler.checking_queue, work);
    }
}

u64 source_memory_usage = 0;

bool parse_package(Package *pkg) {
    read_package_source_files(pkg);
    for (u64 i = 0; i < pkg->numSources; i++) {
        pkg->current_source = &pkg->sources[i];
        parseAllStmts(pkg, pkg->current_source->code);
    }
    if (HasErrors(pkg)) return false;
    declare_package_decls(pkg);
    queue_checking(pkg);
    return !HasErrors(pkg);
}

#if TEST
bool parse_test_package(Package *pkg) {
    ASSERT(pkg->current_source);
    parseAllStmts(pkg, pkg->current_source->code);
    if (HasErrors(pkg)) return false;
    declare_package_decls(pkg);
    queue_checking(pkg);
    return !HasErrors(pkg);
}
#endif

#undef NextToken

#if TEST
#include "tests/parser.c"
#endif
