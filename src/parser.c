
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
    ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Expected token %s, got %s", DescribeTokenKind(kind), DescribeToken(p->tok));
    return false;
}

b32 expectTerminator(Parser *p) {
    if (matchToken(p, TK_Terminator) || isToken(p, TK_Rbrace) || isToken(p, TK_Eof)) return true;
    ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Expected terminator, got %s", DescribeToken(p->tok));
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

            DynamicArray(Expr_KeyValue *) elements = NULL;

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
            if (p->tok.val.ident == internLocation || p->tok.val.ident == internFile || p->tok.val.ident == internLine || p->tok.val.ident == internFunction) {
                SourceRange range = rangeFromTokens(start, start);
                Expr *expr = NewExprLocationDirective(pkg, range, p->tok.val.ident);
                nextToken();
                return expr;
            } else if (p->tok.val.ident == internCVargs) {
                goto caseEllipsis;
            }
            ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Unexpected directive '%s'", p->tok.val.ident);
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
            ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Unexpected keyword '%s'", p->tok.val.ident);
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
                    ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Enum values are established at compile time and declared using '::'");

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
            ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Unexpected token '%s'", DescribeToken(p->tok));
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
                DynamicArray(Expr_KeyValue *) args = NULL;
                if (!isToken(p, TK_Rparen)) {
                    Token start = p->tok;

                    Expr_KeyValue *arg = AllocAst(pkg, sizeof(Expr_KeyValue));
                    arg->value = parseExpr(p, noCompoundLiteral);
                    if (isToken(p, TK_Colon) && arg->value->kind == ExprKind_Ident) {
                        arg->key = arg->value;
                        arg->value = parseExpr(p, noCompoundLiteral);
                    }
                    arg->pos = rangeFromTokenToEndOfNode(start, arg->value);
                    ArrayPush(args, arg);
                    while (matchToken(p, TK_Comma)) {
                        if (isToken(p, TK_Rparen)) break; // Allow trailing comma in argument list
                        start = p->tok;

                        arg = AllocAst(pkg, sizeof(Expr_KeyValue));
                        arg->value = parseExpr(p, noCompoundLiteral);
                        if (isToken(p, TK_Colon) && arg->value->kind == ExprKind_Ident) {
                            arg->key = arg->value;
                            arg->value = parseExpr(p, noCompoundLiteral);
                        }
                        arg->pos = rangeFromTokenToEndOfNode(start, arg->value);
                        ArrayPush(args, arg);
                    }
                }
                SourceRange range = rangeFromTokens(start, p->tok);
                expectToken(p, TK_Rparen);
                x = NewExprCall(pkg, range, x, args);
                continue;
            }

            case TK_Lbrace: {
                if (x->kind == ExprKind_TypeFunction) {
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
                    Stmt_Block *block = AllocAst(p->package, sizeof(Stmt_Block));
                    block->pos = blockRange;
                    block->stmts = stmts;
                    x = NewExprLitFunction(pkg, functionRange, x, block, 0);
                    continue;
                }
                if (noCompoundLiteral) return x;
                nextToken();

                matchToken(p, TK_Terminator);

                DynamicArray(Expr_KeyValue *) elements = NULL;
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

Expr_KeyValue *parseExprCompoundField(Parser *p) {
    Token start = p->tok;
    Expr_KeyValue *field = AllocAst(p->package, sizeof(Expr_KeyValue));
    if (matchToken(p, TK_Lbrack)) {
        field->flags = KeyValueFlag_Index;
        field->key = parseExpr(p, false);
        expectToken(p, TK_Rbrack);
        expectToken(p, TK_Colon);
        field->value = parseExpr(p, false);
        field->pos = rangeFromTokenToEndOfNode(start, field->value);
        return field;
    } else {
        field->value = parseExpr(p, false);
        if (matchToken(p, TK_Colon)) {
            field->key = field->value;
            if (field->key->kind != ExprKind_Ident) {
                ReportErrorPosition(p->package, SyntaxError, p->prevStart, "Named initializer value must be an identifier or surrounded in '[]'");
            }
            field->value = parseExpr(p, false);
        }
        field->pos = rangeFromTokenToEndOfNode(start, field->value);
        return field;
    }
}

Expr *parseType(Parser *p) {
    // NOTE: Right now parseType passes right onto parseAtom but, if types can be returned from
    //   functions calls in the future (CTE) then we will want this.
    // It is also clearer for intent
    return parseExprAtom(p);
}

void parseFunctionParameters(u32 *nVarargs, b32 *namedParameters, Parser *p, DynamicArray(Expr_KeyValue *) *params) {
    expectToken(p, TK_Lparen);
    *nVarargs = 0;
    *namedParameters = false;
    do {
        if (isToken(p, TK_Rparen)) break; // allow trailing ',' in parameter list
        DynamicArray(Expr *) exprs = parseExprList(p, false);
        if (matchToken(p, TK_Colon)) {
            *namedParameters = true;
            Expr *type = parseType(p);
            For (exprs) {
                if (exprs[i]->kind != ExprKind_Ident) {
                    ReportErrorRange(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
                    continue;
                }
                Expr_KeyValue *kv = AllocAst(p->package, sizeof(Expr_KeyValue));
                kv->pos = exprs[i]->pos;
                kv->key = exprs[i];
                kv->value = type;
                ArrayPush(*params, kv);
            }
            if (type->kind == ExprKind_TypeVariadic) {
                *nVarargs += 1;
                if (*nVarargs == 2) {
                    ReportErrorRange(p->package, SyntaxError, type->pos, "Expected at most 1 Variadic as the final parameter");
                }
            }
        } else if (*nVarargs <= 1) {
            if (*namedParameters) {
                ReportErrorRange(p->package, SyntaxError, exprs[0]->pos, "Mixture of named and unnamed parameters is unsupported");
            }
            // The parameters are unnamed and the user may have entered a second variadic
            For (exprs) {
                if (exprs[i]->kind == ExprKind_TypeVariadic) {
                    *nVarargs += 1;
                    if (*nVarargs == 2) {
                        ReportErrorRange(p->package, SyntaxError, exprs[i]->pos, "Expected at most 1 Variadic as the final parameter");
                    }
                }
                Expr_KeyValue *kv = AllocAst(p->package, sizeof(Expr_KeyValue));
                kv->value = exprs[i];
                ArrayPush(*params, kv);
            }
        }
    } while (matchToken(p, TK_Comma));
    expectToken(p, TK_Rparen);
}

Expr *parseFunctionType(Parser *p) {
    Token start = p->tok;
    nextToken();
    DynamicArray(Expr_KeyValue *) params = NULL;
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
                For (exprs) {
                    if (exprs[i]->kind != ExprKind_Ident) {
                        ReportErrorRange(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
                        continue;
                    }
                    ArrayPush(results, type);
                }
            } else if (nVarargs <= 1) {
                if (namedParameters) {
                    ReportErrorRange(p->package, SyntaxError, exprs[0]->pos, "Mixture of named and unnamed parameters is unsupported");
                }
                For (exprs) {
                    if (exprs[i]->kind == ExprKind_TypeVariadic) {
                        nVarargs += 1;
                        if (nVarargs == 1) {
                            ReportErrorRange(p->package, SyntaxError, exprs[i]->pos, "Variadics are only valid in a functions parameters");
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

Stmt_Block *parseBlock(Parser *p) {
    Token start = p->tok;
    expectToken(p, TK_Lbrace);
    DynamicArray(Stmt *) stmts = NULL;
    while (isNotRbraceOrEOF(p)) {
        ArrayPush(stmts, parseStmt(p));
    }

    SourceRange range = rangeFromTokens(start, p->tok);
    expectToken(p, TK_Rbrace);
    matchToken(p, TK_Terminator); // consume terminator if needed `if a {} else {}
    Stmt_Block *block = AllocAst(p->package, sizeof(Stmt_Block));
    block->pos = range;
    block->stmts = stmts;
    return block;
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
                ReportErrorPosition(p->package, SyntaxError, op.pos, "Only regular assignment may have multiple left or right values");
            }
            SourceRange range = rangeFromNodes(exprs[0], rhs[0]);
            rhs[0] = NewExprBinary(pkg, range, op, exprs[0], rhs[0]);
            return NewStmtAssign(pkg, range, exprs, rhs);
        }

        case TK_Colon: {
            Token colon = p->tok;
            nextToken();
            if (ArrayLen(exprs) == 1 && exprs[0]->kind == ExprKind_Ident && (matchToken(p, TK_Terminator) || isTokenEof(p))) {
                SourceRange range = rangeFromTokens(start, colon);
                return NewStmtLabel(pkg, range, exprs[0]->Ident.name);
            }

            Expr *start = exprs[0];
            DynamicArray(Expr_Ident *) idents = NULL;
            ArrayFit(idents, ArrayLen(exprs));
            For (exprs) {
                if (exprs[i]->kind != ExprKind_Ident) {
                    ReportErrorRange(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
                }
                ArrayPush(idents, &exprs[i]->Ident);
            }
            ArrayFree(exprs);

            DynamicArray(Expr *) rhs = NULL;

            if (matchToken(p, TK_Assign)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclVariable(pkg, range, idents, NULL, rhs);
            } 
            
            if (matchToken(p, TK_Colon)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclConstant(pkg, range, idents, NULL, rhs);
            }

            Expr *type = parseExpr(p, noCompoundLiteral);
            if (matchToken(p, TK_Assign)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclVariable(pkg, range, idents, type, rhs);
            } 
            
            if (matchToken(p, TK_Colon)) {
                rhs = parseExprList(p, noCompoundLiteral);
                SourceRange range = rangeFromNodes(start, rhs[ArrayLen(rhs) - 1]);
                return (Stmt *) NewDeclConstant(pkg, range, idents, type, rhs);
            }

            SourceRange range = rangeFromNodes(start, type);
            return (Stmt *) NewDeclVariable(pkg, range, idents, type, NULL);
        }

        default:
            break;
    }

    if (ArrayLen(exprs) > 1 && isIdentList) {
        *isIdentList = true;
        DynamicArray(Expr_Ident *) idents = NULL;
        For (exprs) {
            if (exprs[i]->kind != ExprKind_Ident) {
                ReportErrorRange(p->package, SyntaxError, exprs[i]->pos, "Expected identifier");
            }
            ArrayPush(idents, &exprs[i]->Ident);
        }
        ArrayFree(exprs);
        return (Stmt *) idents;
    } else if (ArrayLen(exprs) > 1) {
        ReportErrorRange(p->package, SyntaxError, exprs[1]->pos, "Expected single expression");
    }

    return (Stmt *) exprs[0];
}

Stmt *parseStmtFor(Parser *p, Package *pkg) {
    ASSERT(p->tok.val.s == Keyword_for);
    Token start = p->tok;
    nextToken();
    if (isToken(p, TK_Lbrace)) {
        Stmt_Block *body = parseBlock(p);
        SourceRange range = rangeFromTokenToEndOffset(start, body->pos.endOffset);
        return NewStmtFor(pkg, range, NULL, NULL, NULL, body);
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
                    ReportErrorRange(p->package, SyntaxError, idents[2]->pos, "For in iteration must provide at most 2 names to assign (value, index)");
                }
                aggregate = parseExpr(p, true);
                Stmt_Block *body = parseBlock(p);
                SourceRange range = rangeFromTokenToEndOffset(start, body->pos.endOffset);
                return NewStmtForIn(pkg, range, valueName, indexName, aggregate, body);
            } else {
                ReportErrorPosition(p->package, SyntaxError, p->tok.pos, "Expected single expression or 'in' for iterator");
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
        ReportErrorRange(p->package, SyntaxError, s2->pos, "Expected expression, got '%s'", AstDescriptions[s2->kind]);
    }

    Stmt_Block *body = parseBlock(p);
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
        Stmt_Block *block = AllocAst(pkg, sizeof(Stmt_Block));
        if (stmts) {
            block->pos = rangeFromTokenToEndOfNode(bodyStart, stmts[ArrayLen(stmts) - 1]);
        } else {
            block->pos = rangeFromTokenToEndOffset(bodyStart, p->tok.pos.offset); // from the ':' to w/e is next
        }
        block->stmts = stmts;
        SourceRange range = rangeFromTokenToEndOffset(caseStart, block->pos.endOffset);
        Stmt *scase = NewStmtSwitchCase(pkg, range, exprs, block);
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
                ReportErrorRange(p->package, TODOError, range, "Multiple linknames provided for declaration");
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
            Stmt_Block *block = parseBlock(p); // FIXME: @extra_alloc
            return NewStmtBlock(pkg, block->pos, block->stmts);
        }

        case TK_Directive: {
            if (isDirective(p, internFile) || isDirective(p, internLine) ||
                isDirective(p, internLocation) || isDirective(p, internFunction) ||
                isDirective(p, internCVargs)) goto exprStart;

            if (!isPrefixOrLoneDirective(p)) {
                // FIXME: Get the range for the directive
                ReportErrorPosition(pkg, TODOError, start.pos, "Directive #%s cannot be used lone or as a prefix", p->tok.val.ident);
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
                        ReportErrorPosition(pkg, TODOError, start.pos, "Use of linkprefix directive is only valid on foreign blocks");
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
    DynamicArray(Stmt *) stmts = NULL;
    while (!isTokenEof(p)) {
        ArrayPush(stmts, parseStmt(p));
    }
    return stmts;
}

void parsePackageCode(Package *pkg, const char *code) {
    Lexer lexer = MakeLexer(code, pkg);
    Token tok = NextToken(&lexer);
    Parser parser = {lexer, .tok = tok, pkg};
    pkg->stmts = parseStmtsUntilEof(&parser);

    if (HasErrors(pkg)) {
        return;
    }

    for(size_t i = 0; i < ArrayLen(pkg->stmts); i++) {
        Decl *decl = (Decl *) pkg->stmts[i];
        Symbol *symbol;

        switch (decl->kind) {
            case StmtDeclKind_Constant: {
                decl->owningScope = pkg->scope;
                ForEach(decl->Constant.names, Expr_Ident *) {
                    // We iterate over, and declare all names of the constant despite only supporting a single name
                    declareSymbol(pkg, pkg->scope, it->name, &symbol, decl);
                    symbol->kind = SymbolKind_Constant;
                    symbol->state = SymbolState_Unresolved;
                    symbol->flags |= SymbolFlag_Global;
                    symbol->decl = decl;
                }
                break;
            }
            case StmtDeclKind_Variable: {
                decl->owningScope = pkg->scope;
                ForEach(decl->Variable.names, Expr_Ident *) {
                    declareSymbol(pkg, pkg->scope, it->name, &symbol, decl);
                    symbol->kind = SymbolKind_Variable;
                    symbol->state = SymbolState_Unresolved;
                    symbol->flags |= SymbolFlag_Global;
                    symbol->decl = decl;
                }
                break;
            }
            case StmtDeclKind_Foreign: {
                decl->owningScope = pkg->scope;
                declareSymbol(pkg, pkg->scope, decl->Foreign.name, &symbol, decl);
                symbol->kind = decl->Foreign.isConstant ? SymbolKind_Constant : SymbolKind_Variable;
                symbol->state = SymbolState_Unresolved;
                symbol->flags |= SymbolFlag_Global;
                symbol->decl = decl;
                break;
            }
            case StmtDeclKind_ForeignBlock: {
                decl->owningScope = pkg->scope;
                Decl_ForeignBlock block = decl->ForeignBlock;
                size_t len = ArrayLen(block.members);
                for (size_t i = 0; i < len; i++) {
                    Decl_ForeignBlockMember *it = &block.members[i];
                    declareSymbol(pkg, pkg->scope, it->name, &symbol, decl);
                    symbol->kind = it->isConstant ? SymbolKind_Constant : SymbolKind_Variable;
                    symbol->state = SymbolState_Unresolved;
                    symbol->flags |= SymbolFlag_Global;
                    symbol->decl = decl;
                    it->symbol = symbol;
                    symbol = NULL;
                }
                break;
            }

            case StmtDeclKind_Import: {
                decl->owningScope = pkg->scope;
                // TODO: To properly support out of order declarations things we will need to re-evaluate how we
                //  evaluate imports. Because the code will potentially have to run through the entire compiler pipeline
                //  before a constant string may be generated. Only then can we 'infer' the name from the import.
                // For now we will just use the alias
                //  -vdka August 2018
                declareSymbol(pkg, pkg->scope, decl->Import.alias, &symbol, decl);
                symbol->type = FileType;
                symbol->kind = SymbolKind_Import;
                symbol->state = SymbolState_Resolved;
                symbol->flags |= SymbolFlag_Global;
                symbol->decl = decl;
                decl->Import.symbol = symbol;
                break;
            }

            default:
                break;
        }
    }

    DynamicArray(CheckerInfo) checkerInfo = NULL;
    ArrayFit(checkerInfo, pkg->astIdCount+1);
    memset(checkerInfo, 0, sizeof(CheckerInfo) * (pkg->astIdCount + 1));
    pkg->checkerInfo = checkerInfo;

    size_t len = ArrayLen(pkg->stmts);
    for (int i = 0; i < len; i++) {
        CheckerWork *work = ArenaAlloc(&checkingQueue.arena, sizeof(CheckerWork));
        work->package = pkg;
        work->stmt = pkg->stmts[i];
        QueuePushBack(&checkingQueue, work);
    }
}

void parsePackage(Package *package) {
    const char *code = ReadEntireFile(package->fullpath);
    if (!code) {
        ReportErrorPosition(package, FatalError, (Position){ .name = package->path }, "Failed to read source file");
        return;
    }
    package->fileHandle = code;
    parsePackageCode(package, code);
}

#undef NextToken

#if TEST
Package parserTestPackage = {0};
Parser newTestParser(const char *stream) {
    Lexer lex = MakeLexer(stream, NULL);
    Token tok = NextToken(&lex);

    ArrayFree(parserTestPackage.diagnostics.errors);
    ArrayFree(parserTestPackage.stmts);
    ArrayFree(parserTestPackage.symbols);

    ArenaFree(&parserTestPackage.arena);
    ArenaFree(&parserTestPackage.diagnostics.arena);

    ArenaFree(&parsingQueue.arena);
    memset(&checkingQueue, 0, sizeof(Queue));
    ArenaFree(&checkingQueue.arena);
    memset(&checkingQueue, 0, sizeof(Queue));

    Parser p = {lex, .tok = tok, &parserTestPackage};
    return p;
}

void test_parseExprAtom() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

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
                      "fn (a, b: $T) -> (b, a: T)"
                      "fn (u8, u8) -> (u8, u8)");
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);

#undef ASSERT_EXPR_KIND
}

void test_parseExprPrimary() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprPrimary(&p, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

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
    ASSERT(expr->LitCompound.elements[0]->flags & KeyValueFlag_Index);

#undef ASSERT_EXPR_KIND
}

void test_parseExprUnary() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprUnary(&p, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("+5 -5 ~a ^a !a &a <a !!a");
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);
    ASSERT_EXPR_KIND(ExprKind_Unary);

#undef ASSERT_EXPR_KIND
}

void test_parseExprBinary() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprBinary(&p, 1, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a + b * c");
    ASSERT_EXPR_KIND(ExprKind_Binary);
    ASSERT(expr->Binary.op.kind == TK_Add);

    p = newTestParser("(a + b) * c");
    ASSERT_EXPR_KIND(ExprKind_Binary);
    ASSERT(expr->Binary.op.kind == TK_Mul);

#undef ASSERT_EXPR_KIND
}

void test_parseExprTernary() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprBinary(&p, 1, false); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;

    Parser p = newTestParser("a ? b : c");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

    p = newTestParser("a ?: b");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

#undef ASSERT_EXPR_KIND
}

void test_parseSimpleStmt() {
    REINIT_COMPILER();

#define ASSERT_STMT_KIND(expected) \
stmt = parseSimpleStmt(&p, false, NULL); \
ASSERT(stmt->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Stmt *stmt;

    Parser p = newTestParser("a := b");
    ASSERT_STMT_KIND(DeclKind_Variable);

#undef ASSERT_STMT_KIND
}

void test_parseStmt() {
    REINIT_COMPILER();

#define ASSERT_STMT_KIND(expected) \
stmt = parseStmt(&p); \
ASSERT(stmt->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Stmt *stmt;
    Parser p;
    
    p = newTestParser("a := b");
    ASSERT_STMT_KIND(DeclKind_Variable);

    p = newTestParser("a :: b");
    ASSERT_STMT_KIND(DeclKind_Constant);

    p = newTestParser("a:");
    ASSERT_STMT_KIND(StmtKind_Label);

    p = newTestParser("for a := 1; a < 2; a += 1 {}");
    ASSERT_STMT_KIND(StmtKind_For);

    p = newTestParser("for a, b in foo {}");
    ASSERT_STMT_KIND(StmtKind_ForIn);

    p = newTestParser("defer Free(mem)");
    ASSERT_STMT_KIND(StmtKind_Defer);

    p = newTestParser("break");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_break);
    ASSERT(stmt->Goto.target == NULL);

    p = newTestParser("break label");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_break);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("continue label");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_continue);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("fallthrough");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_fallthrough);
    ASSERT(stmt->Goto.target == NULL);

    p = newTestParser("goto label");
    ASSERT_STMT_KIND(StmtKind_Goto);
    ASSERT(stmt->Goto.keyword == Keyword_goto);
    ASSERT(stmt->Goto.target != NULL);

    p = newTestParser("for { break }");
    ASSERT_STMT_KIND(StmtKind_For);
    ASSERT(ArrayLen(stmt->For.body->stmts) == 1);

    p = newTestParser("if true {}");
    ASSERT_STMT_KIND(StmtKind_If);
    ASSERT(stmt->If.cond);
    ASSERT(stmt->If.cond->kind == ExprKind_Ident);
    ASSERT(stmt->If.pass);
    ASSERT(stmt->If.pass->kind == StmtKind_Block);
    ASSERT(!stmt->If.fail);

    p = newTestParser("if true {} else {}");
    ASSERT_STMT_KIND(StmtKind_If);
    ASSERT(stmt->If.cond);
    ASSERT(stmt->If.cond->kind == ExprKind_Ident);
    ASSERT(stmt->If.pass);
    ASSERT(stmt->If.pass->kind == StmtKind_Block);
    ASSERT(stmt->If.fail);
    ASSERT(stmt->If.fail->kind == StmtKind_Block);

    p = newTestParser("error:");
    ASSERT_STMT_KIND(StmtKind_Label);
    ASSERT(stmt->Label.name == StrIntern("error"));

    p = newTestParser("return");
    ASSERT_STMT_KIND(StmtKind_Return);
    ASSERT(!stmt->Return.exprs);

    p = newTestParser("return 1, 2, 3");
    ASSERT_STMT_KIND(StmtKind_Return);
    ASSERT(ArrayLen(stmt->Return.exprs) == 3);

    p = newTestParser("switch {"
                      "}");
    ASSERT_STMT_KIND(StmtKind_Switch);
    ASSERT(!stmt->Switch.match);
    ASSERT(!stmt->Switch.cases);

    p = newTestParser("switch foo {\n"
                      "case 1:\n"
                      "case:\n"
                      "}\n");
    ASSERT_STMT_KIND(StmtKind_Switch);
    ASSERT(stmt->Switch.match);
    ASSERT(ArrayLen(stmt->Switch.cases) == 2);

#undef ASSERT_STMT_KIND
}

void test_automaticTerminatorAfterFunction() {
    REINIT_COMPILER();

    Parser p = newTestParser("a :: fn() -> void {}" "\n"
                      "b :: fn() -> void {}" "\n");
    parseStmts(&p);
    ASSERT(!parserTestPackage.diagnostics.errors);
}

void test_parseStruct() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;
    Parser p;

    p = newTestParser("struct { a, b, c: u32 }");
    ASSERT_EXPR_KIND(ExprKind_TypeStruct);
}

void test_parseUnion() {
    REINIT_COMPILER();

#define ASSERT_EXPR_KIND(expected) \
expr = parseExprAtom(&p); \
ASSERT(expr->kind == expected); \
ASSERT(!parserTestPackage.diagnostics.errors)

    Expr *expr;
    Parser p;

    p = newTestParser("union { a, b, c: u32 }");
    ASSERT_EXPR_KIND(ExprKind_TypeUnion);
}
#endif
