
// FIXME: Replace when we get diagnostics
void PARSER_ERROR(Position pos, const char *msg, ...) {
   va_list args;
   va_start(args, msg);
   printf("ERROR: %s:%u:%u ", pos.name, pos.line, pos.column);
   vprintf(msg, args);
   va_end(args);
   printf("\n");
}

typedef struct Parser Parser;
struct Parser {
    Lexer lexer;
    Position prevStart;
    Position prevEnd;
    Token tok;
    Package package;
};

bool isNotRbraceOrEOF(Parser *p) {
    if (p->tok.kind == TK_Rbrace) return false;
    else if (p->tok.kind == TK_Eof) return false;
    return true;
}

Error SyntaxErrorToken(TokenKind kind, Token actual) {
    char *msg = errorBuffPrintf("Expected '%s' but got '%s'", DescribeTokenKind(kind), DescribeTokenKind(actual.kind));
    return (Error) {
        .code = SyntaxError,
        .pos = actual.pos,
        .message = msg,
    };
}

Error SyntaxErrorMessage(const char *msg, Position pos, ...) {
    return (Error) {
        .code = SyntaxError,
        .pos = pos,
        .message = msg,
    };
}

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

b32 isKeyword(Parser *p, const char *ident) {
    return isToken(p, TK_Keyword) && p->tok.val.s == ident;
}

b32 matchKeyword(Parser *p, const char *ident) {
    if (isKeyword(p, ident)) {
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
    if (isToken(p, kind)) {
        nextToken();
        return true;
    }

    ReportError(SyntaxError, p->tok.pos, "Expected token %s, got %s", DescribeTokenKind(kind), DescribeToken(p->tok));
    return false;
}

const char *parseIdent(Parser *p) {
    const char *ident = p->tok.val.ident;
    expectToken(p, TK_Ident);
    return ident;
}

Expr *parseType(Parser *p);
Expr *parseExpr(Parser *p);
Expr *parseFunctionType(Parser *p);
Expr_KeyValue *parseExprCompoundField(Parser *p);
Stmt_Block *parseBlock(Parser *p);

Stmt_Block *parseBlock(Parser *p) {
    UNIMPLEMENTED();
    return NULL;
}

Expr *parseExprAtom(Parser *p) {
   switch (p->tok.kind) {
       case TK_Ident: {
           Expr *e = NewExprIdent(&p->package, p->tok.pos, p->tok.val.ident);
           nextToken();
           if (p->tok.kind == TK_Dot) {  // package.Member
               nextToken();
               const char *ident = parseIdent(p);
               return NewExprSelector(&p->package, e, ident, p->prevEnd);
           }
           return e;
       }

       case TK_Int: {
           Expr *e = NewExprLitInt(&p->package, p->tok.pos, p->tok.val.i);
           nextToken();
           return e;
       }

       case TK_Float: {
           Expr *e = NewExprLitFloat(&p->package, p->tok.pos, p->tok.val.f);
           nextToken();
           return e;
       }

       case TK_String: {
           Expr *e = NewExprLitString(&p->package, p->tok.pos, p->tok.val.s);
           nextToken();
           return e;
       }

       case TK_Lparen: {
           Position start = p->tok.pos;
           nextToken();
           Expr *expr = parseExpr(p);
           expectToken(p, TK_Rparen);
           return NewExprParen(&p->package, expr, start, p->prevStart);
       }

       case TK_Lbrack: {
           Position start = p->tok.pos;
           nextToken();
           if (matchToken(p, TK_Rbrack)) {
               Expr *type = parseType(p);
               return NewExprTypeSlice(&p->package, start, type);
           }
           Expr *length = NULL;
           if (!matchToken(p, TK_Ellipsis)) {
               length = parseExpr(p);
           }
           expectToken(p, TK_Rbrack);
           Expr *type = parseType(p);
           return NewExprTypeArray(&p->package, start, length, type);
       }

       case TK_Dollar: {
           Position start = p->tok.pos;
           nextToken();
           const char *name = parseIdent(p);
           return NewExprTypePolymorphic(&p->package, start, name);
       }

       caseEllipsis:
       case TK_Ellipsis: {
           Position start = p->tok.pos;
           nextToken();
           return NewExprTypeVariadic(&p->package, start, parseType(p), 0);
       }

       case TK_Mul: {
           Position start = p->tok.pos;
           nextToken();
           Expr *type = parseType(p);
           return NewExprTypePointer(&p->package, start, type);
       }

       case TK_Directive: {
           if (p->tok.val.ident == internLocation || p->tok.val.ident == internFile || p->tok.val.ident == internLine || p->tok.val.ident == internFunction) {
               Expr *expr = NewExprLocationDirective(&p->package, p->tok.pos, p->tok.val.ident);
               nextToken();
               return expr;
           } else if (p->tok.val.ident == internCVargs) {
               goto caseEllipsis;
           }
           ReportError(SyntaxError, p->tok.pos, "Unexpected directive '%s'", p->tok.val.ident);
           break;
       }

       case TK_Keyword: {
           if (p->tok.val.ident == Keyword_fn) {
               return parseFunctionType(p);
           } else if (p->tok.val.ident == Keyword_nil) {
               Expr *expr = NewExprLitNil(&p->package, p->tok.pos);
               nextToken();
               return expr;
           } else if (p->tok.val.ident == Keyword_struct) {
               goto caseStruct;
           } else if (p->tok.val.ident == Keyword_union) {
               goto caseUnion;
           } else if (p->tok.val.ident == Keyword_enum) {
               goto caseEnum;
           }
           ReportError(SyntaxError, p->tok.pos, "Unexpected keyword '%s'", p->tok.val.ident);
           break;
       }

       caseStruct: {
           UNIMPLEMENTED();
           break;
       }

       caseUnion: {
           UNIMPLEMENTED();
           break;
       }

       caseEnum: {
           UNIMPLEMENTED();
           break;
       }

       default:
           ReportError(SyntaxError, p->tok.pos, "Unexpected token '%s'", DescribeToken(p->tok));
   }

    Position start = p->tok.pos;
    nextToken();
    return NewExprInvalid(&p->package, start, p->tok.pos);
}

Expr *parseExprPrimary(Parser *p) {
    Package *pkg = &p->package;
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
                    Expr *hi = parseExpr(p);
                    expectToken(p, TK_Rbrack);
                    x = NewExprSlice(pkg, x, NULL, hi, p->prevEnd);
                    continue;
                }
                Expr *index = parseExpr(p);
                if (matchToken(p, TK_Colon)) {
                    if (matchToken(p, TK_Rbrack)) {
                        x = NewExprSlice(pkg, x, index, NULL, p->prevEnd);
                        continue;
                    }
                    Expr *hi = parseExpr(p);
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
                    arg->value = parseExpr(p);
                    if (isToken(p, TK_Colon) && arg->value->kind == ExprKind_Ident) {
                        arg->key = arg->value;
                        arg->value = parseExpr(p);
                    }
                    ArrayPush(args, arg);
                    while (matchToken(p, TK_Comma)) {
                        if (isToken(p, TK_Rparen)) break; // Allow trailing comma in argument list

                        arg->start = p->tok.pos;
                        arg->value = parseExpr(p);
                        if (isToken(p, TK_Colon) && arg->value->kind == ExprKind_Ident) {
                            arg->key = arg->value;
                            arg->value = parseExpr(p);
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
                x = NewExprLitCompound(pkg, x, elements, p->prevEnd);
                continue;
            }

            default:
                return x;
        }
    }
}

Expr *parseExprUnary(Parser *p) {
    if (matchToken(p, TK_Mul)) {
        return NewExprTypePointer(&p->package, p->prevStart, parseType(p));
    }
    switch (p->tok.kind) {
        case TK_Add: case TK_Sub: case TK_Not: case TK_BNot: case TK_Xor: case TK_And: case TK_Lss: {
            TokenKind op = p->tok.kind;
            nextToken();
            return NewExprUnary(&p->package, p->prevStart, op, parseExprPrimary(p));
        }
        default:
            return parseExprPrimary(p);
    }
}

Expr *parseExprBinary(Parser *p, i32 prec1) {
    Expr *lhs = parseExprUnary(p);
    for (;;) {
        TokenKind op = p->tok.kind;
        Position pos = p->tok.pos;
        i32 precedence = PrecedenceForTokenKind[op];
        if (precedence < prec1) return lhs;
        nextToken();
        if (op == TK_Question) {
            Expr *pass = NULL;
            if (!isToken(p, TK_Colon)) {
                pass = parseExpr(p);
            }
            expectToken(p, TK_Colon);
            Expr *fail = parseExpr(p);
            return NewExprTernary(&p->package, lhs, pass, fail);
        }
        Expr *rhs = parseExprBinary(p, precedence + 1);
        lhs = NewExprBinary(&p->package, op, pos, lhs, rhs);
    }
}

Expr *parseExpr(Parser *p) {
    return parseExprBinary(p, 1);
}

Expr_KeyValue *parseExprCompoundField(Parser *p) {
    Expr_KeyValue *field = AllocAst(&p->package, sizeof(Expr_KeyValue));
    field->start = p->tok.pos;
    if (matchToken(p, TK_Lbrack)) {
        field->flags = KeyValueFlagIndex;
        field->key = parseExpr(p);
        expectToken(p, TK_Rbrack);
        expectToken(p, TK_Colon);
        field->value = parseExpr(p);
        return field;
    } else {
        field->value = parseExpr(p);
        if (matchToken(p, TK_Colon)) {
            field->key = field->value;
            if (field->key->kind != ExprKind_Ident) {
                ReportError(SyntaxError, p->prevStart, "Named initializer value must be an identifier or surrounded in '[]'");
            }
            field->value = parseExpr(p);
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
    Expr_KeyValue *kv = AllocAst(&p->package, sizeof(Expr_KeyValue));
    kv->start = p->tok.pos;
    if (isToken(p, TK_Ident)) {
        const char *name = parseIdent(p);
        expectToken(p, TK_Colon);
        kv->key = NewExprIdent(&p->package, start, name);
    }
    if (matchToken(p, TK_Ellipsis)) {
        if (nVarargs) *nVarargs += 1;
        kv->value = NewExprTypeVariadic(&p->package, p->prevStart, parseType(p), 0);
    } else {
        kv->value = parseType(p);
    }
    return kv;
}

Expr *parseFunctionType(Parser *p) {
    Position start = p->tok.pos;
    nextToken();
    expectToken(p, TK_Lparen);
    DynamicArray(Expr_KeyValue *) params = NULL;
    u32 nVarargs = 0;
    if (!isToken(p, TK_Rparen)) {
        ArrayPush(params, parseFunctionParam(p, &nVarargs));
        while (matchToken(p, TK_Comma)) {
            if (isToken(p, TK_Rparen)) break; // allow trailing ',' in parameter list

            Expr_KeyValue *param = parseFunctionParam(p, &nVarargs);
            ArrayPush(params, param);
        }
        if (nVarargs > 0) {
            b32 warnAboutPosition = nVarargs == 1 && params[ArrayLen(params) - 1]->value->kind != ExprKind_TypeVariadic;
            size_t len = ArrayLen(params);
            for (size_t i = 0; i < len; i++) {
                if (params[i]->value->kind == ExprKind_TypeVariadic) {
                    if (warnAboutPosition) {
                        ReportError(SyntaxError, params[i]->value->start, "Variadics must be the last parameter to a function");
                    } else {
                        ReportError(SyntaxError, params[i]->value->start, "Multiple variadic parameters in function type");
                    }
                }
            }
        }
    }
    expectToken(p, TK_Rparen);
    expectToken(p, TK_RetArrow);
    DynamicArray(Expr *) results = NULL;
    if (matchToken(p, TK_Lparen)) {
        nVarargs = 0;
        Expr_KeyValue *kv = parseFunctionParam(p, &nVarargs);
        ArrayPush(results, NewExpr(&p->package, ExprKind_KeyValue, kv->start));
        while (matchToken(p, TK_Comma)) {
            if (isToken(p, TK_Rparen)) break; // allow trailing ',' in return list

            kv = parseFunctionParam(p, &nVarargs);
            ArrayPush(results, NewExpr(&p->package, ExprKind_KeyValue, kv->start));
        }
        expectToken(p, TK_Rparen);
        if (nVarargs > 0) {
            size_t len = ArrayLen(results);
            for (size_t i = 0; i < len; i++) {
                if (results[i]->KeyValue.value->kind == ExprKind_TypeVariadic) {
                    ReportError(SyntaxError, results[i]->start, "Variadics are only valid in a functions parameters");
                }
            }
        }
    } else {
        ArrayPush(results, parseType(p));
        while (matchToken(p, TK_Comma)) {
            ArrayPush(results, parseType(p));
        }
    }
    return NewExprTypeFunction(&p->package, start, params, results);
}

#undef NextToken

#if TEST

Parser newTestParser(const char *stream) {
    Lexer lex = MakeLexer(stream, NULL);
    Package test = {0};
    Token tok = NextToken(&lex);
    Parser p = {lex, .tok = tok, test};
    return p;
}
#endif


#if TEST
void test_parseExprAtom() {
#define ASSERT_EXPR_KIND(expected) \
    expr = parseExprAtom(&p); \
    ASSERT(expr->kind == expected); \
    ASSERT(errorCollector.errorCount == 0)

    InitErrorBuffers();
    Expr *expr;

    Parser p = newTestParser("a 1 1.0 #line nil");
    ASSERT_EXPR_KIND(ExprKind_Ident);
    ASSERT_EXPR_KIND(ExprKind_LitInt);
    ASSERT_EXPR_KIND(ExprKind_LitFloat);
    ASSERT_EXPR_KIND(ExprKind_LocationDirective);
    ASSERT_EXPR_KIND(ExprKind_LitNil);

    p = newTestParser("fn () -> a []a [2]a *a ..a $a");
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
    ASSERT_EXPR_KIND(ExprKind_TypeSlice);
    ASSERT_EXPR_KIND(ExprKind_TypeArray);
    ASSERT_EXPR_KIND(ExprKind_TypePointer);
    ASSERT_EXPR_KIND(ExprKind_TypeVariadic);
    ASSERT_EXPR_KIND(ExprKind_TypePolymorphic);

#undef ASSERT_EXPR_KIND
}
#endif

#if TEST
void test_parseExprPrimary() {
#define ASSERT_EXPR_KIND(expected) \
    expr = parseExprPrimary(&p); \
    ASSERT(expr->kind == expected); \
    ASSERT(errorCollector.errorCount == 0)

    InitErrorBuffers();
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
expr = parseExprUnary(&p); \
ASSERT(expr->kind == expected); \
ASSERT(errorCollector.errorCount == 0)

    InitErrorBuffers();
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
expr = parseExprBinary(&p, 1); \
ASSERT(expr->kind == expected); \
ASSERT(errorCollector.errorCount == 0)

    InitErrorBuffers();
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
expr = parseExprBinary(&p, 1); \
ASSERT(expr->kind == expected); \
ASSERT(errorCollector.errorCount == 0)

    InitErrorBuffers();
    Expr *expr;

    Parser p = newTestParser("a ? b : c");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

    p = newTestParser("a ?: b");
    ASSERT_EXPR_KIND(ExprKind_Ternary);

#undef ASSERT_EXPR_KIND
}
#endif

