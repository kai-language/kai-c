
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
    Position prevPos;
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
p->prevPos = p->tok.pos; \
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

Expr *parseExpr(Parser *p);
Expr *parseFunctionType(Parser *p, b32 allowFunctionLiteral);
Stmt_Block *parseBlock(Parser *p);

Expr *parseExpr(Parser *p) {
    UNIMPLEMENTED();
    return NULL;
}

Stmt_Block *parseBlock(Parser *p) {
    UNIMPLEMENTED();
    return NULL;
}

Expr *parseAtom(Parser *p) {
   switch (p->tok.kind) {
       case TK_Ident: {
           Expr *e = NewExprIdent(&p->package, p->tok.pos, p->tok.val.ident);
           nextToken();
           if (p->tok.kind == TK_Dot) {  // package.Member
               nextToken();
               Position endOfIdent = p->tok.pos;
               const char *ident = parseIdent(p);
               endOfIdent.column += StrInternLen(ident);
               return NewExprSelector(&p->package, parseAtom(p), ident, endOfIdent);
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

       case TK_Lparen: {
           Position start = p->tok.pos;
           nextToken();
           Expr *expr = parseExpr(p);
           expectToken(p, TK_Rparen);
           return NewExprParen(&p->package, expr, start, p->prevPos);
       }

       case TK_Directive: {
           if (p->tok.val.ident == internLocation || p->tok.val.ident == internFile || p->tok.val.ident == internLine || p->tok.val.ident == internFunction) {
               Expr *expr = NewExprLocationDirective(&p->package, p->tok.pos, p->tok.val.ident);
               nextToken();
               return expr;
           }
       }

       case TK_Keyword:
           if (p->tok.val.ident == Keyword_fn) {
               return parseFunctionType(p, /* allowFunctionLiteral:*/ true);
           } else if (p->tok.val.ident == Keyword_nil) {
               Expr *expr = NewExprLitNil(&p->package, p->tok.pos);
               nextToken();
               return expr;
           }
           ReportError(SyntaxError, p->tok.pos, "Unexpected keyword '%s'", p->tok.val.ident);
           break;

       default:
           ReportError(SyntaxError, p->tok.pos, "Unexpected token '%s'", DescribeToken(p->tok));
   }

    Position start = p->tok.pos;
    nextToken();
    return NewExprInvalid(&p->package, start, p->tok.pos);
}

Expr *parseType(Parser *p) {
    // NOTE: Right now parseType passes right onto parseAtom but, if types can be returned from
    //   functions calls in the future (CTE) then we will want this.
    // It is also clearer for intent
    return parseAtom(p);
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
        kv->value = NewExprTypeVariadic(&p->package, p->prevPos, parseType(p), 0);
    } else {
        kv->value = parseType(p);
    }
    return kv;
}

Expr *parseFunctionType(Parser *p, b32 allowFunctionLiteral) {
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

    Expr *type = NewExprTypeFunction(&p->package, start, params, results);
    if (allowFunctionLiteral && isToken(p, TK_Lparen)) {
        nextToken();

        Stmt_Block *body = parseBlock(p);
        return NewExprLitFunction(&p->package, start, type, body, 0);
    }
    return type;
}

#undef NextToken


#if TEST
void test_parseAtom() {

#define ASSERT_EXPR_KIND(expected) \
    expr = parseAtom(&p); \
    ASSERT(expr->kind == expected)

    Expr *expr;

    InitErrorBuffers();
    Lexer lex = MakeLexer("a 1 1.0 #line nil fn () -> void", NULL);
    Package test = {0};
    Token tok = NextToken(&lex);
    Parser p = {lex, .tok = tok, test};
    ASSERT_EXPR_KIND(ExprKind_Ident);
    ASSERT_EXPR_KIND(ExprKind_LitInt);
    ASSERT_EXPR_KIND(ExprKind_LitFloat);
    ASSERT_EXPR_KIND(ExprKind_LocationDirective);
    ASSERT_EXPR_KIND(ExprKind_LitNil);
    ASSERT_EXPR_KIND(ExprKind_TypeFunction);
}
#endif
