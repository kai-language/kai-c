
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
    Token tok;
    Package package;
};

bool isNotRbraceOrEOF(Parser *p) {
    if (p->tok.kind == TK_Rbrace) return false;
    else if (p->tok.kind == TK_Eof) return false;
    return true;
}

Expr *parseFunctionLiteral(Parser *parser) {
   UNIMPLEMENTED();
   return NULL;
}

Error ExpectedToken(TokenKind kind, Token actual) {
    char *msg = errorBuffPrintf("Expected '%s' but got '%s'", DescribeTokenKind(kind), DescribeTokenKind(actual.kind));
    return (Error) {
        .code = ExpectedTokenError,
        .pos = actual.pos,
        .message = msg,
    };
}

#define ConsumeToken() p->tok = NextToken(&p->lexer)

Expr *parseAtom(Parser *p) {
   switch (p->tok.kind) {
       case TK_Ident: {
           Expr *e = NewExprIdent(&p->package, p->tok.pos, p->tok.val.ident);
           ConsumeToken();
           return e;
       }

       case TK_Int: {
           Expr *e = NewExprLitInt(&p->package, p->tok.pos, p->tok.val.i);
           ConsumeToken();
           return e;
       }

       case TK_Float: {
           Expr *e = NewExprLitFloat(&p->package, p->tok.pos, p->tok.val.f);
           ConsumeToken();
           return e;
       }

       case TK_Lparen: {
           Position start = p->tok.pos;
           ConsumeToken();
           Expr *expr = parseAtom(p);
           Token end = NextToken(&p->lexer);
           // TODO: expect and error recovery
           if (end.kind != TK_Rparen) {
               UNIMPLEMENTED();
           }
           Expr *e = NewExprParen(&p->package, expr, start, p->tok.pos);
           ConsumeToken();
           return e;
       }

       case TK_Directive: {
           if (p->tok.val.ident == internLocation || p->tok.val.ident == internFile || p->tok.val.ident == internLine || p->tok.val.ident == internFunction) {
               Expr *expr = NewExprLocationDirective(&p->package, p->tok.pos, p->tok.val.ident);
               p->tok = NextToken(&p->lexer);
               return expr;
           }
       }

       case TK_Keyword:
           if (p->tok.val.ident == Keyword_fn) {
               return parseFunctionLiteral(p);
           } else if (p->tok.val.ident == Keyword_nil) {
               Expr *expr = NewExprLitNil(&p->package, p->tok.pos);
               p->tok = NextToken(&p->lexer);
               return expr;
           }

       default:
           PARSER_ERROR(p->tok.pos, "Unexpected '%s'", DescribeTokenKind(p->tok.kind));

   }

    Position start = p->tok.pos;
    p->tok = NextToken(&p->lexer);
    return NewExprInvalid(&p->package, start, p->tok.pos);
}


#if TEST
void test_parseAtom() {

#define ASSERT_EXPR_KIND(expected) \
    expr = parseAtom(&p); \
    ASSERT(expr->kind == expected)

    Expr *expr;

    InitErrorBuffers();
    Lexer lex = MakeLexer("a 1 1.0 (b) #line nil () -> void", NULL);
    Package test = {0};
    Token tok = NextToken(&lex);
    Parser p = {lex, tok, test};
    ASSERT_EXPR_KIND(ExprKind_Ident);
    ASSERT_EXPR_KIND(ExprKind_LitInt);
    ASSERT_EXPR_KIND(ExprKind_LitFloat);
    ASSERT_EXPR_KIND(ExprKind_Paren);
    ASSERT_EXPR_KIND(ExprKind_LocationDirective);
    ASSERT_EXPR_KIND(ExprKind_LitNil);
}
#endif
