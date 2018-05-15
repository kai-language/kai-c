
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

Expr *parseFunctionLiteral(Parser *parser) {
   UNIMPLEMENTED();
   return NULL;
}

Expr *parseAtom(Parser *p) {
   switch (p->tok.kind) {
       case TK_Ident: {
           Expr *e = NewExprIdent(&p->package, p->tok.pos, p->tok.val.ident);
           p->tok = NextToken(&p->lexer);
           return e;
       }

       case TK_Int: {
           Expr *e = NewExprLitInt(&p->package, p->tok.pos, p->tok.val.i);
           p->tok = NextToken(&p->lexer);
           return e;
       }

       case TK_Float: {
           Expr *e = NewExprLitFloat(&p->package, p->tok.pos, p->tok.val.f);
           p->tok = NextToken(&p->lexer);
           return e;
       }

       case TK_Lparen: {
           Position start = p->tok.pos;
           p->tok = NextToken(&p->lexer);
           Expr *expr = parseAtom(p);
           Token end = NextToken(&p->lexer);
           // TODO: expect and error recovery
           if (end.kind != TK_Rparen) {
               UNIMPLEMENTED();
           }
           Expr *e = NewExprParen(&p->package, expr, start, p->tok.pos);
           p->tok = NextToken(&p->lexer);
           return e;
       }

       case TK_Keyword:
           if (p->tok.val.ident == fnKeyword) return parseFunctionLiteral(p);

       default:
           PARSER_ERROR(p->tok.pos, "Unexpected '%s'", DescribeTokenKind(p->tok.kind));

   }

    Position start = p->tok.pos;
    p->tok = NextToken(&p->lexer);
    return NewExprInvalid(&p->package, start, p->tok.pos);
}
