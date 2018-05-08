#define TOKEN_KINDS \
    TKind(Invalid, "<invalid>"), \
    TKind(Eof, "EOF"), \
    TKind(Comment, "comment"), \
    TKind(Ident, "identifier"), \
    TKind(Directive, "directive"), \
    TKind(Int, "int"), \
    TKind(Float, "float"), \
    TKind(String, "string"), \
    TKind(Add, "+"), \
    TKind(Sub, "-"), \
    TKind(Mul, "*"), \
    TKind(Div, "/"), \
    TKind(Rem, "%"), \
    TKind(And, "&"), \
    TKind(Or, "|"), \
    TKind(Xor, "^"), \
    TKind(Shl, "<<"), \
    TKind(Shr, ">>"), \
    TKind(AssignAdd, "+="), \
    TKind(AssignSub, "-="), \
    TKind(AssignMul, "*="), \
    TKind(AssignDiv, "/="), \
    TKind(AssignRem, "%="), \
    TKind(AssignAnd, "&="), \
    TKind(AssignOr, "|="), \
    TKind(AssignXor, "^="), \
    TKind(AssignShl, "<<="), \
    TKind(AssignShr, ">>="), \
    TKind(Land, "&&"), \
    TKind(Lor, "||"), \
    TKind(Lss, "<"), \
    TKind(Gtr, ">"), \
    TKind(Not, "!"), \
    TKind(Eql, "=="), \
    TKind(Neq, "!="), \
    TKind(Leq, "<="), \
    TKind(Geq, ">="), \
    TKind(Assign, "="), \
    TKind(Ellipsis, ".."), \
    TKind(Dollar, "$"), \
    TKind(Question, "?"), \
    TKind(RetArrow, "->"), \
    TKind(Lparen, "("), \
    TKind(Lbrack, "["), \
    TKind(Lbrace, "{"), \
    TKind(Rparen, ")"), \
    TKind(Rbrack, "]"), \
    TKind(Rbrace, "}"), \
    TKind(Comma, ","), \
    TKind(Period, "."), \
    TKind(Colon, ":"), \
    TKind(Semicolon, ";"), \
\
    /* NOTE: all keywords must come after this case and be before "__Meta_KeywordsEnd" */ \
    /* These keywords are roughly-sorted by how common they are */ \
    TKind(__Meta_KeywordsStart, ""), \
    TKind(If, "if"), \
    TKind(For, "for"), \
    TKind(Fn, "fn"), \
    TKind(Return, "return"), \
    TKind(Nil, "nil"), \
    TKind(Struct, "struct"), \
    TKind(Enum, "enum"), \
    TKind(Union, "union"), \
    TKind(Else, "else"), \
    TKind(Switch, "switch"), \
    TKind(Case, "case"), \
    TKind(Cast, "cast"), \
    TKind(Bitcast, "bitcast"), \
    TKind(Autocast, "autocast"), \
    TKind(Using, "using"), \
    TKind(Goto, "goto"), \
    TKind(Break, "break"), \
    TKind(Continue, "continue"), \
    TKind(Fallthrough, "fallthrough"), \
    TKind(Defer, "defer"), \
    TKind(In, "in"), \
    TKind(Variant, "variant"), \
    TKind(__Meta_KeywordsEnd, "")

enum TokenKind {
#define TKind(e, s) TK_##e
    TOKEN_KINDS
#undef TKind
};

String const TokenDescriptions[] = {
#define TKind(e, s) STR(s)
    TOKEN_KINDS
#undef TKind
};

String DescribeTokenKind(TokenKind tk) {
    return TokenDescriptions[tk];
}

struct Position {
    String filename;
    u32 offset,
        line,
        column;
};

struct Token {
    TokenKind kind;
    String lit;
    Position pos;
};

Token InvalidToken = {
    TK_Invalid,
    STR("<invalid>")
};

struct Lexer {
    String path;
    String data;

    u32 lineCount;
    u32 offset;
    u32 column;

    u32 currentCp;

    u8 currentCpWidth;

    b8 insertSemi;
    b8 insertSemiBeforeLBrace;
};

void NextCodePoint(Lexer *l);

b32 MakeLexer(Lexer *l, String path) {
    String data;
    b32 ok = ReadFile(&data, path);
    if (!ok) {
        return false;
    }

    l->path = path;
    l->data = data;
    l->offset = 0;
    l->lineCount = 1;
    l->insertSemi = false;
    l->insertSemiBeforeLBrace = false;

    NextCodePoint(l);

    return true;
}

void NextCodePoint(Lexer *l) {
    if (l->offset < l->data.len) {
        String curr = Slice(l->data, l->offset, l->data.len);
        u32 cp, cpWidth;
        cp = DecodeCodePoint(&cpWidth, curr);

        if (cp == '\n') {
            l->lineCount += (l->insertSemi) ? 1 : 0;
            l->column = 0;
        }

        l->currentCp = cp;
        l->currentCpWidth = cpWidth;
        l->offset += cpWidth;
        l->column += 1;
    } else {
        l->offset = l->data.len - 1;
        if (l->data[l->offset] == '\n') {
            l->lineCount += 1;
            l->column = 0;
        }

        l->currentCp = FileEnd;
        l->currentCpWidth = 1;
    }
}

void skipWhitespace(Lexer *l) {
    while (true) {
        switch (l->currentCp) {
        case '\n': {
            if (l->insertSemi)
                return;
            NextCodePoint(l);
        } break;

        case ' ':
        case '\t':
        case '\r': {
            NextCodePoint(l);
        } break;

        default:
            return;
        }
    }
}

u32 digitValue(u32 cp) {
    if (cp >= '0' && cp <= '9')
        return cp - '0';
    else if (cp >= 'A' && cp <= 'F')
        return cp - 'A' + 10;
    else if (cp >= 'a' && cp <= 'f')
        return cp - 'a' + 10;
    else
        return 16;
}

void scanMatissa(Lexer *l, u32 base) {
    while (l->currentCp != FileEnd) {
        if (l->currentCp != '_' && digitValue(l->currentCp) >= base)
            break;

        NextCodePoint(l);
    }
}

Token scanNumber(Lexer *l, b32 seenDecimal) {
    u32 start = l->offset;

    Token token;
    token.kind = TK_Int;
    token.lit = Slice(l->data, start-l->currentCpWidth, start);
    token.pos.filename = l->path;
    token.pos.line = l->lineCount;
    token.pos.offset = l->offset;
    token.pos.column = l->column;

    b32 mustBeInteger = false;

    if (seenDecimal) {
        token.kind = TK_Float;
        scanMatissa(l, 10);
    }

    if (l->currentCp == '0' && !seenDecimal) {
        NextCodePoint(l);

        switch (l->currentCp) {
        case 'x': {
            NextCodePoint(l);
            scanMatissa(l, 16);
            mustBeInteger = true;
        } break;

        case 'b': {
            NextCodePoint(l);
            scanMatissa(l, 2);
            mustBeInteger = true;
        } break;

        default:
            scanMatissa(l, 10);
        }
    }

    if (!seenDecimal && !mustBeInteger) {
        scanMatissa(l, 10);
    }

    if (l->currentCp == '.' && !seenDecimal && !mustBeInteger) {
        NextCodePoint(l);
        token.kind = TK_Float;
        scanMatissa(l, 10);
    }

    // TODO(Brett): exponent

    token.lit.len = l->offset - start;
    return token;
}

b32 findLineEnd(Lexer *l) {
    u32 cp, offset, lc;
    cp = l->currentCp;
    offset = l->offset;
    lc = l->lineCount;
    b32 found = false;

    while (l->currentCp == '/' || l->currentCp == '*') {
        if (l->currentCp == '/') {
            found = true;
            goto done;
        }

        NextCodePoint(l);
        while (l->currentCp != FileEnd) {
            if (l->currentCp == '\n') {
                found = true;
                goto done;
            }

            NextCodePoint(l);
            if (l->currentCp == '*' && l->currentCp == '/') {
                break;
            }
        }

        skipWhitespace(l);
        if (l->currentCp == '\n' || l->currentCp == FileEnd) {
            found = true;
            goto done;
        }

        if (l->currentCp != '/') {
            found = false;
            goto done;
        }

        NextCodePoint(l);
    }

done:
    l->currentCp = cp;
    l->offset = offset;
    l->lineCount = lc;
    return found;
}

void scanEscape(Lexer *l) {
    switch (l->currentCp) {
    case 'a':
    case 'b':
    case 'f':
    case 'n':
    case 'r':
    case 't':
    case 'v':
    case '\\':
    case '"': {
        NextCodePoint(l);
    } break;

    default:
        UNIMPLEMENTED(); // TODO(Brett): "unknown escape" error
    }
}

#define CASE1(c1, t1) \
    case c1: \
        token.kind = t1; \
        break;

#define CASE2(c1, t1, c2, t2) \
    case c1: \
        token.kind = t1; \
        if (l->currentCp == c2) { \
            NextCodePoint(l); \
            token.kind = t2; \
        } \
        break;
    
#define CASE3(c1, t1, c2, t2, c3, t3) \
    case c1: \
        token.kind = t1; \
        if (l->currentCp == c2) { \
            NextCodePoint(l); \
            token.kind = t2; \
        } else if (l->currentCp == c3) { \
            NextCodePoint(l); \
            token.kind = t3; \
        } \
        break;

#define CASE_SHIFT(c1, t1, t2, teq1, teq2) \
    case c1: \
        token.kind = t1; \
        if (l->currentCp == c1) { \
            NextCodePoint(l); \
            token.kind = t2; \
            if (l->currentCp == '=') { \
                token.kind = teq2; \
            } \
        } else if (l->currentCp == '=') { \
            token.kind = teq1; \
        } \
        break;

Token NextToken(Lexer *l) {
    skipWhitespace(l);

    u8 *start = &l->data[l->offset - l->currentCpWidth];
    String lit = MakeString(start, l->currentCpWidth);

    Token token = InvalidToken;
    token.pos.filename = l->path;
    token.pos.line = l->lineCount;
    token.pos.offset = l->offset;
    token.pos.column = l->column;

    l->insertSemi = false;

    u32 cp = l->currentCp;
    if (IsAlpha(cp)) {
        u32 offset = l->offset;
        while (IsAlpha(cp) || IsNumeric(cp)) {
            NextCodePoint(l);
            cp = l->currentCp;
        }

        lit.len = l->offset - offset;
        token.kind = TK_Ident;

        for (u32 i = TK___Meta_KeywordsStart; i < TK___Meta_KeywordsEnd; i += 1) {
            if (lit == TokenDescriptions[i]) {
                token.kind = (TokenKind)i;
                break;
            }
        }

        switch (token.kind) {
        case TK_Ident:
        case TK_Break:
        case TK_Continue:
        case TK_Fallthrough:
        case TK_Return:
        case TK_Nil: {
            l->insertSemi = true;
        } break;

        case TK_If:
        case TK_For:
        case TK_Switch: {
            l->insertSemiBeforeLBrace = true;
        } break;
        }
    } else if (IsNumeric(cp)) {
        l->insertSemi = true;
        // NOTE: `scanNumber` handles its own lit parsing logic and we must return this token
        // or the `token.lit = lit` below will override it
        return scanNumber(l, false);
    } else {
        NextCodePoint(l);

        switch (cp) {
        case FileEnd: {
            if (l->insertSemi) {
                l->insertSemi = false;
                token.kind = TK_Semicolon;
                lit = STR("\n");
            } else {
                token.kind = TK_Eof;
                lit.len = 0;
            }
        } break;

        case '\n': {
            // NOTE: we only reach here is self.insertSemi was
            // set in the first place and exited early
            // from `skipWhitespace`
            l->insertSemi = false;
            l->insertSemiBeforeLBrace = false;
            token.kind = TK_Semicolon;
        } break;

        case '"': {
            l->insertSemi = true;
            u32 offset = l->offset - l->currentCpWidth;

            while (true) {
                u32 cp = l->currentCp;

                if (cp == FileEnd) {
                    // TODO(Brett): error handling
                    UNIMPLEMENTED(); //"String literal not terminated"
                    break;
                }

                NextCodePoint(l);

                if (cp == '"')
                    break;

                if (cp == '\\')
                    scanEscape(l);
            }

            lit.len = l->offset - offset;
            token.kind = TK_String;
            StringEscapeError status;
            status = EscapeString(&lit);
            if (status == SEE_Error) {
                // TODO(Brett): report error
            } else if (status == SEE_AllocatedMem) {
                // TODO(Brett): register this allocation
                fprintf(stderr, "NOTE: string escaping is currently unsupported\n");
            }
        } break;

        case '/': {
            token.kind = TK_Div;
            if (l->currentCp == '/' || l->currentCp == '*') {
                token.kind = TK_Comment;
                if (l->insertSemi && findLineEnd(l)) {
                    l->insertSemi = false;
                    token.kind = TK_Semicolon;
                    lit = STR("\n");
                } else {
                    u32 offset = l->offset - l->currentCpWidth;
                    if (l->currentCp == '/') {
                        NextCodePoint(l);
                        while (l->currentCp != '\n' && l->currentCp != FileEnd) {
                            NextCodePoint(l);
                        }
                    } else {
                        for (cp = l->currentCp; cp != FileEnd; cp = l->currentCp) {
                            NextCodePoint(l);
                            if (cp == '*' && l->currentCp == '/') {
                                NextCodePoint(l);
                                break;
                            }
                        }
                    }

                    lit.len = l->offset - offset;
                }
            } else if (l->currentCp == '=') {
                NextCodePoint(l);
                token.kind = TK_AssignDiv;
            }
        } break;

        CASE1(':', TK_Colon);
        CASE1('$', TK_Dollar);
        CASE1('?', TK_Question);
        CASE1(',', TK_Comma);
        CASE1(';', TK_Semicolon);
        CASE1('#', TK_Directive);
        CASE1('(', TK_Lparen);
        CASE1('[', TK_Lbrack);
        CASE1('{', TK_Lbrace);
        CASE2('!', TK_Not, '=', TK_Neq);
        CASE2('+', TK_Add, '=', TK_AssignAdd);
        CASE2('*', TK_Mul, '=', TK_AssignMul);
        CASE2('%', TK_Rem, '=', TK_AssignRem);
        CASE2('^', TK_Xor, '=', TK_AssignXor);
        CASE2('=', TK_Assign, '=', TK_Eql);
        CASE2('.', TK_Period, '.', TK_Ellipsis);
        CASE3('|', TK_Or, '=', TK_AssignOr, '|', TK_Lor);
        CASE3('&', TK_And, '=', TK_AssignAnd, '&', TK_Land);
        CASE3('-', TK_Sub, '=', TK_AssignSub, '>', TK_RetArrow);
        CASE_SHIFT('>', TK_Gtr, TK_Shr, TK_Geq, TK_AssignShr);
        CASE_SHIFT('<', TK_Lss, TK_Shl, TK_Leq, TK_AssignShl);

        case ')': { token.kind = TK_Rparen; l->insertSemi = true; } break;
        case ']': { token.kind = TK_Rbrack; l->insertSemi = true; } break;
        case '}': { token.kind = TK_Rbrace; l->insertSemi = true; } break;
        default: token.kind = TK_Invalid;
        }
    }

    token.lit = lit;
    return token;
}

#undef CASE1
#undef CASE2
#undef CASE3
#undef CASE4
