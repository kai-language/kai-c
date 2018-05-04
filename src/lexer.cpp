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
    TKind(Cast, "cast"), \
    TKind(Bitcast, "bitcast"), \
    TKind(Autocast, "autocast"), \
    TKind(Using, "using"), \
    TKind(Goto, "goto"), \
    TKind(Break, "break"), \
    TKind(Continue, "continue"), \
    TKind(Fallthrough, "fallthrough"), \
    TKind(Return, "return"), \
    TKind(If, "if"), \
    TKind(For, "for"), \
    TKind(Else, "else"), \
    TKind(Defer, "defer"), \
    TKind(In, "in"), \
    TKind(Switch, "switch"), \
    TKind(Case, "case"), \
    TKind(Fn, "fn"), \
    TKind(Union, "union"), \
    TKind(Variant, "variant"), \
    TKind(Enum, "enum"), \
    TKind(Struct, "struct"), \
    TKind(Nil, "nil")

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
        String curr = Slice(l->data, l->offset, l->data.len - 1);
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

TokenKind GetTokenKind(String ident) {
    return TK_Ident;
}

TokenKind switch2(Lexer *l, TokenKind t1, TokenKind t2) {
    if (l->currentCp == '=') {
        NextCodePoint(l);
        return t2;
    }

    return t1;
}

TokenKind switch3(Lexer *l, TokenKind t1, TokenKind t2, TokenKind t3, u32 ch) {
    if (l->currentCp == '=') {
        NextCodePoint(l);
        return t2;
    }

    if (l->currentCp == ch) {
        NextCodePoint(l);
        return t3;
    }

    return t1;
}

TokenKind switch4(Lexer *l, TokenKind t1, TokenKind t2, TokenKind t3, TokenKind t4, u8 ch) {
    if (l->currentCp == '=') {
        NextCodePoint(l);
        return t2;
    }

    if (l->currentCp == ch) {
        NextCodePoint(l);
        if (l->currentCp == '=') {
            NextCodePoint(l);
            return t4;
        }
        return t3;
    }

    return t1;
}

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
        token.kind = GetTokenKind(lit);

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
                token.lit = STR("\n");
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

        case '+': token.kind = switch2(l, TK_Add, TK_AssignAdd); break;
        case '-': token.kind = switch3(l, TK_Sub, TK_AssignSub, TK_RetArrow, '>'); break;
        case '*': token.kind = switch2(l, TK_Mul, TK_AssignMul); break;
        case '/': UNIMPLEMENTED(); break;

        case '%': token.kind = switch2(l, TK_Rem, TK_AssignRem);
        case '^': token.kind = switch2(l, TK_Xor, TK_AssignXor);
        case '>': token.kind = switch4(l, TK_Gtr, TK_Geq, TK_Shr, TK_AssignShr, '>');
        case '<': token.kind = switch4(l, TK_Lss, TK_Leq, TK_Shl, TK_AssignShl, '<');
        case '=': token.kind = switch2(l, TK_Assign, TK_Eql);
        case '!': token.kind = switch2(l, TK_Not,    TK_Neq);
        case '&': token.kind = switch3(l, TK_And, TK_AssignAnd, TK_Land, '&');
        case '|': token.kind = switch3(l, TK_Or, TK_AssignOr, TK_Lor, '|');

        case ':': token.kind = TK_Colon; break;
        case '$': token.kind = TK_Dollar; break;
        case '?': token.kind = TK_Question; break;
        case ',': token.kind = TK_Comma; break;
        case ';': token.kind = TK_Semicolon; break;
        case '#': token.kind = TK_Directive; break;

        case '(': token.kind = TK_Lparen; break;
        case '[': token.kind = TK_Lbrack; break;
        case '{': token.kind = TK_Lbrace; break;


        case ')': { token.kind = TK_Rparen; l->insertSemi = true; } break;
        case ']': { token.kind = TK_Rbrack; l->insertSemi = true; } break;
        case '}': { token.kind = TK_Rbrace; l->insertSemi = true; } break;

        case '.': {
            token.kind = TK_Period;
            if (l->currentCp == '.') {
                token.kind = TK_Ellipsis;
                lit.len = 2;
                NextCodePoint(l);
            }
        } break;

        default: token.kind = TK_Invalid;
        }
    }

    token.lit = lit;
    return token;
}
