
const char *Keyword_if;
const char *Keyword_for;
const char *Keyword_fn;
const char *Keyword_return;
const char *Keyword_nil;
const char *Keyword_struct;
const char *Keyword_enum;
const char *Keyword_union;
const char *Keyword_else;
const char *Keyword_switch;
const char *Keyword_case;
const char *Keyword_cast;
const char *Keyword_bitcast;
const char *Keyword_autocast;
const char *Keyword_using;
const char *Keyword_goto;
const char *Keyword_break;
const char *Keyword_continue;
const char *Keyword_fallthrough;
const char *Keyword_defer;

const char *Keyword_first;
const char *Keyword_last;
const char **Keywords;

const char *internNewline;
const char *internSemicolon;

const char *internIn;

// Directive names
const char *internImport;
const char *internAssert;
const char *internForeign;
const char *internLocation;
const char *internLine;
const char *internFile;
const char *internFunction;

#define KEYWORD(name) Keyword_##name = StrIntern(#name); ArrayPush(Keywords, Keyword_##name)

void InitKeywords(void) {
    static bool inited;
    if (inited) {
        return;
    }

    KEYWORD(if);
    KEYWORD(for);
    KEYWORD(fn);
    KEYWORD(return);
    KEYWORD(nil);
    KEYWORD(struct);
    KEYWORD(enum);
    KEYWORD(union);
    KEYWORD(else);
    KEYWORD(switch);
    KEYWORD(case);
    KEYWORD(cast);
    KEYWORD(bitcast);
    KEYWORD(autocast);
    KEYWORD(using);
    KEYWORD(goto);
    KEYWORD(break);
    KEYWORD(continue);
    KEYWORD(fallthrough);
    KEYWORD(defer);
    Keyword_first = Keyword_if;
    Keyword_last = Keyword_defer;

    internNewline = StrIntern("\n");
    internSemicolon = StrIntern(";");

    internIn = StrIntern("in");

    internImport = StrIntern("import");
    internAssert = StrIntern("assert");
    internForeign = StrIntern("foreign");
    internLocation = StrIntern("location");
    internLine = StrIntern("line");
    internFile = StrIntern("file");
    internFunction = StrIntern("function");

    inited = true;
}

#undef KEYWORD

bool isKeyword(const char *name) {
    return Keyword_first <= name && name <= Keyword_last;
}

bool shouldInsertSemiAfterKeyword(const char *keyword) {
    if (keyword == Keyword_break) return true;
    if (keyword == Keyword_return) return true;
    if (keyword == Keyword_continue) return true;
    if (keyword == Keyword_fallthrough) return true;
    return false;
}

#define TOKEN_KINDS \
    FOR_EACH(Eof, "EOF") \
    FOR_EACH(Comment, "comment") \
    FOR_EACH(Ident, "identifier") \
    FOR_EACH(Directive, "directive") \
    FOR_EACH(Int, "int") \
    FOR_EACH(Float, "float") \
    FOR_EACH(String, "string") \
    FOR_EACH(Add, "+") \
    FOR_EACH(Sub, "-") \
    FOR_EACH(Mul, "*") \
    FOR_EACH(Div, "/") \
    FOR_EACH(Rem, "%") \
    FOR_EACH(And, "&") \
    FOR_EACH(Or, "|") \
    FOR_EACH(Xor, "^") \
    FOR_EACH(Shl, "<<") \
    FOR_EACH(Shr, ">>") \
    FOR_EACH(AddAssign, "+=") \
    FOR_EACH(SubAssign, "-=") \
    FOR_EACH(MulAssign, "*=") \
    FOR_EACH(DivAssign, "/=") \
    FOR_EACH(RemAssign, "%=") \
    FOR_EACH(AndAssign, "&=") \
    FOR_EACH(OrAssign, "|=") \
    FOR_EACH(XorAssign, "^=") \
    FOR_EACH(ShlAssign, "<<=") \
    FOR_EACH(ShrAssign, ">>=") \
    FOR_EACH(Land, "&&") \
    FOR_EACH(Lor, "||") \
    FOR_EACH(Lss, "<") \
    FOR_EACH(Gtr, ">") \
    FOR_EACH(Not, "!") \
    FOR_EACH(BNot, "~") \
    FOR_EACH(Eql, "==") \
    FOR_EACH(Neq, "!=") \
    FOR_EACH(Leq, "<=") \
    FOR_EACH(Geq, ">=") \
    FOR_EACH(Assign, "=") \
    FOR_EACH(Ellipsis, "..") \
    FOR_EACH(Dollar, "$") \
    FOR_EACH(Question, "?") \
    FOR_EACH(RetArrow, "->") \
    FOR_EACH(Lparen, "(") \
    FOR_EACH(Lbrack, "[") \
    FOR_EACH(Lbrace, "{") \
    FOR_EACH(Rparen, ")") \
    FOR_EACH(Rbrack, "]") \
    FOR_EACH(Rbrace, "}") \
    FOR_EACH(Comma, ",") \
    FOR_EACH(Dot, ".") \
    FOR_EACH(Colon, ":") \
    FOR_EACH(Terminator, ";") \
    FOR_EACH(Keyword, "")

typedef enum TokenKind TokenKind;
enum TokenKind {
#define FOR_EACH(e, s) TK_##e,
    TOKEN_KINDS
#undef FOR_EACH
    NUM_TOKEN_KINDS,
};

const char *TokenDescriptions[] = {
#define FOR_EACH(e, s) "" #s ""
    TOKEN_KINDS
#undef FOR_EACH
};

const char *DescribeTokenKind(TokenKind tk) {
    return TokenDescriptions[tk];
}

Error InvalidEscape(Position pos) {
    return (Error) {
        .code = InvalidEscapeError,
        .pos = pos,
        .message = "Escape sequence is an invalid Unicode codepoint"
    };
}

Error StringContainsNewline(Position pos) {
    return (Error) {
        .code = StringContainsNewlineError,
        .pos = pos,
        .message = "String literal cannot contain a newline"
    };
}

Error UnexpectedEOF(Position pos) {
    return (Error) {
        .code = UnexpectedEOFError,
        .pos = pos,
        .message = "Unexpectedly reached end of file while parsing string literal"
    };
}

Error FloatOverflow(Position pos) {
    return (Error) {
        .code = FloatOverflowError,
        .pos = pos,
        .message = "Float literal is larger than maximum allowed value"
    };
}

Error IntOverflow(Position pos) {
    return (Error) {
        .code = IntOverflowError,
        .pos = pos,
        .message = "Integer literal is larger than maximum allowed value"
    };
}

Error WrongDoubleQuote(Position pos) {
    return (Error) {
        .code = WrongDoubleQuoteError,
        .pos = pos,
        .message = "Unsupported unicode character '“' (0x201c). Did you mean `\"`?\n"
    };
}

Error InvalidCharacterEscape(u32 cp, Position pos) {
    char buff[4];
    u32 len = EncodeCodePoint(&buff[0], cp);
    char *msg = errorBuffPrintf("Invalid character literal escape '\\%.*s'", len, buff);

    return (Error) {
        .code = InvalidCharacterEscapeError,
        .pos = pos,
        .message = msg
    };
}

Error ExpectedDigit(u32 cp, Position pos) {
    char buff[4];
    u32 len = EncodeCodePoint(&buff[0], cp);
    char *msg = errorBuffPrintf("Expected digit after float literal exponent, found '%c'.", len, buff);

    return (Error) {
        .code = ExpectedDigitError,
        .pos = pos,
        .message = msg
    };
}

Error DigitOutOfRange(u32 cp, u32 base, Position pos) {
    char buff[4];
    u32 len = EncodeCodePoint(&buff[0], cp);
    char *msg = errorBuffPrintf("Digit '%.*s' out of range for base %d", len, buff, base);

    return (Error) {
        .code = DigitOutOfRangeError,
        .pos = pos,
        .message = msg
    };
}

Error InvalidUnicodeCodePoint(u32 cp, Position pos) {
    char buff[4];
    u32 len = EncodeCodePoint(&buff[0], cp);
    char *msg = errorBuffPrintf("Invalid Unicode codepoint '%.*s'", len, buff);

    return (Error) {
        .code = InvalidCodePointError,
        .pos = pos,
        .message = msg
    };
}

Error InvalidIdentifierHead(u32 cp, Position pos) {
    switch (cp) {
    case LeftDoubleQuote: return WrongDoubleQuote(pos);
    }

    return InvalidUnicodeCodePoint(cp, pos);
}

typedef struct Token Token;
struct Token {
    TokenKind kind;
    const char *start;
    const char *end;
    Position pos;
    union val {
        unsigned long long i;
        double f;
        const char *s;
        const char *ident;
    } val;
};

typedef struct Lexer Lexer;
struct Lexer {
    const char *stream;
    const char *startOfLine;
    const char *startOfFile;

    Position pos;

    b8 insertSemi;
    b8 insertSemiBeforeLBrace;
};

Lexer MakeLexer(const char *data, const char *name) {
    Lexer l = {0};

    l.stream = data;
    l.startOfLine = data;
    l.startOfFile = data;

    l.pos.name = name;
    l.pos.line = 1;

    return l;
}

u32 NextCodePoint(Lexer *l) {
    u32 cpWidth;
    u32 cp = DecodeCodePoint(&cpWidth, l->stream);
    l->stream += cpWidth;

    return cp;
}

u8 charToDigit[256] = {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

char escapeToChar[256] = {
    ['\''] = '\'',
    ['"'] = '"',
    ['`'] = '`',
    ['\\'] = '\\',
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['b'] = '\b',
    ['a'] = '\a',
    ['0'] = 0,
};

u32 scanNumericEscape(Lexer *l, i32 n, u32 max) {
    u32 x = 0;

    for (; n > 0; n--) {
        u32 cp = NextCodePoint(l);
        if (cp == FileEnd || cp > 255) goto error;
        u32 digit = charToDigit[(u8) cp];
        if (digit == 0 && cp != '0') goto error;
        x *= 16;
        x += digit;
    }


    if (x > max || (0xD800 <= x && x < 0xE000)) {
        Report(InvalidEscape(l->pos));
        return 0;
    }
    return x;

error:
    Report(InvalidEscape(l->pos));
    return 0;
}

// thread_local is C11
// NOTE: We can free this after we are done all lexing
_Thread_local static DynamicArray(char) _scanStringTempBuffer;
const char *scanString(Lexer *l) {
    char quote = *l->stream++;
    ASSERT(quote == '"' || quote == '`');

    Position start = l->pos;

    b8 isMultiline = quote == '`';
    ArrayClear(_scanStringTempBuffer);
    
    char otherQuote = isMultiline ? '`' : '"';

    while (*l->stream && *l->stream != quote) {
        u32 cp = NextCodePoint(l);
        u32 val;
        if (cp == '\n' && !isMultiline) {
            Report(StringContainsNewline(l->pos));
            return NULL;
        } else if (cp == '\\') {
            cp = NextCodePoint(l);
            switch (cp) {
                case 'x':
                    val = scanNumericEscape(l, 2, 0xFF);
                    break;

                case 'u':
                    val = scanNumericEscape(l, 4, 0x0010FFFF);
                    break;

                case 'U':
                    val = scanNumericEscape(l, 8, 0x0010FFFF);
                    break;

                default:
                    if (cp > 255) goto error;
                    val = escapeToChar[(u8) cp];
                    if (val == 0 && cp != '0' && cp != otherQuote) {
                    error:
                        Report(InvalidCharacterEscape(cp, l->pos));
                        return NULL;
                    }
            }
            cp = val;
        }
        // Encode the code point directly to the array
        ArrayFit(_scanStringTempBuffer, ArrayLen(_scanStringTempBuffer) + 4);
        u32 len = EncodeCodePoint(_scanStringTempBuffer + ArrayLen(_scanStringTempBuffer), cp);
        _array_hdr(_scanStringTempBuffer)->len += len;
    }

    u32 closingQuote = NextCodePoint(l);
    if (closingQuote == FileEnd) {
        Report(UnexpectedEOF(start));
        return NULL;
    }
    ASSERT(closingQuote == quote);

    ArrayPush(_scanStringTempBuffer, 0); // Nul term

    return _scanStringTempBuffer;
}

double scanFloat(Lexer *l) {
    const char *start = l->stream;
    while (isdigit(*l->stream)) {
        l->stream++;
    }
    if (*l->stream == '.') {
        l->stream++;
    }
    while (isdigit(*l->stream)) {
        l->stream++;
    }
    if (tolower(*l->stream) == 'e') {
        l->stream++;
        if (*l->stream == '+' || *l->stream == '-') {
            l->stream++;
        }
        if (!isdigit(*l->stream)) {
            Report(ExpectedDigit(*l->stream, l->pos));
        }
        while (isdigit(*l->stream)) {
            l->stream++;
        }
    }

    double val = strtod(start, NULL);
    if (val == HUGE_VAL) {
        Report(FloatOverflow(l->pos));
        return 0.f;
    }
    return val;
}

u64 scanInt(Lexer *l) {
    u32 base = 10;
    const char *start_digits = l->stream;
    if (*l->stream == '0') {
        l->stream++;
        if (tolower(*l->stream) == 'x') {
            l->stream++;
            base = 16;
            start_digits = l->stream;
        } else if (tolower(*l->stream) == 'b') {
            l->stream++;
            base = 2;
            start_digits = l->stream;
        } else if (*l->stream == 'o') {
            l->stream++;
            base = 8;
            start_digits = l->stream;
        } else {
            // Unlike C we do not use a leading zero to change an integer to octal.
            base = 10;
            start_digits = --l->stream;
        }
    }
    u64 val = 0;
    for (;;) {
        if (*l->stream == '_') {
            l->stream++;
            continue;
        }
        int digit = charToDigit[(u8) *l->stream];
        if (digit == 0 && *l->stream != '0') {
            break;
        }
        if (digit >= base) {
            Report(DigitOutOfRange(*l->stream, base, l->pos));
            digit = 0;
        }
        if (val > (ULLONG_MAX - digit) / base) {
            Report(IntOverflow(l->pos));
            while (isdigit(*l->stream)) {
                l->stream++;
            }
            val = 0;
            break;
        }
        val = val * base + digit;
        l->stream++;
    }
    if (l->stream == start_digits) {
        Report(DigitOutOfRange(*l->stream, base, l->pos));
    }
    return val;
}

#define CASE1(c1, t1) \
    case c1: \
        l->stream++; \
        token.kind = t1; \
        break;

#define CASE1_INSERT_SEMI(c1, t1) \
    case c1: \
        l->stream++; \
        token.kind = t1; \
        l->insertSemi = true; \
        break;

#define CASE2(c1, t1, c2, t2) \
    case c1: \
        l->stream++; \
        token.kind = t1; \
        if (*l->stream == c2) { \
            l->stream++; \
            token.kind = t2; \
        } \
        break;
    
#define CASE3(c1, t1, c2, t2, c3, t3) \
    case c1: \
        l->stream++; \
        token.kind = t1; \
        if (*l->stream == c2) { \
            l->stream++; \
            token.kind = t2; \
        } else if (*l->stream == c3) { \
            l->stream++; \
            token.kind = t3; \
        } \
        break;

#define CASE_SHIFT(c1, t1, t2, teq1, teq2) \
    case c1: \
        l->stream++; \
        token.kind = t1; \
        if (*l->stream == c1) { \
            l->stream++; \
            token.kind = t2; \
            if (*l->stream == '=') { \
                l->stream++; \
                token.kind = teq2; \
            } \
        } else if (*l->stream == '=') { \
            l->stream++; \
            token.kind = teq1; \
        } \
        break;

Token NextToken(Lexer *l) {
repeat: ;
    Token token;
    token.start = l->stream;
    token.pos = l->pos;

    if (*token.start == '\n' && l->insertSemi) {
        l->stream++;
        token.kind = TK_Terminator;
        token.val.ident = internNewline;

        token.end = l->stream;
        token.pos.column = (u32)(intptr_t) (token.start - l->startOfLine) + 1;
        token.pos.offset = (u32)(intptr_t) (token.start - l->startOfFile);

        l->pos.line++;
        l->startOfLine = l->stream;
        l->insertSemi = false;
        return token;
    }

    l->insertSemi = false;

    switch (*token.start) {
        case ' ': case '\n': case '\r': case '\t': case '\v': {
            // Skips whitespace
            while (isspace(*l->stream)) {
                if (*l->stream++ == '\n') {
                    l->pos.line++;
                    l->startOfLine = l->stream;
                }
            }
            goto repeat;
        }

        case ';': {
            l->stream++;
            token.kind = TK_Terminator;
            token.val.ident = internSemicolon;
            break;
        }

        case '"': case '`': {
            token.kind = TK_String;
            token.val.s = scanString(l);
            l->insertSemi = true;
            break;
        }

        case '.': {
            if (isdigit(l->stream[1])) {
                token.kind = TK_Float;
                token.val.f = scanFloat(l);
                l->insertSemi = true;
            } else if (l->stream[1] == '.') {
                token.kind = TK_Ellipsis;
                l->stream += 2;
            } else {
                token.kind = TK_Dot;
                l->stream++;
            }
            break;
        }

        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            while (isdigit(*l->stream)) {
                l->stream++;
            }
            char c = *l->stream;
            l->stream = token.start;
            if (c == '.' || tolower(c) == 'e') {
                token.kind = TK_Float;
                token.val.f = scanFloat(l);
            } else {
                token.kind = TK_Int;
                token.val.i = scanInt(l);
            }
            l->insertSemi = true;
            break;
        }

        case '/': {
            token.kind = TK_Div;
            l->stream++;
            if (*l->stream == '=') {
                token.kind = TK_DivAssign;
                l->stream++;
            } else if (*l->stream == '/') {
                l->stream++;
                while (*l->stream && *l->stream != '\n') {
                    l->stream++;
                }
                if (FlagParseComments) goto returnComment;
                goto repeat;
            } else if (*l->stream == '*') {
                l->stream++;
                int level = 1;
                while (*l->stream && level > 0) {
                    if (l->stream[0] == '/' && l->stream[1] == '*') {
                        level++;
                        l->stream += 2;
                    } else if (l->stream[0] == '*' && l->stream[1] == '/') {
                        level--;
                        l->stream += 2;
                    } else {
                        if (*l->stream == '\n') {
                            token.pos.line++;
                        }
                        l->stream++;
                    }
                }
                if (FlagParseComments) goto returnComment;
                goto repeat;
            }
            break;

        returnComment:
            token.kind = TK_Comment;
            size_t len = l->stream - token.start;
            char* mem = (char*) Alloc(DefaultAllocator, len);
            token.val.s = strncpy(mem, token.start, len);
            mem[len] = 0; // replace newline with NUL terminator
            break;
        }

        CASE1_INSERT_SEMI(')', TK_Rparen);
        CASE1_INSERT_SEMI(']', TK_Rbrack);
        CASE1_INSERT_SEMI('}', TK_Rbrace);

        CASE1(':', TK_Colon);
        CASE1('$', TK_Dollar);
        CASE1('?', TK_Question);
        CASE1(',', TK_Comma);
        CASE1('(', TK_Lparen);
        CASE1('[', TK_Lbrack);
        CASE1('{', TK_Lbrace);
        CASE2('!', TK_Not, '=', TK_Neq);
        CASE2('+', TK_Add, '=', TK_AddAssign);
        CASE2('*', TK_Mul, '=', TK_MulAssign);
        CASE2('%', TK_Rem, '=', TK_RemAssign);
        CASE2('^', TK_Xor, '=', TK_XorAssign);
        CASE2('=', TK_Assign, '=', TK_Eql);
        CASE3('|', TK_Or, '=', TK_OrAssign, '|', TK_Lor);
        CASE3('&', TK_And, '=', TK_AndAssign, '&', TK_Land);
        CASE3('-', TK_Sub, '=', TK_SubAssign, '>', TK_RetArrow);
        CASE_SHIFT('>', TK_Gtr, TK_Shr, TK_Geq, TK_ShrAssign);
        CASE_SHIFT('<', TK_Lss, TK_Shl, TK_Leq, TK_ShlAssign);

        case '#': {
            token.kind = TK_Directive;
            l->stream++;
            if (*l->stream == FileEnd) Report(UnexpectedEOF(l->pos));

            u32 cpWidth;
            u32 cp = DecodeCodePoint(&cpWidth, l->stream);
            while (IsIdentifierCharacter(cp)) {
                l->stream += cpWidth;
                cp = DecodeCodePoint(&cpWidth, l->stream);
            }
            token.val.ident = StrInternRange(token.start + 1, l->stream);
            break;
        }

        default: {
            u32 cp = NextCodePoint(l);
            if (cp == FileEnd) {
                token.kind = TK_Eof;
                break;
            }

            if (!IsIdentifierHead(cp)) {
                Report(InvalidIdentifierHead(cp, l->pos));
                l->stream++;
                goto repeat;
            }

            u32 cpWidth;
            cp = DecodeCodePoint(&cpWidth, l->stream);
            while (IsIdentifierCharacter(cp)) {
                l->stream += cpWidth;
                cp = DecodeCodePoint(&cpWidth, l->stream);
            }
            token.val.ident = StrInternRange(token.start, l->stream);
            token.kind = TK_Ident;
            if (isKeyword(token.val.ident)) {
                token.kind = TK_Keyword;
                if (shouldInsertSemiAfterKeyword(token.val.ident)) {
                    l->insertSemi = true;
                }
            } else {
                l->insertSemi = true;
            }
        }
    }
    token.end = l->stream;
    token.pos.column = (u32)(intptr_t) (token.start - l->startOfLine) + 1;
    token.pos.offset = (u32)(intptr_t) (token.start - l->startOfFile);
    return token;
}

#undef CASE1
#undef CASE2
#undef CASE3
#undef CASE4

#if TEST
void test_keywords() {
    InitKeywords();
    ASSERT(isKeyword(Keyword_first));
    ASSERT(isKeyword(Keyword_last));

    for (const char **it = Keywords; it != ArrayEnd(Keywords); it++) {
        ASSERT(isKeyword(*it));
    }
    ASSERT(!isKeyword(StrIntern("asdf")));
    ASSERT(StrIntern("fn") == Keyword_fn);
}
#endif


#if TEST
void test_lexer() {
    test_keywords();

#define ASSERT_TOKEN_IDENT(x) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == TK_Ident, "Expected ident token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(strcmp(tok.val.ident, (x)) == 0, "Expected ident token with value %s got %s", (x), tok.val.s)

#define ASSERT_TOKEN_INT(x) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == TK_Int, "Expected integer token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(tok.val.i == (x), "Expected integer token with value %llu got %llu", (x), tok.val.i)

#define ASSERT_TOKEN_FLOAT(x) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == TK_Float, "Expected float token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(tok.val.f == (x), "Expected float token with value %f got %f", (x), tok.val.f)

#define ASSERT_TOKEN_STRING(x) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == TK_String, "Expected string token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(strcmp(tok.val.s, (x)) == 0, "Expected string token with value %s got %s", (x), tok.val.s)

#define ASSERT_TOKEN_DIRECTIVE(x) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == TK_Directive, "Expected directive token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(strcmp(tok.val.s, (x)) == 0, "Expected directive token with value %s got %s", (x), tok.val.s)

#define ASSERT_TOKEN_EOF() \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == TK_Eof, "Expected EOF token got %s", DescribeTokenKind(tok.kind));

#define ASSERT_TOKEN_KIND(x) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.kind == (x), "Expected EOF token got %s", DescribeTokenKind((x)), DescribeTokenKind(tok.kind));

#define ASSERT_TOKEN_POS(OFFSET, LINE, COLUMN) \
    tok = NextToken(&lex); \
    ASSERT_MSG_VA(tok.pos.line == LINE && tok.pos.column == COLUMN && tok.pos.offset == OFFSET, \
        "Expected token at position %d:%d (offset %d) got %d:%d  (offset %d)", \
        LINE, COLUMN, OFFSET, tok.pos.line, tok.pos.column, tok.pos.offset); \

    Token tok;
    Lexer lex;

    lex = MakeLexer("0 2147483647 0x7fffffff 0b1111", NULL);
    ASSERT_TOKEN_INT(0);
    ASSERT_TOKEN_INT(2147483647);
    ASSERT_TOKEN_INT(0x7fffffff);
    ASSERT_TOKEN_INT(0xf);
    ASSERT_TOKEN_EOF();

    lex = MakeLexer("3.14 .123 42. 3e10", NULL);
    ASSERT_TOKEN_FLOAT(3.14);
    ASSERT_TOKEN_FLOAT(.123);
    ASSERT_TOKEN_FLOAT(42.);
    ASSERT_TOKEN_FLOAT(3e10);
    ASSERT_TOKEN_EOF();

    lex = MakeLexer("\"Hello, \\\"World\\\"\\n\" `\\n` `\\`` \"\\x45\" `\\0` `\u2687` `⚇`", NULL);
    ASSERT_TOKEN_STRING("Hello, \"World\"\n");
    ASSERT_TOKEN_STRING("\n");
    ASSERT_TOKEN_STRING("`");
    ASSERT_TOKEN_STRING("\x45");
    ASSERT_TOKEN_STRING("\0");
    ASSERT_TOKEN_STRING("⚇");
    ASSERT_TOKEN_STRING("⚇");
    ASSERT_TOKEN_EOF();

    lex = MakeLexer(": := + += < <= << <<=", NULL);
    ASSERT_TOKEN_KIND(TK_Colon);
    ASSERT_TOKEN_KIND(TK_Colon);
    ASSERT_TOKEN_KIND(TK_Assign);
    ASSERT_TOKEN_KIND(TK_Add);
    ASSERT_TOKEN_KIND(TK_AddAssign);
    ASSERT_TOKEN_KIND(TK_Lss);
    ASSERT_TOKEN_KIND(TK_Leq);
    ASSERT_TOKEN_KIND(TK_Shl);
    ASSERT_TOKEN_KIND(TK_ShlAssign);
    ASSERT_TOKEN_EOF();

    lex = MakeLexer("\nmain :: fn {\n    print(\"Hello, World\")\n}", NULL);
    ASSERT_TOKEN_POS(1, 2, 1); // main
    ASSERT_TOKEN_POS(6, 2, 6); // :
    ASSERT_TOKEN_POS(7, 2, 7); // :
    ASSERT_TOKEN_POS(9, 2, 9); // fn
    ASSERT_TOKEN_POS(12, 2, 12); // {
    ASSERT_TOKEN_POS(18, 3, 5); // print
    ASSERT_TOKEN_POS(23, 3, 10); // (
    ASSERT_TOKEN_POS(24, 3, 11); // "Hello, World"
    ASSERT_TOKEN_POS(38, 3, 25); // )
    ASSERT_TOKEN_POS(39, 3, 26); // ;
    ASSERT_MSG(tok.kind == TK_Terminator, "Expected terminator to be automatically inserted");
    ASSERT_MSG(tok.val.ident == internNewline, "Expected terminator to set it's value the the character that spawned it");
    ASSERT_TOKEN_POS(40, 4, 1); // }
    ASSERT_TOKEN_EOF();

    lex = MakeLexer("\n#import kai(\"core\")\n", NULL);
    ASSERT_TOKEN_DIRECTIVE("import");
    ASSERT_TOKEN_IDENT("kai");
    ASSERT_TOKEN_KIND(TK_Lparen);
    ASSERT_TOKEN_STRING("core");
    ASSERT_TOKEN_KIND(TK_Rparen);
    ASSERT_TOKEN_KIND(TK_Terminator);
    ASSERT_TOKEN_EOF();
}
#endif
