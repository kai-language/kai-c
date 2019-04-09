#include "lexer.h"

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
const char *Keyword_autocast;
const char *Keyword_using;
const char *Keyword_goto;
const char *Keyword_break;
const char *Keyword_continue;
const char *Keyword_fallthrough;
const char *Keyword_defer;

const char *Keyword_first;
const char *Keyword_last;
DynamicArray(const char *) Keywords;

const char *internNewline;
const char *internSemicolon;

const char *internUnderscore;
const char *internIn;

// Directive names
const char *internLine;
const char *internFile;
const char *internAssert;
const char *internImport;
const char *internCVargs;
const char *internForeign;
const char *internFunction;
const char *internLocation;
const char *internCallConv;
const char *internLinkName;
const char *internLinkPrefix;

const char *internCallConv_C;

#define KEYWORD(name) Keyword_##name = StrIntern(#name); ArrayPush(Keywords, Keyword_##name)

bool initialized_keywords = false;
void InitKeywords() {
    if (initialized_keywords) return;
    initialized_keywords = true;

    KEYWORD(if);
    void *arena_end = internArena.end;
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
    KEYWORD(autocast);
    KEYWORD(using);
    KEYWORD(goto);
    KEYWORD(break);
    KEYWORD(continue);
    KEYWORD(fallthrough);
    KEYWORD(defer);
    ASSERT(internArena.end == arena_end);
    Keyword_first = Keyword_if;
    Keyword_last = Keyword_defer;

    ASSERT(Keyword_first < Keyword_last);

    internNewline = StrIntern("\n");
    internSemicolon = StrIntern(";");

    internUnderscore = StrIntern("_");
    internIn = StrIntern("in");

    internLine = StrIntern("line");
    internFile = StrIntern("file");
    internAssert = StrIntern("assert");
    internImport = StrIntern("import");
    internCVargs = StrIntern("cvargs");
    internForeign = StrIntern("foreign");
    internFunction = StrIntern("function");
    internLocation = StrIntern("location");
    internCallConv = StrIntern("callconv");
    internLinkName = StrIntern("linkname");
    internLinkPrefix = StrIntern("linkprefix");

    internCallConv_C = StrIntern("c");

//    for (int i = 0; i < NUM_KEYWORDS; i++) {
//        if (keywords[i]) keywords[i] = StrIntern(keywords[i]);
//    }
}

#undef KEYWORD

bool isStringKeyword(const char *name) {
    return Keyword_first <= name && name <= Keyword_last;
}

bool shouldInsertSemiAfterKeyword(const char *keyword) {
    if (keyword == Keyword_break) return true;
    if (keyword == Keyword_return) return true;
    if (keyword == Keyword_continue) return true;
    if (keyword == Keyword_fallthrough) return true;
    if (keyword == Keyword_nil) return true;
    return false;
}

const char *TokenDescriptions[NUM_TOKEN_KINDS] = {
    [TK_Eof] = "EOF",
    [TK_Comment] = "comment",
    [TK_Ident] = "identifier",
    [TK_Directive] = "directive",
    [TK_Int] = "int",
    [TK_Float] = "float",
    [TK_String] = "string",
    [TK_Add] = "+",
    [TK_Sub] = "-",
    [TK_Mul] = "*",
    [TK_Div] = "/",
    [TK_Rem] = "%",
    [TK_And] = "&",
    [TK_Or] = "|",
    [TK_Xor] = "^",
    [TK_Shl] = "<<",
    [TK_Shr] = ">>",
    [TK_AddAssign] = "+=",
    [TK_SubAssign] = "-=",
    [TK_MulAssign] = "*=",
    [TK_DivAssign] = "/=",
    [TK_RemAssign] = "%=",
    [TK_AndAssign] = "&=",
    [TK_OrAssign] = "|=",
    [TK_XorAssign] = "^=",
    [TK_ShlAssign] = "<<=",
    [TK_ShrAssign] = ">>=",
    [TK_Land] = "&&",
    [TK_Lor] = "||",
    [TK_Lss] = "<",
    [TK_Gtr] = ">",
    [TK_Not] = "!",
    [TK_BNot] = "~",
    [TK_Eql] = "==",
    [TK_Neq] = "!=",
    [TK_Leq] = "<=",
    [TK_Geq] = ">=",
    [TK_Assign] = "=",
    [TK_Ellipsis] = "..",
    [TK_Dollar] = "$",
    [TK_Question] = "?",
    [TK_RetArrow] = "->",
    [TK_Lparen] = "(",
    [TK_Lbrack] = "[",
    [TK_Lbrace] = "{",
    [TK_Rparen] = ")",
    [TK_Rbrack] = "]",
    [TK_Rbrace] = "}",
    [TK_Comma] = ",",
    [TK_Dot] = ".",
    [TK_Colon] = ":",
    [TK_Terminator] = ";",
    [TK_Keyword] = "keyword",
};

const char *DescribeTokenKind(TokenKind tk) {
    return TokenDescriptions[tk];
}

const char *DescribeToken(Token tok) {
    if (tok.kind == TK_Ident || tok.kind == TK_Keyword || tok.kind == TK_Directive) {
        return tok.val.s;
    }
    return DescribeTokenKind(tok.kind);
}

void lexer_donothing() {}

void lexer_init(Lex2 *self, const char *data) {
    self->start = data;
    self->str = data;
    self->tok = (Tok2){0};
    self->client = (LexerClient){
        .data = NULL,
        .online = (OnLineFunc) (void *) lexer_donothing,
        .onstr = (OnStrFunc) (void *) lexer_donothing,
        .onname = (OnNameFunc) (void *) lexer_donothing,
        .onmsg = (OnMsgFunc) (void *) lexer_donothing,
        .oncomment = NULL,
    };
}

Lexer MakeLexer(const char *data, Package *pkg) {
    Lexer l = {0};

    l.stream = data;
    l.startOfLine = data;
    l.startOfFile = data;

    l.pos.name = pkg ? pkg->path : "<builtin>";
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

b8 valid_char_digit[256] = {
    ['0'] = true,
    ['1'] = true,
    ['2'] = true,
    ['3'] = true,
    ['4'] = true,
    ['5'] = true,
    ['6'] = true,
    ['7'] = true,
    ['8'] = true,
    ['9'] = true,
    ['a'] = true, ['A'] = true,
    ['b'] = true, ['B'] = true,
    ['c'] = true, ['C'] = true,
    ['d'] = true, ['D'] = true,
    ['e'] = true, ['E'] = true,
    ['f'] = true, ['F'] = true,
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

u32 scan_numeric_escape(Lex2 *self, i32 n, u32 max) {
    u32 x = 0;
    for (; n > 0; n--) {
        if (!*self->str) goto error;
        bool valid = valid_char_digit[*self->str];
        int digit = charToDigit[*self->str++];
        if (!valid) goto error;
        x *= 16;
        x += digit;
    }
    if (x > max || (0xD800 <= x && x < 0xE000)) goto error;
    return x;

error:;
    char msg[] = "Escape sequence is invalid";
    self->client.onmsg(self->client.data, (Pos) (self->str - self->start), msg, sizeof msg);
    return 0;
}

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
    if (x > max || (0xD800 <= x && x < 0xE000)) goto error;
    return x;

error:
    ReportError(l->package, InvalidEscapeError, rangeFromPosition(l->pos), "Escape sequence is an invalid Unicode codepoint");
    return 0;
}

// NOTE: We can free this after we are done all lexing
// NOTE: @performance we can save work here by only copying into the temp buffer when we see an
//   escape and otherwise just providing calling onstr with the string directly from the source.
//   We can then in onstr detect that the tok.end - tok.start = len and prevent any allocation.
const char *scan_string(Lex2 *self) {
    const char *start = self->str;
    char quote = *self->str++;
    ASSERT(quote == '"' || quote == '`');
    b8 isMultiline = quote == '`';
    bool has_escape = false;
    u32 width;
    while (*self->str && *self->str != quote) {
        u32 rune = DecodeCodePoint(&width, self->str);
        self->str += width;
        u32 val;
        if (rune == '\n' && !isMultiline) {
            self->client.online(self->client.data, (Pos) (self->str - self->start));
            char msg[] = "Regular string literal cannot contain a newline";
            self->client.onmsg(self->client.data, (Pos) (self->str - self->start), msg, sizeof msg);
            return NULL;
        } else if (rune == '\\') {
            if (!has_escape) { // We need to copy the string into the temp buffer
                ArrayClear(self->string_temp_buffer);
                has_escape = true;
                u32 len = (u32) (self->str - start - 2);
                ArrayFit(self->string_temp_buffer, self->str - start);
                strncpy(self->string_temp_buffer, start + 1, len);
                _array_hdr(self->string_temp_buffer)->len += len;
            }
            rune = DecodeCodePoint(&width, self->str);
            self->str += width;
            switch (rune) {
                case 'x':
                    val = scan_numeric_escape(self, 2, 0xFF);
                    break;
                case 'u':
                    val = scan_numeric_escape(self, 4, 0x0010FFFF);
                    break;
                case 'U':
                    val = scan_numeric_escape(self, 8, 0x0010FFFF);
                    break;
                default:
                    rune = MIN(rune, 255);
                    val = escapeToChar[rune];
                    if (val == 0 && rune != '0') {
                        char msg[1024];
                        int len = snprintf(msg, sizeof msg, "Invalid character literal escape '\\%.*s'", width, self->str - width);
                        self->client.onmsg(self->client.data, (Pos) (self->str - self->start - width), msg, len);
                        return NULL;
                    }
            }
            rune = val;
        }
        // Encode the code point directly to the array
        if (has_escape) {
            size_t temp_buffer_len = ArrayLen(self->string_temp_buffer);
            ArrayFit(self->string_temp_buffer, temp_buffer_len + 4);
            u32 len = EncodeCodePoint(self->string_temp_buffer + temp_buffer_len, rune);
            _array_hdr(self->string_temp_buffer)->len += len;
        }
    }
    if (!*self->str) {
        char msg[] = "Unexpectedly reached end of file while parsing string literal";
        self->client.onmsg(self->client.data, self->tok.range.start, msg, sizeof msg);
        return NULL;
    }
    ASSERT(*self->str == quote);
    self->str++;
    self->tok.range.end = (Pos) (self->str - self->start);
    u32 len = (u32) (has_escape ? ArrayLen(self->string_temp_buffer) : (self->str - start - 2));
    const char *str = has_escape ? self->string_temp_buffer : start + 1;
    return self->client.onstr(self->client.data, &self->tok, str, len, has_escape);
}

// NOTE: We can free this after we are done all lexing
static DynamicArray(char) _scanStringTempBuffer;
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
            ReportError(l->package, StringContainsNewlineError, rangeFromPosition(l->pos), "String literal cannot contain a newline");
            ReportNote(l->package, rangeFromPosition(l->pos), "Multiline string literals use `backticks` instead of \"quotes\"");
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
                    error: ;
                        u32 cpWidth;
                        DecodeCodePoint(&cpWidth, l->stream);
                        ReportError(l->package, InvalidCharacterEscapeError, rangeFromPosition(l->pos), "Invalid character literal escape '\\%.*s'", cpWidth, l->stream);
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
        ReportError(l->package, UnexpectedEOFError, rangeFromPosition(l->pos), "Unexpectedly reached end of file while parsing string literal");
        ReportNote(l->package, rangeFromPosition(start), "String began here");
        return NULL;
    }
    ASSERT(closingQuote == quote);

    ArrayPush(_scanStringTempBuffer, 0); // Nul term

    // TODO: @performance use StrInternRange
    return StrIntern(_scanStringTempBuffer);
}

double scan_float(Lex2 *self) {
    const char *str = self->str;
    while (isdigit(*str)) str++;
    if (*str == '.') str++;
    while (isdigit(*str)) str++;
    if (tolower(*str) == 'e') {
        str++;
        if (*str == '+' || *str == '-') str++;
        if (!isdigit(*str)) {
            char msg[] = "Expected digit after float literal exponent";
            self->client.onmsg(self->client.data, self->tok.range.start, msg, sizeof msg);
        }
        while (isdigit(*str)) str++;
    }
    double val = strtod(self->str, NULL);
    if (val == HUGE_VAL) {
        char msg[] = "Float literal is larger than maximum allowed value";
        self->client.onmsg(self->client.data, self->tok.range.start, msg, sizeof msg);
        val = 0.f;
    }
    self->str = str;
    return val;
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
            u32 cpWidth;
            DecodeCodePoint(&cpWidth, l->stream);
            ReportError(l->package, ExpectedDigitError, rangeFromPosition(l->pos), "Expected digit after float literal exponent, found '%.*s'", cpWidth, l->stream);
        }
        while (isdigit(*l->stream)) {
            l->stream++;
        }
    }

    double val = strtod(start, NULL);
    if (val == HUGE_VAL) {
        ReportError(l->package, FloatOverflowError, rangeFromPosition(l->pos), "Float literal is larger than maximum allowed value");
        return 0.f;
    }
    return val;
}

u64 scan_int(Lex2 *self) {
    u32 base = 10;
    const char *str = self->str;
    if (*str == '0') {
        str++;
        if (tolower(*str) == 'x') {
            str++;
            base = 16;
            self->str = str;
        } else if (tolower(*str) == 'b') {
            str++;
            base = 2;
            self->str = str;
        } else if (*str == 'o') {
            str++;
            base = 8;
            self->str = str;
        } else {
            // Unlike C we do not use a leading zero to change an integer to octal.
            base = 10;
            self->str = --str;
        }
    }
    u64 val = 0;
    for (;;) {
        if (*str == '_') {
            str++;
            continue;
        }
        bool valid = valid_char_digit[*str];
        if (!valid) goto ret;
        int digit = charToDigit[*str];
        if (digit >= base) {
            char msg[1024];
            int len = snprintf(msg, sizeof msg, "Digit '%c' out of range for base '%d'", *str, base);
            self->client.onmsg(self->client.data, self->tok.range.start, msg, len);
            while (isdigit(*str)) str++;
            val = 0;
            break;
        }
        if (val > (ULLONG_MAX - digit) / base) {
            char msg[] = "Integer literal is larger than maximum allowed value";
            self->client.onmsg(self->client.data, self->tok.range.start, msg, sizeof msg);
            while (isdigit(*str)) str++;
            val = 0;
            break;
        }
        val = val * base + digit;
        str++;
    }
ret:;
    self->str = str;
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
            u32 cpWidth;
            DecodeCodePoint(&cpWidth, l->stream);
            ReportError(l->package, DigitOutOfRangeError, rangeFromPosition(l->pos), "Digit '%.*s' out of range for base '%d'", cpWidth, l->stream, base);
        }
        if (val > (ULLONG_MAX - digit) / base) {
            ReportError(l->package, IntOverflowError, rangeFromPosition(l->pos), "Integer literal is larger than maximum allowed value");
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
        u32 cpWidth;
        DecodeCodePoint(&cpWidth, l->stream);
        ReportError(l->package, DigitOutOfRangeError, rangeFromPosition(l->pos), "Digit '%.*s' out of range for base '%d'", cpWidth, l->stream, base);
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

Inline void lexer_match_shift(Lex2 *self, u8 t1, u8 t2, u8 teq1, u8 teq2);
Inline void lexer_match_case3(Lex2 *self, u8 t1, u8 c2, u8 t2, u8 c3, u8 t3);

Tok2 lexer_next_token(Lex2 *self) {
repeat:;
    self->tok.range.start = (u32) (self->str - self->start);

    switch (*self->str) {
        default: {
            self->tok.kind = TK_Ident;
            u32 width;
            u32 rune = DecodeCodePoint(&width, self->str);
            if (!IsIdentifierHead(rune)) {
                char msg[4096];
                int len = snprintf(msg, sizeof msg, "Invalid Unicode codepoint '%.*s'",
                                   width, self->str);
                self->client.onmsg(self->client.data, (u32) (self->str - self->start), msg, len);
                self->tok.kind = TK_Invalid;
                break;
            }
            while (IsIdentifierCharacter(rune)) {
                self->str += width;
                rune = DecodeCodePoint(&width, self->str);
            }
            const char *start = self->start + self->tok.range.start;
            self->tok.range.end = (u32) (self->str - self->start);
            u32 len = self->tok.range.end - self->tok.range.start;
            self->tok.tname = self->client.onname(self->client.data, &self->tok, start, len);
            break;
        }
        case 0: {
            self->tok.kind = TK_Eof;
            break;
        }
        case '#': {
            self->tok.kind = TK_Directive;
            self->str++;
            u32 width;
            u32 rune = DecodeCodePoint(&width, self->str);
            if (!IsIdentifierHead(rune)) {
                char msg[4096];
                int len = snprintf(msg, sizeof msg, "Invalid Unicode codepoint '%.*s'",
                                   width, self->str);
                self->client.onmsg(self->client.data, (u32) (self->str - self->start), msg, len);
                self->tok.kind = TK_Invalid;
                break;
            }
            while (IsIdentifierCharacter(rune)) {
                self->str += width;
                rune = DecodeCodePoint(&width, self->str);
            }
            const char *start = self->start + self->tok.range.start + 1;
            self->tok.range.end = (u32) (self->str - self->start);
            u32 len = self->tok.range.end - self->tok.range.start - 1;
            self->tok.tname = self->client.onname(self->client.data, &self->tok, start, len);
            break;
        }
        case '"': case '`': {
            self->tok.kind = TK_String;
            self->tok.tstr = scan_string(self);
            break;
        }
        case '.': {
            if (isdigit(self->str[1])) {
                self->tok.kind = TK_Float;
                self->tok.tfloat = scan_float(self);
            } else if (self->str[1] == '.') {
                self->tok.kind = TK_Ellipsis;
                self->str += 2;
            } else {
                self->tok.kind = *self->str++;
            }
            break;
        }
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
            while (isdigit(*self->str)) self->str++;
            char c = *self->str;
            self->str = self->start + self->tok.range.start;
            if (c == '.' || tolower(c) == 'e') {
                self->tok.kind = TK_Float;
                self->tok.tfloat = scan_float(self);
            } else {
                self->tok.kind = TK_Int;
                self->tok.tint = scan_int(self);
            }
            break;
        }
        case '/': {
            self->str++;
            if (*self->str == '=') {
                self->tok.kind = TK_DivAssign;
                self->str++;
            } else if (*self->str == '/') {
                self->str++;
                while (*self->str && *self->str != '\n') self->str++;
                if (self->client.oncomment) {
                    Pos start = self->tok.range.start;
                    u32 len = (u32) (self->str - self->tok.range.start);
                    self->client.oncomment(self->client.data, start, self->start + start, len);
                }
                goto repeat;
            } else if (*self->str == '*') {
                self->str++;
                u32 level = 1;
                while (*self->str && level > 0) {
                    if (self->str[0] == '/' && self->str[1] == '*') {
                        level++;
                        self->str += 2;
                    } else if (self->str[0] == '*' && self->str[1] == '/') {
                        level--;
                        self->str += 2;
                    } else {
                        if (*self->str == '\n') {
                            self->client.online(self->client.data, (u32) (self->str - self->start));
                        }
                        self->str++;
                    }
                }
                goto repeat;
            }
            self->tok.kind = TK_Div;
            break;
        }
        case '\n': {
            self->str++;
            self->client.online(self->client.data, (u32) (self->str - self->start));
            self->tok.kind = TK_Terminator;
            break;
        }
        case ';': {
            self->str++;
            self->tok.kind = TK_Terminator;
            break;
        }
        case ' ': case '\r': case '\t': case '\v': {
            // Skips whitespace
            while (isspace(*self->str)) {
                if (*self->str == '\n') self->client.online(self, (u32) (self->str - self->start));
                self->str++;
            }
            goto repeat;
        }
        case '(': case ')': case '[': case ']': case '{': case '}':
        case ':': case '$': case '?': case '~': case ',': { // 1 char
            self->tok.kind = *self->str++;
            break;
        }
        case '!': case '+': case '*': case '%': case '^': case '=': {
            // 2 chars where second maybe '='
            self->tok.kind = *self->str++;
            if (*self->str == '=') {
                self->str++;
                self->tok.kind |= 0x80;
            }
            break;
        }
        case '|': {
            lexer_match_case3(self, '|', '|', TK_Lor, '=', TK_OrAssign);
            break;
        }
        case '&': {
            lexer_match_case3(self, '&', '&', TK_Land, '=', TK_AndAssign);
            break;
        }
        case '-': {
            lexer_match_case3(self, '-', '>', TK_RetArrow, '=', TK_SubAssign);
            break;
        }

        case '>': {
            lexer_match_shift(self, '>', TK_Shr, TK_Geq, TK_ShrAssign);
            break;
        }
        case '<': {
            lexer_match_shift(self, TK_Lss, TK_Shl, TK_Leq, TK_ShlAssign);
            break;
        }
    }
    self->tok.range.end = (u32) (self->str - self->start);
    return self->tok;
}

Inline
void lexer_match_case3(Lex2 *self, u8 t1, u8 c2, u8 t2, u8 c3, u8 t3) {
    self->str++;
    self->tok.kind = t1;
    if (*self->str == c2) {
        self->str++;
        self->tok.kind = t2;
    } else if (*self->str == c3) {
        self->str++;
        self->tok.kind = t3;
    }
}

Inline
void lexer_match_shift(Lex2 *self, u8 t1, u8 t2, u8 teq1, u8 teq2) {
    self->str++;
    self->tok.kind = t1;
    if (*self->str == t1) {
        self->str++;
        self->tok.kind = t2;
        if (*self->str == '=') {
            self->str++;
            self->tok.kind = teq2;
        }
    } else if (*self->str == '=') {
        self->str++;
        self->tok.kind = teq1;
    }
}

Token NextToken(Lexer *l) {
repeat: ;
    Token token;
    token.start = l->stream;
    token.pos = l->pos;

    if ((*token.start == '\n') && l->insertSemi) {
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

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
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
                if (compiler.flags.parseComments) goto returnComment;
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
                if (compiler.flags.parseComments) goto returnComment;
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
        CASE1('~', TK_BNot);
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
            if (*l->stream == FileEnd) ReportError(l->package, UnexpectedEOFError, rangeFromPosition(l->pos), "Unexpectedly reached end of file while parsing directive");

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
                // NextCodePoint advances the stream pointer, past the end of the buffer.
                l->stream -= 1;
                token.kind = TK_Eof;
                break;
            }

            if (!IsIdentifierHead(cp)) {
                switch (cp) {
                    case LeftDoubleQuote:
                        ReportError(l->package, WrongDoubleQuoteError, rangeFromPosition(l->pos), "Unsupported unicode character '“' (0x201c). Did you mean `\"`?");
                        break;
                    default: {
                        char buff[4];
                        u32 len = EncodeCodePoint(buff, cp);
                        ReportError(l->package, InvalidCodePointError, rangeFromPosition(l->pos), "Invalid Unicode codepoint '%.*s'", len, buff);
                    }
                }
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
            if (isStringKeyword(token.val.ident)) {
                token.kind = TK_Keyword;
                l->insertSemi = shouldInsertSemiAfterKeyword(token.val.ident);
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

    ASSERT(isStringKeyword(Keyword_first));
    ASSERT(isStringKeyword(Keyword_last));

    for (const char **it = Keywords; it != ArrayEnd(Keywords); it++) {
        ASSERT(isStringKeyword(*it));
    }
    ASSERT(!isStringKeyword(StrIntern("asdf")));
    ASSERT(StrIntern("fn") == Keyword_fn);
}

const char *lexer_onstr_calloc(void *userdata, Tok2 *tok, const char *str, u32 len, bool is_temp) {
    char *out = calloc(len + 1, sizeof *out);
    strncpy(out, str, len);
    return out;
}

void test_lexer() {

#define ASSERT_TOKEN_IDENT(x) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == TK_Ident, "Expected ident token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(strcmp(tok.tname, (x)) == 0, "Expected ident token with value %s got %s", (x), tok.tname)

#define ASSERT_TOKEN_INT(x) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == TK_Int, "Expected integer token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(tok.tint == (x), "Expected integer token with value %d got %llu", (x), tok.tint)

#define ASSERT_TOKEN_FLOAT(x) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == TK_Float, "Expected float token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(tok.tfloat == (x), "Expected float token with value %f got %f", (x), tok.tfloat)

#define ASSERT_TOKEN_STRING(x) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == TK_String, "Expected string token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(strcmp(tok.tstr, (x)) == 0, "Expected string token with value %s got %s", (x), tok.tstr)

#define ASSERT_TOKEN_DIRECTIVE(x) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == TK_Directive, "Expected directive token got %s", DescribeTokenKind(tok.kind)); \
    ASSERT_MSG_VA(strcmp(tok.tname, (x)) == 0, "Expected directive token with value %s got %s", (x), tok.tname)

#define ASSERT_TOKEN_EOF() \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == TK_Eof, "Expected EOF token got %s", DescribeTokenKind(tok.kind));

#define ASSERT_TOKEN_KIND(x) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.kind == (x), "Expected %s token got %s", DescribeTokenKind((x)), DescribeTokenKind(tok.kind));

#define ASSERT_TOKEN_POS(OFFSET, LINE, COLUMN) \
    tok = lexer_next_token(&lexer); \
    ASSERT_MSG_VA(tok.range.start == OFFSET, \
        "Expected token at position %d:%d (offset %d) got (offset %d)", \
        LINE, COLUMN, OFFSET, tok.range.start); \

    Tok2 tok;
    Lexer lex;
    Lex2 lexer;

    lexer_init(&lexer, "0 2147483647 0x7fffffff 0b1111");
    ASSERT_TOKEN_INT(0);
    ASSERT_TOKEN_INT(2147483647);
    ASSERT_TOKEN_INT(0x7fffffff);
    ASSERT_TOKEN_INT(0xf);
    ASSERT_TOKEN_EOF();

    lexer_init(&lexer, "3.14 .123 42. 3e10");
    ASSERT_TOKEN_FLOAT(3.14);
    ASSERT_TOKEN_FLOAT(.123);
    ASSERT_TOKEN_FLOAT(42.);
    ASSERT_TOKEN_FLOAT(3e10);
    ASSERT_TOKEN_EOF();

    lexer_init(&lexer, "\"Hello, \\\"World\\\"\\n\" `\\n` `\\`` \"\\x45\" `\\0` `\\u2687` `⚇`");
    lexer.client.onstr = lexer_onstr_calloc;
    ASSERT_TOKEN_STRING("Hello, \"World\"\n");
    ASSERT_TOKEN_STRING("\n");
    ASSERT_TOKEN_STRING("`");
    ASSERT_TOKEN_STRING("\x45");
    ASSERT_TOKEN_STRING("\0");
    ASSERT_TOKEN_STRING("⚇");
    ASSERT_TOKEN_STRING("⚇");
    ASSERT_TOKEN_EOF();

    lexer_init(&lexer, ": := + += < <= << <<=");
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

    lexer_init(&lexer, "\nmain :: fn {\n    print(\"Hello, World\")\n}");
    ASSERT_TOKEN_POS(0, 1, 1); // \n
    ASSERT_TOKEN_POS(1, 2, 1); // main
    ASSERT_TOKEN_POS(6, 2, 6); // :
    ASSERT_TOKEN_POS(7, 2, 7); // :
    ASSERT_TOKEN_POS(9, 2, 9); // fn
    ASSERT_TOKEN_POS(12, 2, 12); // {
    ASSERT_TOKEN_POS(13, 2, 13); // \n
    ASSERT_TOKEN_POS(18, 3, 5); // print
    ASSERT_TOKEN_POS(23, 3, 10); // (
    ASSERT_TOKEN_POS(24, 3, 11); // "Hello, World"
    ASSERT_TOKEN_POS(38, 3, 25); // )
    ASSERT_TOKEN_POS(39, 3, 26); // \n
    ASSERT_MSG(tok.kind == TK_Terminator, "Expected terminator to be automatically inserted");
//    ASSERT_MSG(tok.val.ident == internNewline, "Expected terminator to set it's value the the character that spawned it");
    ASSERT_TOKEN_POS(40, 4, 1); // }
    ASSERT_TOKEN_EOF();

    lexer_init(&lexer, "\n#import kai(\"core\")\n");
    lexer.client.onname = (OnNameFunc) (void *) lexer_onstr_calloc;
    lexer.client.onstr = lexer_onstr_calloc;
    ASSERT_TOKEN_KIND(TK_Terminator); // \n
    ASSERT_TOKEN_DIRECTIVE("import");
    ASSERT_TOKEN_IDENT("kai");
    ASSERT_TOKEN_KIND(TK_Lparen);
    ASSERT_TOKEN_STRING("core");
    ASSERT_TOKEN_KIND(TK_Rparen);
    ASSERT_TOKEN_KIND(TK_Terminator);
    ASSERT_TOKEN_EOF();
}
#endif
