
typedef struct Tok2 Tok2;
typedef struct Lex2 Lex2;
typedef struct LexerClient LexerClient;

typedef const char *(*OnStrFunc)  (void *userdata, Tok2 *tok, const char *str, u32 len, bool is_temp);
typedef const char *(*OnNameFunc) (void *userdata, Tok2 *tok, const char *str, u32 len);
typedef void  (*OnLineFunc)    (void *userdata, u32 offset);
typedef void  (*OnMsgFunc)     (void *userdata, u32 offset, const char *str, u32 len);
typedef void  (*OnCommentFunc) (void *userdata, u32 offset, const char *str, u32 len);

struct Tok2 {
    TokenKind kind;
    u32 offset_start;
    u32 offset_end;
    union {
        const char *tname;
        const char *tstr;
        u64 tint;
        f64 tfloat;
    };
};

struct LexerClient {
    void *data;
    OnLineFunc    online;
    OnStrFunc     onstr;
    OnNameFunc    onname;
    OnMsgFunc     onmsg;
    OnCommentFunc oncomment;
};

struct Lex2 {
    const char *str;
    const char *start;

    Tok2 tok;
    DynamicArray(char) string_temp_buffer;

    LexerClient client;
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

u32 scan_numeric_escape(Lex2 *self, i32 n, u32 max);
const char *scan_string(Lex2 *self);
double scan_float(Lex2 *self);
u64 scan_int(Lex2 *self);
Inline void lexer_match_shift(Lex2 *self, u8 t1, u8 t2, u8 teq1, u8 teq2);
Inline void lexer_match_case3(Lex2 *self, u8 t1, u8 c2, u8 t2, u8 c3, u8 t3);

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

void lexer_fast_forward(Lex2 *self, u32 offset) {
    self->str += offset;
}

Tok2 lexer_next_token(Lex2 *self) {
repeat:;
    self->tok.offset_start = (u32) (self->str - self->start);

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
            const char *start = self->start + self->tok.offset_start;
            self->tok.offset_end = (u32) (self->str - self->start);
            u32 len = self->tok.offset_end - self->tok.offset_start;
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
            const char *start = self->start + self->tok.offset_start + 1;
            self->tok.offset_end = (u32) (self->str - self->start);
            u32 len = self->tok.offset_end - self->tok.offset_start - 1;
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
            self->str = self->start + self->tok.offset_start;
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
                    u32 start = self->tok.offset_start;
                    u32 len = (u32) (self->str - self->tok.offset_start);
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
    self->tok.offset_end = (u32) (self->str - self->start);
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
    self->client.onmsg(self->client.data, (u32) (self->str - self->start), msg, sizeof msg);
    return 0;
}

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
            self->client.online(self->client.data, (u32) (self->str - self->start));
            char msg[] = "Regular string literal cannot contain a newline";
            self->client.onmsg(self->client.data, (u32) (self->str - self->start), msg, sizeof msg);
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
                        self->client.onmsg(self->client.data, (u32) (self->str - self->start - width), msg, len);
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
        self->client.onmsg(self->client.data, self->tok.offset_start, msg, sizeof msg);
        return NULL;
    }
    ASSERT(*self->str == quote);
    self->str++;
    self->tok.offset_end = (u32) (self->str - self->start);
    u32 len = (u32) (has_escape ? ArrayLen(self->string_temp_buffer) : (self->str - start - 2));
    const char *str = has_escape ? self->string_temp_buffer : start + 1;
    return self->client.onstr(self->client.data, &self->tok, str, len, has_escape);
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
            self->client.onmsg(self->client.data, self->tok.offset_start, msg, sizeof msg);
        }
        while (isdigit(*str)) str++;
    }
    double val = strtod(self->str, NULL);
    if (val == HUGE_VAL) {
        char msg[] = "Float literal is larger than maximum allowed value";
        self->client.onmsg(self->client.data, self->tok.offset_start, msg, sizeof msg);
        val = 0.f;
    }
    self->str = str;
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
            self->client.onmsg(self->client.data, self->tok.offset_start, msg, len);
            while (isdigit(*str)) str++;
            val = 0;
            break;
        }
        if (val > (ULLONG_MAX - digit) / base) {
            char msg[] = "Integer literal is larger than maximum allowed value";
            self->client.onmsg(self->client.data, self->tok.offset_start, msg, sizeof msg);
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

#if TEST
const char *lexer_onstr_calloc(void *userdata, Tok2 *tok, const char *str, u32 len, bool is_temp) {
    char *out = calloc(len + 1, sizeof *out);
    strncpy(out, str, len);
    return out;
}

void test_lexer2() {

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
ASSERT_MSG_VA(tok.offset_start == OFFSET, \
"Expected token at position %d:%d (offset %d) got (offset %d)", \
LINE, COLUMN, OFFSET, tok.offset_start); \

    Tok2 tok;
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
