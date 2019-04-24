
#include "all.h"
#include "lexer.h"
#include "utf.h"

static void lexer_donothing() {}

void lexer_init(Lexer *self, const char *data) {
    TRACE(LEXING);
    self->start = data;
    self->str = data;
    self->tok = (Token){0};
    self->client = (LexerClient){
        .data = NULL,
        .online = (OnLineFunc) (void *) lexer_donothing,
        .onstr = (OnStrFunc) (void *) lexer_donothing,
        .onname = (OnNameFunc) (void *) lexer_donothing,
        .onmsg = (OnMsgFunc) (void *) lexer_donothing,
        .oncomment = NULL,
    };
}

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

u8 char_to_digit[256] = {
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

char escape_to_char[256] = {
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

INLINE
void lexer_match_case3(Lexer *self, u8 t1, u8 c2, u8 t2, u8 c3, u8 t3) {
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

INLINE
void lexer_match_shift(Lexer *self, u8 t1, u8 t2, u8 teq1, u8 teq2) {
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

u32 scan_numeric_escape(Lexer *self, i32 n, u32 max) {
    u32 x = 0;
    for (; n > 0; n--) {
        if (!*self->str) goto error;
        bool valid = valid_char_digit[*self->str];
        int digit = char_to_digit[*self->str++];
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

const char *scan_string(Lexer *self) {
    TRACE(LEXING);
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
                has_escape = true;
                u32 len = (u32) (self->str - start - 2);
                arrsetlen(self->string_temp_buffer, len);
                strncpy(self->string_temp_buffer, start + 1, len);
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
                    val = escape_to_char[rune];
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
            size_t temp_buffer_len = arrlen(self->string_temp_buffer);
            arrsetcap(self->string_temp_buffer, temp_buffer_len + 4);
            u32 len = EncodeCodePoint(self->string_temp_buffer + temp_buffer_len, rune);
            arrsetlen(self->string_temp_buffer, arrlen(self->string_temp_buffer) + len);
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
    u32 len = (u32) (has_escape ? arrlen(self->string_temp_buffer) : (self->str - start - 2));
    const char *str = has_escape ? self->string_temp_buffer : start + 1;
    return self->client.onstr(self->client.data, &self->tok, str, len, has_escape);
}

double scan_float(Lexer *self) {
    TRACE(LEXING);
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

u64 scan_int(Lexer *self) {
    TRACE(LEXING);
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
        int digit = char_to_digit[*str];
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

Token lexer_next_token(Lexer *self) {
    TRACE(LEXING);
repeat:;
    self->tok.offset_start = (u32) (self->str - self->start);

    switch (*self->str) {
        default: {
            self->tok.kind = TK_Name;
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
            self->client.online(self->client.data, (u32) (self->str - self->start));
            self->str++;
            goto repeat;
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

const char *token_names[] = {
    [TK_Invalid]    = "invalid",
    [TK_Eof]        = "EOF",
    [TK_Name]      = "name",
    [TK_Keyword]    = "keyword",
    [TK_Directive]  = "directive",
    [TK_Int]        = "int",
    [TK_Float]      = "float",
    [TK_String]     = "string",
    [TK_Ellipsis]   = "...",
    [TK_RetArrow]   = "->",
    [TK_Comment]    = "comment",
    [TK_Add]        = "+",
    [TK_Sub]        = "-",
    [TK_Mul]        = "*",
    [TK_Div]        = "/",
    [TK_Rem]        = "%",
    [TK_And]        = "&",
    [TK_Or]         = "|",
    [TK_Xor]        = "^",
    [TK_Shl]        = "_",
    [TK_Shr]        = "`",
    [TK_Not]        = "!",
    [TK_BNot]       = "~",
    [TK_Assign]     = "=",
    [TK_Dollar]     = "$",
    [TK_Question]   = "?",
    [TK_Lparen]     = "(",
    [TK_Lbrack]     = "[",
    [TK_Lbrace]     = "{",
    [TK_Rparen]     = ")",
    [TK_Rbrack]     = "]",
    [TK_Rbrace]     = "}",
    [TK_Comma]      = ",",
    [TK_Dot]        = ".",
    [TK_Colon]      = ":",
    [TK_Terminator] = ";",
    [TK_AddAssign]  = "+=",
    [TK_SubAssign]  = "-=",
    [TK_MulAssign]  = "*=",
    [TK_DivAssign]  = "/=",
    [TK_RemAssign]  = "%=",
    [TK_AndAssign]  = "&=",
    [TK_OrAssign]   = "|=",
    [TK_XorAssign]  = "^=",
    [TK_ShlAssign]  = "<<=",
    [TK_ShrAssign]  = ">>=",
    [TK_Lss]        = "<",
    [TK_Gtr]        = ">",
    [TK_Eql]        = "==",
    [TK_Neq]        = "!=",
    [TK_Leq]        = "<=",
    [TK_Geq]        = ">=",
    [TK_Land]       = "&&",
    [TK_Lor]        = "||",
};

const char *token_name(TokenKind kind) {
    ASSERT(token_names[kind] != NULL);
    return token_names[kind];
}

const char *token_info(Token tok) {
    if (tok.kind == TK_Name || tok.kind == TK_Keyword) return tok.tname;
    return token_name(tok.kind);
}
