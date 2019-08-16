#pragma once

// Requires nothing

typedef enum TokenKind TokenKind;
enum TokenKind {
    TK_Invalid   = 0,
    TK_Eof       = 127,
    TK_Name     = 'n',
    TK_Keyword   = 'k',
    TK_Directive = '#',
    TK_Int       = 'i',
    TK_Float     = 'f',
    TK_String    = 's',
    TK_Ellipsis  = 'e',
    TK_RetArrow  = 'r',
    TK_Comment   = 10,

    TK_Add        = '+',
    TK_Sub        = '-',
    TK_Mul        = '*',
    TK_Div        = '/',
    TK_Rem        = '%',
    TK_And        = '&',
    TK_Or         = '|',
    TK_Xor        = '^',
    TK_Shl        = '_',
    TK_Shr        = '`',
    TK_Not        = '!',
    TK_BNot       = '~',
    TK_Assign     = '=',
    TK_Dollar     = '$',
    TK_Question   = '?',
    TK_Lparen     = '(',
    TK_Lbrack     = '[',
    TK_Lbrace     = '{',
    TK_Rparen     = ')',
    TK_Rbrack     = ']',
    TK_Rbrace     = '}',
    TK_Comma      = ',',
    TK_Dot        = '.',
    TK_Colon      = ':',
    TK_Terminator = ';',

    TK_Land      = 'a',
    TK_Lor       = 'o',
    TK_Lss       = '<',
    TK_Gtr       = '>',
    TK_Eql       = TK_Assign | 0x80,
    TK_Neq       = TK_Not    | 0x80,
    TK_Leq       = TK_Lss    | 0x80,
    TK_Geq       = TK_Gtr    | 0x80,

    TK_AddAssign = TK_Add    | 0x80,
    TK_SubAssign = TK_Sub    | 0x80,
    TK_MulAssign = TK_Mul    | 0x80,
    TK_DivAssign = TK_Div    | 0x80,
    TK_RemAssign = TK_Rem    | 0x80,
    TK_AndAssign = TK_And    | 0x80,
    TK_OrAssign  = TK_Or     | 0x80,
    TK_XorAssign = TK_Xor    | 0x80,
    TK_ShlAssign = TK_Shl    | 0x80,
    TK_ShrAssign = TK_Shr    | 0x80,
};

typedef struct Token Token;
struct Token {
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

typedef const char *(*OnStrFunc)  (void *userdata, Token *tok, const char *str, u32 len, bool is_temp);
typedef const char *(*OnNameFunc) (void *userdata, Token *tok, const char *str, u32 len);
typedef void  (*OnLineFunc)    (void *userdata, u32 offset);
typedef void  (*OnMsgFunc)     (void *userdata, u32 offset, const char *str, u32 len);
typedef void  (*OnCommentFunc) (void *userdata, u32 offset, const char *str, u32 len);

typedef struct LexerClient LexerClient;
struct LexerClient {
    void *data;
    OnLineFunc    online;
    OnStrFunc     onstr;
    OnNameFunc    onname;
    OnMsgFunc     onmsg;
    OnCommentFunc oncomment;
};

typedef struct Lexer Lexer;
struct Lexer {
    const char *str;
    const char *start;

    Token tok;
    char *string_temp_buffer;

    LexerClient client;
};

void lexer_init(Lexer *self, const char *data);
Token lexer_next_token(Lexer *self);
const char *token_name(TokenKind kind);
const char *token_info(Token tok);
