
typedef u8 TokenKind;
enum Enum_TokenKind {
    TK_Invalid = 0,
    TK_Eof = 127,
    TK_Ident = 'n',
    TK_Keyword = 'k',
    TK_Directive = '#',
    TK_Int = 'i',
    TK_Float = 'f',
    TK_String = 's',
    TK_Ellipsis = 'e',
    TK_RetArrow = 'r',

    TK_Comment = 10,

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

    TK_Lss       = '<',
    TK_Gtr       = '>',
    TK_Eql       = TK_Assign | 0x80,
    TK_Neq       = TK_Not    | 0x80,
    TK_Leq       = TK_Lss    | 0x80,
    TK_Geq       = TK_Gtr    | 0x80,
    TK_Land = 'a',
    TK_Lor  = 'o',
    NUM_TOKEN_KINDS = 255,
};

STATIC_ASSERT(NUM_TOKEN_KINDS <= UINT8_MAX, "enum values overflow storage type");

#define TokenAssignOffset(Kind) Kind - (TK_AddAssign - TK_Add)

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

    Package *package;

    Position pos;

    b8 insertSemi;
    b8 insertSemiBeforeLBrace;
};

typedef struct Tok2 Tok2;
typedef struct Lex2 Lex2;
typedef struct LexerClient LexerClient;

typedef const char *(*OnStrFunc)  (void *userdata, Tok2 *tok, const char *str, u32 len, bool is_temp);
typedef const char *(*OnNameFunc) (void *userdata, Tok2 *tok, const char *str, u32 len);
typedef void  (*OnLineFunc)    (void *userdata, Pos pos);
typedef void  (*OnMsgFunc)     (void *userdata, Pos pos, const char *str, u32 len);
typedef void  (*OnCommentFunc) (void *userdata, Pos pos, const char *str, u32 len);

struct Tok2 {
    TokenKind kind;
//    TokenFlags flags;
    Range range;
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

const char *DescribeTokenKind(TokenKind tk);
const char *DescribeToken(Token tok);

extern const char *internCallConv_C;
