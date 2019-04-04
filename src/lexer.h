
typedef u8 TokenKind;
enum Enum_TokenKind {
    TK_Eof,
    TK_Comment,
    TK_Ident,
    TK_Directive,
    TK_Int,
    TK_Float,
    TK_String,
    TK_Add,
    TK_Sub,
    TK_Mul,
    TK_Div,
    TK_Rem,
    TK_And,
    TK_Or,
    TK_Xor,
    TK_Shl,
    TK_Shr,
    TK_AddAssign,
    TK_SubAssign,
    TK_MulAssign,
    TK_DivAssign,
    TK_RemAssign,
    TK_AndAssign,
    TK_OrAssign,
    TK_XorAssign,
    TK_ShlAssign,
    TK_ShrAssign,
    TK_Land,
    TK_Lor,
    TK_Lss,
    TK_Gtr,
    TK_Not,
    TK_BNot,
    TK_Eql,
    TK_Neq,
    TK_Leq,
    TK_Geq,
    TK_Assign,
    TK_Ellipsis,
    TK_Dollar,
    TK_Question,
    TK_RetArrow,
    TK_Lparen,
    TK_Lbrack,
    TK_Lbrace,
    TK_Rparen,
    TK_Rbrack,
    TK_Rbrace,
    TK_Comma,
    TK_Dot,
    TK_Colon,
    TK_Terminator,
    TK_Keyword,
    NUM_TOKEN_KINDS,
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

const char *DescribeTokenKind(TokenKind tk);
const char *DescribeToken(Token tok);

extern const char *internCallConv_C;
