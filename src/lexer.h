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

typedef enum TokenKind {
#define FOR_EACH(e, s) TK_##e,
    TOKEN_KINDS
#undef FOR_EACH
    NUM_TOKEN_KINDS,
} TokenKind;

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

