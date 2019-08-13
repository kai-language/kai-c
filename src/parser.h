#pragma once

// Requires
// lexer.h

// ast.h
typedef struct Stmt Stmt;

// package.h
typedef struct Package Package;
typedef struct Source Source;

typedef enum Keyword Keyword;
enum Keyword {
    KW_NONE,
    KW_FN,
    KW_FOR,
    KW_NIL,
    KW_ENUM,
    KW_UNION,
    KW_STRUCT,
    KW_IF,
    KW_ELSE,
    KW_SWITCH,
    KW_CASE,
    KW_USING,
    KW_DEFER,
    KW_CAST,
    KW_AUTOCAST,
    KW_GOTO,
    KW_BREAK,
    KW_RETURN,
    KW_CONTINUE,
    KW_FALLTHROUGH,
    NUM_KEYWORDS,
};

typedef struct Parser Parser;
struct Parser {
    Package *package;
    Source *source;

    Lexer lexer;
    Token tok;

    u32 ostart; // current token start offset
    u32 oend;   // current token end offset
    u32 olast;  // previous token end offset

    u32 str_len; // set in parser_onstr
    u32 str_mapped; // set in parser_onstr

    i32 expr_level; // < 0: in control clause, >= 0: in expression

    bool in_control_stmt;
    bool allow_multi;
    bool was_terminator;
    bool was_newline;
    bool was_error_in_line;
    const char *calling_conv;
    const char *link_prefix;

    Stmt **stmts; // arr
};

void parser_init_interns(void);
void parse_package(Package *package);
