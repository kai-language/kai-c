
#include "all.h"
#include "os.h"
#include "lexer.h"
#include "parser.h"
#include "arena.h"
#include "package.h"
#include "ast.h"
#include "checker.h"
#include "ast.h"
#include "string.h"
#include "checker.h"
#include "queue.h"
#include "compiler.h"

#define error(self, range, fmt, ...) add_error(self->package, range, fmt, ##__VA_ARGS__)
#define note(self, range, fmt, ...) add_note(self->package, range, fmt, ##__VA_ARGS__)

void parse_source(Package *package, Source *source);
Stmt *parse_stmt(Parser *self);
Expr *parse_expr(Parser *self);
const char *name_for_import(const char *in_path);

const char *intern_in;
const char *keywords[] = {
    [KW_IF] = "if",
    [KW_FN] = "fn",
    [KW_FOR] = "for",
    [KW_NIL] = "nil",
    [KW_ENUM] = "enum",
    [KW_UNION] = "union",
    [KW_STRUCT] = "struct",
    [KW_ELSE] = "else",
    [KW_CASE] = "case",
    [KW_USING] = "using",
    [KW_DEFER] = "defer",
    [KW_CAST] = "cast",
    [KW_AUTOCAST] = "autocast",
    [KW_GOTO] = "goto",
    [KW_BREAK] = "break",
    [KW_RETURN] = "return",
    [KW_SWITCH] = "switch",
    [KW_CONTINUE] = "continue",
    [KW_FALLTHROUGH] = "fallthrough",
};

const char *directives[] = {
    [DIR_FLAGS] = "flags",
    [DIR_CVARGS] = "cvargs",
    [DIR_LOCATION] = "location",
    [DIR_FILE] = "file",
    [DIR_FUNCTION] = "function",
    [DIR_LINE] = "line",
    [DIR_IMPORT] = "import",
    [DIR_FOREIGN] = "foreign",
    [DIR_CALLCONV] = "callconv",
    [DIR_LINKNAME] = "linkname",
    [DIR_LINKPREFIX] = "linkprefix",
};

i32 token_precedence[255] = {
    [TK_Question] = 1,
    [TK_Lor] = 1,
    [TK_Land] = 2,
    [TK_Eql] = 3,
    [TK_Neq] = 3,
    [TK_Lss] = 3,
    [TK_Leq] = 3,
    [TK_Gtr] = 3,
    [TK_Geq] = 3,
    [TK_Add] = 4,
    [TK_Sub] = 4,
    [TK_Or]  = 4,
    [TK_Xor] = 4,
    [TK_Mul] = 5,
    [TK_Div] = 5,
    [TK_Rem] = 5,
    [TK_Shl] = 5,
    [TK_Shr] = 5,
    [TK_And] = 5,
    [TK_Assign] = 0,
};

void parser_online(Parser *self, u32 offset) {
    TRACE(PARSING);
    self->was_newline = true;
    arrput(self->source->line_offsets, offset);
}

const char *parser_onname(Parser *self, Token *tok, const char *str, u32 len) {
    TRACE(PARSING);
    str = str_intern_range(str, str + len);
    int num_keywords = sizeof keywords / sizeof *keywords;
    for (int i = 0; i < num_keywords; i++) {
        if (keywords[i] == str) {
            tok->kind = TK_Keyword;
            break;
        }
    }
    return str;
}

const char *parser_onstr(Parser *self, Token *tok, const char *str, u32 len, bool is_temp) {
    TRACE(PARSING);
    self->str_len = len;
    self->str_mapped = !is_temp;
    if (!is_temp) return str;
    char *mem = arena_alloc(&self->package->arena, len + 1);
    memcpy(mem, str, len);
    mem[len] = '\0';
    return mem;
}

void parser_onmsg(Parser *self, u32 offset, const char *str, u32 len) {
    TRACE(PARSING);
    Range range = {offset, offset};
    error(self, range, str);
}

void parse_package(Package *package) {
    TRACE1(PARSING, STR("package.path", package->path));
    package_read_source_files(package);
    for (int i = 0; i < arrlen(package->sources); i++) {
        verbose("Parsing %s", package->sources[i].path);
        parse_source(package, package->sources + i);
    }
    for (int i = 0; i < hmlen(package->imports); i++) {
        ImportMapEntry import = package->imports[i];
        const char *path = resolve_value(package, import.key->dimport.path).p;
        import.value->name = name_for_import(path);
        verbose("Resolved name '%s' for import path '%s'", import.value->name, path);
        import_package(path, package);
    }
}

INLINE
Token eat_tok(Parser *self) {
    TRACE(PARSING);
    self->olast = self->oend;
    self->tok = lexer_next_token(&self->lexer);
    self->ostart = self->source->start + self->tok.offset_start;
    self->oend = self->source->start + self->tok.offset_end;
    return self->tok;
}

INLINE
Range parser_range(Parser *self) {
    Range range = {self->ostart, self->oend};
    return range;
}

INLINE
Range r(u32 start, u32 end) {
    Range range = {start, end};
    return range;
}

INLINE
bool is_tok(Parser *self, TokenKind kind) {
    return self->tok.kind == kind;
}

INLINE
bool is_eof(Parser *self) {
    return self->tok.kind == TK_Eof;
}

INLINE
bool is_tok_name(Parser *self, const char *name) {
    return self->tok.kind == TK_Name && self->tok.tname == name;
}

INLINE
bool is_kw(Parser *self, Keyword keyword) {
    return is_tok(self, TK_Keyword) && self->tok.tname == keywords[keyword];
}

INLINE
bool is_directive(Parser *self, Directive directive) {
    return is_tok(self, TK_Directive) && self->tok.tname == directives[directive];
}

INLINE
bool match_kw(Parser *self, Keyword keyword) {
    if (!is_kw(self, keyword)) return false;
    eat_tok(self);
    return true;
}

INLINE
bool match_directive(Parser *self, Directive directive) {
    if (!is_directive(self, directive)) return false;
    eat_tok(self);
    return true;
}

INLINE
bool match_tok(Parser *self, TokenKind kind) {
    if (!is_tok(self, kind)) return false;
    eat_tok(self);
    return true;
}

INLINE
bool expect_tok(Parser *self, TokenKind kind) {
    if (!is_tok(self, kind)) {
        error(self, parser_range(self), "Expected token %s, got %s",
              token_name(kind), token_info(self->tok));
        return false;
    }
    eat_tok(self);
    return true;
}

INLINE
bool is_terminator(Parser *self) {
    if (self->was_newline) return true;
    if (is_eof(self)) return true;
    if (is_tok(self, TK_Rbrace)) return true;
    if (is_tok(self, TK_Terminator)) return true;
    return false;
}

INLINE
bool match_terminator(Parser *self) {
    if (is_eof(self)) return true;
    return match_tok(self, TK_Terminator);
}

INLINE
bool expect_terminator(Parser *self) {
    if (is_tok(self, TK_Terminator)) {
        self->tok = lexer_next_token(&self->lexer);
        return true;
    }
    if (self->was_newline) {
        self->was_newline = false;
        return true;
    }
    if (is_tok(self, TK_Rbrace)) return true;
    if (is_tok(self, TK_Eof)) return true;
    if (is_kw(self, KW_ELSE)) return true;
    error(self, parser_range(self), "Expected ';' or newline");
    return false;
}

void parse_source(Package *package, Source *source) {
    TRACE(PARSING);
    Parser parser = {
        .package = package,
        .source = source,
    };
    lexer_init(&parser.lexer, source->code);
    parser.lexer.client.data = &parser;
    parser.lexer.client.online = (void *) parser_online;
    parser.lexer.client.onname = (void *) parser_onname;
    parser.lexer.client.onstr  = (void *) parser_onstr;
    parser.lexer.client.onmsg  = (void *) parser_onmsg;
    eat_tok(&parser);
    while (!is_eof(&parser)) {
        Stmt *stmt = parse_stmt(&parser);
        arrput(package->stmts, stmt);
    }
}

void parser_error(Parser *self, Range range, const char *msg, ...) {
    va_list args;
    char msg_buffer[512];
    va_start(args, msg);
    vsnprintf(msg_buffer, sizeof msg_buffer, msg, args);
    va_end(args);

    char err_buffer[512];

    PosInfo start = package_posinfo(self->package, range.start);
    int len = snprintf(err_buffer, sizeof err_buffer, "ERROR(%s:%u:%u): %s\n",
                       self->source->name, start.line, start.column, msg_buffer);

    len = MIN(len, sizeof err_buffer);
    char *err_msg = arena_alloc(&self->package->arena, len + 1);
    memcpy(err_msg, err_buffer, len + 1);

//    const char *code_block = NULL;
    // TODO: findCodeBlockAndHighlightError with the new lexer

//    DiagnosticError error = {err_msg, code_block};
//    ArrayPush(self->package->diagnostics.errors, error);
}

INLINE
void expect_single_expr(Parser *self, Expr **exprs) {
    TRACE(PARSING);
    if (arrlen(exprs) > 1) {
        error(self, r(exprs[1]->range.start, arrlast(exprs)->range.end),
              "Unsupported multiple values");
    }
}

INLINE
void expect_expr_is_name(Parser *self, Expr *expr) {
    TRACE(PARSING);
    if (expr->kind != EXPR_NAME) {
        error(self, expr->range, "Expected identifier");
    }
}

INLINE
void expect_exprs_are_name(Parser *self, Expr **exprs) {
    TRACE(PARSING);
    for (int i = 0; i < arrlen(exprs); i++)
        expect_expr_is_name(self, exprs[i]);
}

const char *name_for_import(const char *in_path) {
    ASSERT(in_path);
    char path[MAX_PATH];
    char name[MAX_NAME];
    path_copy(path, in_path);
    char *last_component = path_file(path);
    strncpy(name, last_component, MAX_NAME);
    name[MAX_NAME - 1] = 0;
    char *ext = path_ext(name);
    if (ext != name) *ext = '\0';
    return str_intern(name);
}

Sym *parser_new_sym(Parser *self, Decl *decl, SymKind kind) {
    Sym *sym = arena_calloc(&self->package->arena, sizeof *sym);
    sym->owning_package = self->package;
    sym->decl = decl;
    return sym;
}

void parser_declare_and_queue_checking(Parser *self, Decl *decl) {
    TRACE(PARSING);
    switch (decl->kind) {
        case DECL_VAL: {
            Sym *sym = parser_new_sym(self, decl, SYM_VAL);
            sym->name = decl->dval.name->ename;
            scope_declare(self->package->scope, sym);
            break;
        }
        case DECL_VAR: {
            for (int i = 0; i < arrlen(decl->dvar.names); i++) {
                Sym *sym = parser_new_sym(self, decl, SYM_VAR);
                const char *name = decl->dvar.names[i]->ename;
                sym->name = name;
                scope_declare(self->package->scope, sym);
            }
            break;
        }
        case DECL_IMPORT: {
            Sym *sym = parser_new_sym(self, decl, SYM_PACKAGE);
            if (decl->dimport.alias) {
                sym->name = decl->dimport.alias->ename;
            }
            ImportMapEntry entry = {decl, sym};
            hmputs(self->package->imports, entry);
            break;
        }
        case DECL_FOREIGN: {
            Sym *sym = parser_new_sym(self, decl, decl->flags&DECL_CONSTANT ? SYM_VAL : SYM_VAR);
            sym->state = SYM_RESOLVED;
            sym->name = decl->dforeign.name->ename;
            scope_declare(self->package->scope, sym);
            break;
        }
        case DECL_FOREIGNBLOCK: {
            fatal("Parser should call parser_declare for each member of the block");
        }
        default:
            break;
    }
    CheckerWork *work = arena_alloc(&self->package->arena, sizeof *work);
    work->package = self->package;
    work->stmt = (Stmt *) decl;
    queue_push_back(&compiler.checking_queue, work);
}

CompoundField parse_compound_field(Parser *self) {
    TRACE(PARSING);
    CompoundField field = {0};
    if (match_tok(self, TK_Lbrack)) {
        field.kind = FIELD_INDEX;
        field.key = parse_expr(self);
        expect_tok(self, TK_Rbrack);
        expect_tok(self, TK_Colon);
        field.val = parse_expr(self);
        return field;
    }
    field.val = parse_expr(self);
    if (match_tok(self, TK_Colon)) {
        field.key = field.val;
        expect_expr_is_name(self, field.key); // TODO: Support deep init { ._union.ptr = x }
        field.val = parse_expr(self);
    }
    return field;
}

typedef Expr *(*ParseExprFn) (Parser *self);
Expr **parse_expr_list(Parser *self, ParseExprFn fn) {
    TRACE(PARSING);
    Expr **exprs = NULL;
    do {
        Expr *expr = fn(self);
        arrput(exprs, expr);
    } while (match_tok(self, TK_Comma));
    return exprs;
}

FuncParam *parse_single_param_or_many_with_same_type(Parser *self) {
    TRACE(PARSING);
    FuncParam *params = NULL;
    bool named_params = false;
    bool mixed = false;
    do {
        if (is_tok(self, TK_Rparen)) break;
        Expr **exprs = parse_expr_list(self, parse_expr);
        if (match_tok(self, TK_Colon)) {
            named_params = true;
            Expr *type = parse_expr(self);
            for (int i = 0; i < arrlen(exprs); i++) {
                expect_expr_is_name(self, exprs[i]);
                FuncParam param = {exprs[i], type};
                arrput(params, param);
            }
            arrfree(exprs);
            continue;
        }
        if (named_params && !mixed) {
            mixed = true;
            error(self, parser_range(self),
                  "Mixture of named and unnamed parameters is ambiguous");
            note(self, parser_range(self), "Use '_:' instead");
        }
        for (int i = 0; i < arrlen(exprs); i++) {
            FuncParam param = {NULL, exprs[i]};
            arrput(params, param);
        }
        arrfree(exprs);
    } while (match_tok(self, TK_Comma));
    return params;
}

FuncParam *parse_params(Parser *self, FuncFlags *flags) {
    TRACE(PARSING);
    FuncParam *params = NULL;
    for (;;) {
        if (is_tok(self, TK_Rparen) || is_eof(self)) break;
        FuncParam *some = parse_single_param_or_many_with_same_type(self);
        for (int i = 0; i < arrlen(some); i++) {
            arrput(params, some[i]);
        }
        arrfree(some);
        if (match_tok(self, TK_Ellipsis)) {
            *flags |= FUNC_VARGS;
            if (match_directive(self, DIR_CVARGS)) *flags |= FUNC_CVARGS;
            break;
        }
    }
    return params;
}

FuncParam *parse_result(Parser *self) {
    TRACE(PARSING);
    FuncParam *params = NULL;
    FuncParam param;
    if (match_tok(self, TK_Lparen)) {
        do {
            param.name = NULL;
            if (is_tok(self, TK_Rparen)) break;
            param.type = parse_expr(self);
            if (match_tok(self, TK_Colon)) {
                param.name = param.type;
                expect_expr_is_name(self, param.name);
                param.type = parse_expr(self);
            }
            arrput(params, param);
        } while (match_tok(self, TK_Comma));
        expect_tok(self, TK_Rparen);
        return params;
    }
    param.type = parse_expr(self);
    arrput(params, param);
    return params;
}

Expr *parse_function_type(Parser *self) {
    TRACE(PARSING);
    ASSERT(is_kw(self, KW_FN));
    u32 start = self->ostart;
    eat_tok(self);
    FuncParam *params = NULL;
    FuncParam *result = NULL;
    FuncFlags flags = FUNC_NONE;
    if (match_tok(self, TK_Lparen)) {
        params = parse_params(self, &flags);
        expect_tok(self, TK_Rparen);
    }
    if (match_tok(self, TK_RetArrow)) {
        i32 prev_expr_level = self->expr_level;
        self->expr_level = -1;
        result = parse_result(self);
        self->expr_level = prev_expr_level;
    }
    return new_expr_functype(self->package, r(start, self->olast), flags, params, result);
}

Expr *parse_name(Parser *self) {
    TRACE(PARSING);
    const char *ident = self->tok.tname;
    Range range = parser_range(self);
    expect_tok(self, TK_Name);
    return new_expr_name(self->package, range, ident);
}

Expr *parse_str(Parser *self) {
    TRACE(PARSING);
    Range range = parser_range(self);
    Expr *expr = new_expr_str(self->package, range, self->tok.tstr, self->str_len, self->str_mapped);
    expect_tok(self, TK_String);
    return expr;
}

Expr *parse_expr_atom(Parser *self) {
    TRACE(PARSING);
    u32 start = self->ostart;
    switch (self->tok.kind) {
        case TK_Name: {
            Expr *e = new_expr_name(self->package, parser_range(self), self->tok.tname);
            eat_tok(self);
            if (match_tok(self, TK_Dot)) { // name.name
                Expr *name = parse_name(self);
                return new_expr_field(self->package, r(start, name->range.end), e, name);
            }
            return e;
        }
        case TK_Int: {
            Expr *expr = new_expr_int(self->package, parser_range(self), self->tok.tint);
            eat_tok(self);
            return expr;
        }
        case TK_Float: {
            Expr *expr = new_expr_float(self->package, parser_range(self), self->tok.tfloat);
            eat_tok(self);
            return expr;
        }
        case TK_String: {
            Range range = parser_range(self);
            Expr *expr = new_expr_str(self->package, range, self->tok.tstr, self->str_len, self->str_mapped);
            eat_tok(self);
            return expr;
        }
        case TK_Lparen: {
            eat_tok(self);
            Expr *expr = parse_expr(self);
            expect_tok(self, TK_Rparen);
            return new_expr_paren(self->package, r(start, self->olast), expr);
        }
        case TK_Lbrack: {
            eat_tok(self);
            if (match_tok(self, TK_Rbrack)) {
                Expr *type = parse_expr(self);
                return new_expr_slicetype(self->package, r(start, type->range.end), type);
            }
            Expr *length = NULL;
            if (!match_tok(self, TK_Ellipsis)) length = parse_expr(self);
            expect_tok(self, TK_Rbrack);
            Expr *type = parse_expr(self);
            return new_expr_array(self->package, r(start, type->range.end), type, length);
        }
        case TK_Lbrace: {
            eat_tok(self);
            CompoundField *fields = NULL;
            if (!is_tok(self, TK_Rbrace)) {
                CompoundField field = parse_compound_field(self);
                arrput(fields, field);
                while (match_tok(self, TK_Comma)) {
                    if (is_tok(self, TK_Rbrace)) break;
                    field = parse_compound_field(self);
                    arrput(fields, field);
                }
            }
            expect_tok(self, TK_Rbrace);
            return new_expr_compound(self->package, r(start, self->olast), NULL, fields);
        }
        case TK_Dollar: {
            fatal("Unsupported polymorphism");
        }
        case TK_Mul: {
            eat_tok(self);
            Expr *type = parse_expr(self);
            return new_expr_pointer(self->package, r(start, type->range.end), type);
        }
        case TK_Directive: {
            const char *name = self->tok.tname;
            eat_tok(self);
            error(self, r(start, self->olast), "Unexpected directive '%s'", name);
            return new_ast_invalid(self->package, r(start, self->olast));
        }
        case TK_Keyword: {
            const char *name = self->tok.tname;
            if (name == keywords[KW_FN]) {
                return parse_function_type(self);
            } else if (name == keywords[KW_STRUCT]) {
                goto case_struct;
            } else if (name == keywords[KW_UNION]) {
                goto case_union;
            } else if (name == keywords[KW_ENUM]) {
                goto case_enum;
            } else if (name == keywords[KW_NIL]) {
                eat_tok(self);
                return new_expr_nil(self->package, r(start, self->olast));
            }
            eat_tok(self);
            error(self, r(start, self->olast), "Unexpected keyword '%s'", name);
            return new_ast_invalid(self->package, r(start, self->olast));
        }
        case_struct: {
            eat_tok(self);
            expect_tok(self, TK_Lbrace);
            AggregateField *fields = NULL;
            while (!is_tok(self, TK_Rbrace)) {
                Expr **names = parse_expr_list(self, parse_name);
                expect_tok(self, TK_Colon);
                Expr *type = parse_expr(self);
                AggregateField field = {names, type};
                arrput(fields, field);
                if (is_tok(self, TK_Rbrace)) break;
                match_tok(self, TK_Terminator);
                if (is_eof(self)) break;
            }
            expect_tok(self, TK_Rbrace);
            return new_expr_struct(self->package, r(start, self->olast), fields);
        }
        case_union: {
            eat_tok(self);
            expect_tok(self, TK_Lbrace);
            AggregateField *fields = NULL;
            while (!is_tok(self, TK_Rbrace)) {
                Expr **names = parse_expr_list(self, parse_name);
                expect_tok(self, TK_Colon);
                Expr *type = parse_expr(self);
                AggregateField field = {names, type};
                arrput(fields, field);
                if (is_tok(self, TK_Rbrace)) break;
                match_tok(self, TK_Terminator);
                if (is_eof(self)) break;
            }
            expect_tok(self, TK_Rbrace);
            return new_expr_union(self->package, r(start, self->olast), fields);
        }
        case_enum: {
            eat_tok(self);
            EnumFlags flags = 0;
            Expr *type = NULL;
            if (match_tok(self, TK_Lparen)) {
                type = parse_expr(self);
                expect_tok(self, TK_Rparen);
            }
            while (is_tok(self, TK_Directive) || is_eof(self)) {
                if (match_directive(self, DIR_FLAGS)) {
                    flags |= ENUM_FLAGS;
                } else {
                    error(self, parser_range(self),
                          "Invalid directive '#%s'", token_info(self->tok));
                    eat_tok(self);
                }
            }
            expect_tok(self, TK_Lbrace);
            EnumItem *items = NULL;
            if (!is_tok(self, TK_Rbrace)) {
                while (!is_tok(self, TK_Rbrace)) {
                    Expr *name = parse_name(self);
                    Expr *value = NULL;
                    if (match_tok(self, TK_Colon)) {
                        expect_tok(self, TK_Colon);
                        value = parse_expr(self);
                    }
                    EnumItem item = {name, value};
                    arrput(items, item);
                    match_tok(self, TK_Terminator);
                    if (is_eof(self)) break;
                }
            }
            expect_tok(self, TK_Rbrace);
            return new_expr_enum(self->package, r(start, self->olast), flags, type, items);
        }
        default: {
            error(self, r(start, self->oend),
                  "Unexpected token '%s'", token_info(self->tok));
        }
    }
    eat_tok(self);
    return new_ast_invalid(self->package, r(start, self->olast));
}

Expr *parse_expr_primary(Parser *self) {
    TRACE(PARSING);
    Expr *x = parse_expr_atom(self);
    for (;;) {
        u32 start = self->ostart;
        switch (self->tok.kind) {
            case TK_Dot: { // Field
                eat_tok(self);
                Expr *name = parse_name(self);
                x = new_expr_field(self->package, r(start, name->range.end), x, name);
                continue;
            }
            case TK_Lbrack: { // Index | Slice
                eat_tok(self);
                if (match_tok(self, TK_Colon)) {
                    if (match_tok(self, TK_Rbrack)) {
                        x = new_expr_slice(self->package, r(start, self->olast), x, NULL, NULL);
                        continue;
                    }
                    Expr *hi = parse_expr(self);
                    expect_tok(self, TK_Rbrack);
                    return new_expr_slice(self->package, r(start, self->olast), x, NULL, hi);
                }
                Expr *index = parse_expr(self);
                if (match_tok(self, TK_Colon)) {
                    if (match_tok(self, TK_Rbrack)) {
                        x = new_expr_slice(self->package, r(start, self->olast), x, index, NULL);
                        continue;
                    }
                    Expr *hi = parse_expr(self);
                    expect_tok(self, TK_Rbrack);
                    x = new_expr_slice(self->package, r(start, self->olast), x, index, hi);
                    continue;
                }
                expect_tok(self, TK_Rbrack);
                x = new_expr_index(self->package, r(start, self->olast), x, index);
                continue;
            }
            case TK_Lparen: { // Call
                eat_tok(self);
                CallArg *args = NULL;
                if (!is_tok(self, TK_Rparen)) {
                    CallArg arg = {0};
                    arg.expr = parse_expr(self);
                    if (is_tok(self, TK_Colon)) {
                        arg.name = arg.expr;
                        arg.expr = parse_expr(self);
                    }
                    arrput(args, arg);
                    while (match_tok(self, TK_Comma)) {
                        if (is_tok(self, TK_Rparen)) break; // allow trailing comma
                        arg.expr = parse_expr(self);
                        if (is_tok(self, TK_Colon)) {
                            arg.name = arg.expr;
                            arg.expr = parse_expr(self);
                        }
                        arrput(args, arg);
                    }
                }
                expect_tok(self, TK_Rparen);
                x = new_expr_call(self->package, r(start, self->olast), x, args);
                continue;
            }
            case TK_Lbrace: {
                if (x->kind == EXPR_FUNCTYPE) {
                    eat_tok(self);
                    Stmt **stmts = NULL;
                    while (!is_tok(self, TK_Rbrace) && !is_eof(self)) {
                        Stmt *stmt = parse_stmt(self);
                        expect_terminator(self);
                        arrput(stmts, stmt);
                    }
                    expect_tok(self, TK_Rbrace);
                    Stmt *body = new_stmt_block(self->package, r(start, self->olast), stmts);
                    x = new_expr_func(self->package, r(x->range.start, self->olast), 0, x, body);
                    continue;
                }
                if (self->expr_level < 0) return x;
                eat_tok(self);
                match_tok(self, TK_Terminator);
                CompoundField *fields = NULL;
                if (!is_tok(self, TK_Rbrace)) {
                    CompoundField field = parse_compound_field(self);
                    arrput(fields, field);
                    while (match_tok(self, TK_Comma)) {
                        if (is_tok(self, TK_Rbrace)) break;
                        field = parse_compound_field(self);
                        arrput(fields, field);
                    }
                }
                expect_tok(self, TK_Rbrace);
                x = new_expr_compound(self->package, r(start, self->olast), x, fields);
                continue;
            }
            default:
                return x;
        }
    }
}

Expr *parse_expr_unary(Parser *self) {
    TRACE(PARSING);
    u32 start = self->ostart;
    if (match_tok(self, TK_Mul)) {
        Expr *type = parse_expr(self);
        return new_expr_pointer(self->package, r(start, type->range.end), type);
    }
    switch (self->tok.kind) {
        case TK_Mul: {
            eat_tok(self);
            Expr *type = parse_expr(self);
            return new_expr_pointer(self->package, r(start, type->range.end), type);
        }
        case TK_Add: case TK_Sub: case TK_Not: case TK_BNot: case TK_Xor: case TK_And: case TK_Lss: {
            Op op = (Op) self->tok.kind;
            eat_tok(self);
            Expr *expr = parse_expr_unary(self);
            return new_expr_unary(self->package, r(start, expr->range.end), op, expr);
        }
        default:
            return parse_expr_primary(self);
    }
}

Expr *parse_expr_binary(Parser *self, i32 prec1) {
    TRACE(PARSING);
    Expr *lhs = parse_expr_unary(self);
    for (;;) {
        Token op = self->tok;
        i32 precedence = token_precedence[op.kind];
        if (precedence < prec1) return lhs;
        eat_tok(self);
        if (op.kind == TK_Question) {
            Expr *pass = NULL;
            if (!is_tok(self, TK_Colon)) {
                pass = parse_expr(self);
            }
            expect_tok(self, TK_Colon);
            Expr *fail = parse_expr(self);
            Range range = {lhs->range.start, fail->range.end};
            return new_expr_ternary(self->package, range, lhs, pass, fail);
        }
        Expr *rhs = parse_expr_binary(self, precedence + 1);
        Range range = {lhs->range.start, rhs->range.end};
        lhs = new_expr_binary(self->package, range, (Op) op.kind, lhs, rhs);
    }
}

INLINE
Expr *parse_expr(Parser *self) {
    TRACE(PARSING);
    return parse_expr_binary(self, 1);
}

Stmt *parse_simple_stmt(Parser *self) {
    TRACE(PARSING);
    u32 start = self->ostart;
    Expr **exprs = parse_expr_list(self, parse_expr);
    switch (self->tok.kind) {
        case TK_Assign: {
            eat_tok(self);
            Expr **rhs = parse_expr_list(self, parse_expr);
            return new_stmt_assign(self->package, r(start, arrlast(rhs)->range.end), exprs, rhs);
        }
        case TK_AddAssign: case TK_SubAssign: case TK_MulAssign: case TK_DivAssign:
        case TK_RemAssign: case TK_AndAssign: case TK_OrAssign:
        case TK_XorAssign: case TK_ShlAssign: case TK_ShrAssign: {
            Op op = (Op) (self->tok.kind & ~0x80);
            Expr **rhs = parse_expr_list(self, parse_expr);
            expect_single_expr(self, exprs);
            expect_single_expr(self, rhs);
            Range range = {exprs[0]->range.start, arrlast(rhs)->range.end};
            rhs[0] = new_expr_binary(self->package, range, op, exprs[0], rhs[0]);
            return new_stmt_assign(self->package, range, exprs, rhs);
        }
        case TK_Colon: {
            eat_tok(self);
            if (arrlen(exprs) == 1 && exprs[0]->kind == EXPR_NAME && match_terminator(self)) {
                Expr *name = exprs[0];
                arrfree(exprs);
                return new_stmt_label(self->package, r(start, self->olast), name);
            }
            expect_exprs_are_name(self, exprs);
            Expr *type = NULL;
            Expr **rhs = NULL;
            bool is_val = false;
            if (match_tok(self, TK_Assign)) { // :=
                rhs = parse_expr_list(self, parse_expr);
                goto ret;
            }
            if (match_tok(self, TK_Colon)) { // ::
                is_val = true;
                rhs = parse_expr_list(self, parse_expr);
                expect_single_expr(self, exprs);
                expect_single_expr(self, rhs);
                goto ret;
            }
            type = parse_expr(self);
            if (match_tok(self, TK_Assign)) { // : T =
                rhs = parse_expr_list(self, parse_expr);
                goto ret;
            }
            if (match_tok(self, TK_Colon)) { // : T :
                is_val = true;
                rhs = parse_expr_list(self, parse_expr);
                expect_single_expr(self, exprs);
                expect_single_expr(self, rhs);
                goto ret;
            }
        ret:;
            Range range = {start, rhs ? arrlast(rhs)->range.end : type->range.end};
            Decl *decl = is_val ?
                new_decl_val(self->package, range, exprs[0], type, rhs[0]) :
                new_decl_var(self->package, range, exprs, type, rhs);
            parser_declare_and_queue_checking(self, decl);
            return (Stmt *) decl;
        }
        default: {
            if (self->expr_level == -2 && is_tok_name(self, intern_in)) {
                Range range = {};
                return new_stmt_names(self->package, range, exprs);
            }
            expect_single_expr(self, exprs);
            return (Stmt *) exprs[0]; // Statement is an expression
        }
    }
}

Stmt *parse_stmt(Parser *self) {
    TRACE(PARSING);
begin:;
    u32 start = self->ostart;
    switch (self->tok.kind) {
        case_expr:
        case TK_Name: case TK_Int: case TK_Float: case TK_String: case TK_Lparen:
        case TK_Mul: case TK_Lbrack: case TK_Ellipsis: case TK_Dollar: // Beginning of Atom's
        case TK_Add: case TK_Sub: case TK_Not: case TK_BNot: case TK_Xor: case TK_And: case TK_Lss: // Unary's
        {
            Stmt *s = parse_simple_stmt(self);
            expect_terminator(self);
            return s;
        }
        case TK_Lbrace: {
            eat_tok(self);
            Stmt **stmts = NULL;
            while (!is_tok(self, TK_Rbrace) || is_eof(self)) {
                Stmt *stmt = parse_stmt(self);
                arrput(stmts, stmt);
            }
            expect_tok(self, TK_Rbrace);
            Range range = {start, self->olast};
            expect_terminator(self);
            return new_stmt_block(self->package, range, stmts);
        }
        case TK_Directive: {
            if (is_directive(self, DIR_FILE) || is_directive(self, DIR_LINE) ||
                is_directive(self, DIR_LOCATION) || is_directive(self, DIR_FUNCTION))
            {
                goto case_expr;
            }
            if (match_directive(self, DIR_FOREIGN)) {
                // #foreign glfw #callconv "c" #linkprefix "glfw"
                u32 prev_expr_level = self->expr_level;
                self->expr_level = -1;
                Expr *library = parse_expr(self);
                self->expr_level = prev_expr_level;
                Expr *linkprefix = NULL;
                Expr *linkname = NULL;
                Expr *callconv = NULL;
                for (;;) {
                    if (match_directive(self, DIR_LINKNAME)) {
                        if (linkname) error(self, parser_range(self), "Duplicate #linkname");
                        linkname = parse_str(self);
                    } else if (match_directive(self, DIR_LINKPREFIX)) {
                        if (linkprefix) error(self, parser_range(self), "Duplicate #linkprefix");
                        linkprefix = parse_str(self);
                    } else if (match_directive(self, DIR_CALLCONV)) {
                        if (callconv) error(self, parser_range(self), "Duplicate #callconv");
                        callconv = parse_str(self);
                    } else {
                        break;
                    }
                }
                Decl **decls = NULL;
                bool is_block = false;
                if (match_tok(self, TK_Lbrace)) is_block = true;
                do {
                    u32 start = self->ostart;
                    Expr *name = parse_name(self);
                    DeclFlags flags = DECL_NONE;
                    expect_tok(self, TK_Colon);
                    if (match_tok(self, TK_Colon)) flags |= DECL_CONSTANT;
                    Expr *type = parse_expr(self);
                    for (;;) { // Parse trailing directives
                        if (match_directive(self, DIR_LINKNAME)) {
                            if (linkname) {
                                error(self, parser_range(self), "Duplicate #linkname");
                            }
                            linkname = parse_str(self);
                        } else if (match_directive(self, DIR_CALLCONV)) {
                            if (callconv) {
                                error(self, parser_range(self), "Duplicate #callconv");
                            }
                            callconv = parse_str(self);
                        } else {
                            break;
                        }
                    }
                    expect_terminator(self);
                    Decl *decl = new_decl_foreign(self->package, r(start, self->olast), flags,
                                                  name, library, type, linkname, callconv);
                    parser_declare_and_queue_checking(self, decl);
                    if (!is_block) return (Stmt *) decl;
                    arrput(decls, decl);
                } while (!is_tok(self, TK_Rbrace) && !is_eof(self));
                expect_tok(self, TK_Rbrace);
                expect_terminator(self);
                Range range = {start, self->olast};
                return (Stmt *) new_decl_foreign_block(self->package, range, decls, linkprefix, callconv);
            }
            if (match_directive(self, DIR_IMPORT)) {
                Expr *path = parse_expr(self);
                Expr *alias = NULL;
                if (!is_terminator(self) && !is_tok(self, TK_Lbrace)) {
                    alias = parse_name(self);
                }
                ImportItem *items = NULL;
                if (match_tok(self, TK_Lbrace)) {
                    do {
                        if (match_tok(self, TK_Rbrace)) break;
                        Expr *name = parse_name(self);
                        Expr *alias = NULL;
                        if (match_tok(self, TK_Assign)) {
                            alias = parse_name(self);
                        }
                        ImportItem item = {name, alias};
                        arrput(items, item);
                    } while (match_tok(self, TK_Comma));
                }
                expect_terminator(self);
                Range range = {start, self->olast};
                Decl *decl = new_decl_import(self->package, range, path, alias, items);
                parser_declare_and_queue_checking(self, decl);
                return (Stmt *) decl;
            }
            error(self, parser_range(self), "Unexpected directive #%s", self->tok.tname);
            eat_tok(self);
            goto case_expr;
        }
        case TK_Keyword: {
            if (is_kw(self, KW_STRUCT) || is_kw(self, KW_UNION) || is_kw(self, KW_ENUM)
                || is_kw(self, KW_FN) || is_kw(self, KW_NIL) || is_kw(self, KW_USING)) {
                goto case_expr;
            }
            if (match_kw(self, KW_IF)) {
                i32 prev_expr_level = self->expr_level;
                self->expr_level = -1;
                Expr *cond = parse_expr(self);
                self->expr_level = prev_expr_level;
                Stmt *pass = parse_stmt(self);
                Stmt *fail = NULL;
                if (match_kw(self, KW_ELSE)) {
                    fail = parse_stmt(self);
                }
                expect_terminator(self);
                return new_stmt_if(self->package, r(start, self->olast), cond, pass, fail);
            }
            if (match_kw(self, KW_DEFER)) {
                Stmt *stmt = parse_stmt(self);
                return new_stmt_defer(self->package, r(start, self->olast), stmt);
            }
            if (match_kw(self, KW_FALLTHROUGH)) {
                expect_terminator(self);
                return new_stmt_goto(self->package, r(start, self->olast), GOTO_FALLTHROUGH, NULL);
            }
            if (match_kw(self, KW_BREAK)) {
                Expr *expr = NULL;
                if (!is_terminator(self)) expr = parse_name(self);
                expect_terminator(self);
                return new_stmt_goto(self->package, r(start, self->olast), GOTO_BREAK, expr);
            }
            if (match_kw(self, KW_CONTINUE)) {
                Expr *expr = NULL;
                if (!is_terminator(self)) expr = parse_name(self);
                return new_stmt_goto(self->package, r(start, self->olast), GOTO_CONTINUE, expr);
            }
            if (match_kw(self, KW_GOTO)) {
                Expr *expr = parse_name(self);
                expect_terminator(self);
                return new_stmt_goto(self->package, r(start, self->olast), GOTO_GOTO, expr);
            }
            if (match_kw(self, KW_FOR)) {
                if (is_tok(self, TK_Lbrace)) {
                    Stmt *body = parse_stmt(self);
                    expect_terminator(self);
                    Range range = {start, body->range.end};
                    return new_stmt_for(self->package, range, NULL, NULL, NULL, body);
                }
                Stmt *s1, *s2, *s3;
                s1 = s2 = s3 = NULL;
                i32 prev_expr_level = self->expr_level;
                self->expr_level = -2; // see default case of parse_simple_stmt
                s2 = parse_simple_stmt(self);
                if (s2->kind == STMT_NAMES) {
                    Expr *aggregate = NULL;
                    Expr *value = NULL;
                    Expr *index = NULL;
                    if (is_tok_name(self, intern_in)) {
                        eat_tok(self);
                        if (arrlen(s2->snames) > 0) value = s2->snames[0];
                        if (arrlen(s2->snames) > 1) value = s2->snames[1];
                        if (arrlen(s2->snames) > 2) {
                            Range range = {s2->snames[2]->range.start, arrlast(s2->snames)->range.end};
                            error(self, range,
                                  "For in iteration permits at most 2 names (value, index)");
                        }
                        free(s2); // See new_stmt_names
                        aggregate = parse_expr(self);
                        self->expr_level = prev_expr_level;
                        Stmt *body = parse_stmt(self);
                        Range range = {start, self->olast};
                        return new_stmt_for_aggregate(self->package, range, value, index, aggregate, body);
                    } else if (arrlen(s2->snames) == 1) {
                        Stmt *names = s2;
                        s2 = (Stmt *) s2->snames[0];
                        free(names);
                    } else {
                        error(self, parser_range(self),
                              "Expected single expression or 'in' to follow");
                    }
                }
                if (match_tok(self, TK_Terminator)) {
                    s1 = s2;
                    s2 = NULL;
                    if (!is_tok(self, TK_Lbrace) && !is_tok(self, TK_Terminator)) {
                        s2 = parse_simple_stmt(self);
                    }
                    expect_tok(self, TK_Terminator);
                    if (!is_tok(self, TK_Lbrace) && !is_tok(self, TK_Terminator)) {
                        s3 = parse_simple_stmt(self);
                    }
                    if (s2 && !ISEXPR(s2)) {
                        error(self, s2->range, "Expected expression");
                    }
                }
                self->expr_level = prev_expr_level;
                Stmt *body = parse_stmt(self);
                Range range = {start, self->olast};
                return new_stmt_for(self->package, range, s1, (Expr *) s2, s3, body);
            }
            if (match_kw(self, KW_SWITCH)) {
                i32 prev_expr_level = self->expr_level;
                self->expr_level = -1;
                Expr *match = NULL;
                if (!is_tok(self, TK_Lbrace)) match = parse_expr(self);
                self->expr_level = prev_expr_level;

                expect_tok(self, TK_Lbrace);
                SwitchCase *cases = NULL;
                for (;;) {
                    if (!match_kw(self, KW_CASE)) break;
                    u32 start = self->olast;
                    Expr **exprs = NULL;
                    if (!match_tok(self, TK_Colon)) {
                        exprs = parse_expr_list(self, parse_expr);
                        expect_tok(self, TK_Colon);
                    }
                    Stmt **stmts = NULL;
                    while (!(is_kw(self, KW_CASE) || is_tok(self, TK_Rbrace) || is_eof(self))) {
                        Stmt *stmt = parse_stmt(self);
                        arrput(stmts, stmt);
                    }
                    Stmt *body = new_stmt_block(self->package, r(start, self->olast), stmts);
                    SwitchCase c = {exprs, body};
                    arrput(cases, c);
                }
                expect_tok(self, TK_Rbrace);
                expect_terminator(self);
                return new_stmt_switch(self->package, r(start, self->olast), match, cases);
            }
            if (match_kw(self, KW_RETURN)) {
                Expr **exprs = NULL;
                if (!is_tok(self, TK_Terminator) && !is_eof(self) && !is_tok(self, TK_Rbrace)) {
                    exprs = parse_expr_list(self, parse_expr);
                }
                expect_terminator(self);
                return new_stmt_return(self->package, r(start, self->olast), exprs);
            }
        }
        case TK_Terminator: {
            eat_tok(self);
            goto begin;
        }
        default:
            fatal("");
            break;
    }
    eat_tok(self);
    return new_ast_invalid(self->package, parser_range(self));
}

void parser_init_interns(void) {
    TRACE(INIT);
    int num_keywords = sizeof keywords / sizeof *keywords;
    for (int i = KW_NONE + 1; i < num_keywords; i++) {
        const char *keyword = keywords[i];
        if (!keyword) continue;
        keywords[i] = str_intern(keyword);
    }
    int num_directives = sizeof directives / sizeof *directives;
    for (int i = DIR_NONE + 1; i < num_directives; i++) {
        const char *directive = directives[i];
        if (!directive) continue;
        directives[i] = str_intern(directive);
    }
    intern_in = str_intern("in");
}

#undef error
#undef note
