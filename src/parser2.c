
i32 token_precedence[NUM_TOKEN_KINDS] = {
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

typedef struct Parse Parse;
struct Parse {
    Package *package;

    Lex2 lexer;
    Tok2 tok;

    const char *calling_conv;
    const char *link_prefix;
};

Tok2 next_token(Parser *self) {
    self->tok2 = lexer_next_token(&self->lexer2);
    self->tok2.offset_start += self->source->start;
    self->tok2.offset_end   += self->source->start;
    return self->tok2;
}

bool is_token(Parser *self, TokenKind kind) {
    return self->tok2.kind == kind;
}

bool is_token_eof(Parser *self) {
    return self->tok2.kind == TK_Eof;
}

bool is_token_name(Parser *self, const char *name) {
    return self->tok2.kind == TK_Ident && self->tok2.tname == name;
}

bool is_keyword(Parser *self, const char *name) {
    return is_token(self, TK_Keyword) && self->tok2.tname == name;
}

bool match_keyword(Parser *self, const char *name) {
    if (!is_keyword(self, name)) return false;
    next_token(self);
    return true;
}

bool match_token(Parser *self, TokenKind kind) {
    if (!is_token(self, kind)) return false;
    next_token(self);
    return true;
}

bool expect_token(Parser *self, TokenKind kind) {
    if (!is_token(self, kind)) {
        //        fatal_error_here("Expected token %s, got %s", token_kind_name(kind), token_info());
        return false;
    }
    next_token(self);
    return true;
}

void package_posinfo_online(Package *package, u32 offset) {
}

PosInfo package_posinfo(Package *package, Pos pos) {
    Source *source;
    for (int i = 0; i < package->sources.count; i++) {
        source = &package->sources.list[i];
        Pos end = source->start + source->len;
        if (pos > source->start && pos < end) goto found;
    }
    return (PosInfo){0};

found:;
    u32 offset = pos - source->start;

    Lex2 lexer;
    lexer_init(&lexer, source->code);
    lexer.client = (LexerClient){
        .data = &source,
        .online = (void *) package_posinfo_online,
    };
    return (PosInfo){0};
}

Source *source_at_pos(Sources sources, Pos pos) {
    for (int i = 0; i < sources.count; i++) {
        Source *source = &sources.list[i];
        Pos end = source->len;
        if (pos > source->start && pos < end) return source;
    }
    return NULL;
}

typedef struct PosInfoWithLineOffset PosInfoWithLineOffset;
struct PosInfoWithLineOffset {
    PosInfo pos;
    u32 last_line_offset;
};

void lex_pos_info_online(PosInfoWithLineOffset *info, u32 offset) {
    info->pos.line += 1;
    info->last_line_offset += 1;
}

PosInfo lex_pos_info(Pos pos, Sources sources) {
    Source *source = source_at_pos(sources, pos);
    if (!source) return (PosInfo){0};
    Lex2 lexer;
    lexer_init(&lexer, source->code);
    u32 offset = pos - source->start;
    PosInfoWithLineOffset info = {0};
    lexer.client = (LexerClient){
        .data = &info,
        .online = (OnLineFunc) (void *) lex_pos_info_online,
    };
    while (lexer.str - lexer.start < offset) lexer_next_token(&lexer);
    return info.pos;
}

void parser_error(Parser *self, Range range, const char *msg, ...) {
    va_list args;
    char msg_buffer[512];
    va_start(args, msg);
    vsnprintf(msg_buffer, sizeof msg_buffer, msg, args);
    va_end(args);

    char err_buffer[512];

    PosInfo start = lex_pos_info(range.start, self->package->sources);
    int len = snprintf(err_buffer, sizeof err_buffer, "ERROR(%s:%u:%u): %s\n",
                       self->source->name, start.line, start.column, msg_buffer);

    len = MIN(len, sizeof err_buffer);
    char *err_msg = ArenaAlloc(&self->package->arena, len + 1);
    memcpy(err_msg, err_buffer, len + 1);

    const char *code_block = NULL;
    // TODO: findCodeBlockAndHighlightError with the new lexer

    DiagnosticError error = {err_msg, code_block};
    ArrayPush(self->package->diagnostics.errors, error);
}

void parse_expr_atom(Parse *self);

#if TEST
//void test_lex_pos_info() {
//    Lex2 lexer;
//    char src[] = "main :: fn() -> void { }";
//    Source source = {.name = "", .path = "", .code = src, .len = sizeof src};
//    Sources sources = {.list = &source, .count = 1, .end = sizeof src};
//
//    lexer_init(&lexer, src);
//    lexer_next_token(&lexer);
//    lexer_next_token(&lexer);
//    Tok2 tok = lexer_next_token(&lexer);
//    lexer_posinfo(&lexer, pos);
//}
//
//Package parser2TestPackage = {0};
//void new_test_parser(const char *code) {
//    Lex2 lexer;
//    Package *package = calloc(sizeof *package, 1);
//    package->path = "test";
//    package->fullpath = "test";
//    package->scope = pushScope(package, compiler.builtin_package.scope);
//
//    Source source = {0};
//    strcpy(source.path, ":test:");
//    source.code = code;
//    source.len = (u32) strlen(code);
//    package->sources.count = 1;
//    package->sources.list = &source;
//    Parse parser = {package, lexer};
//}
//
//void test_parse_expr_atom() {
//
//}
#endif
