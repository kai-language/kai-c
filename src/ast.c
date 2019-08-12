
#include "all.h"
#include "ast.h"
#include "arena.h"
#include "package.h"
#include "string.h"

STATIC_ASSERT(offsetof(Ast, range) == offsetof(Expr, range));
STATIC_ASSERT(offsetof(Ast, range) == offsetof(Stmt, range));
STATIC_ASSERT(offsetof(Ast, range) == offsetof(Decl, range));

void *ast_alloc(Package *package, int kind, int flags, Range range, size_t size) {
    ASSERT(size >= offsetof(Ast, enil));
    ASSERT(kind < UINT8_MAX);
    ASSERT(flags < UINT8_MAX);
    Ast *ast = arena_alloc(&package->arena, size);
    ast->kind = kind;
    ast->flags = flags;
    ast->range = range;
    return ast;
}

#define ast_size(type, member) offsetof(type, member) + sizeof(((type *)0)->member)

void *new_ast_invalid(Package *package, Range range) {
    return ast_alloc(package, INVALID, 0, range, sizeof(Ast));
}

Expr *new_expr_nil(Package *package, Range range) {
    Expr *e = ast_alloc(package, EXPR_NIL, 0, range, ast_size(Expr, enil));
    return e;
}

Expr *new_expr_paren(Package *package, Range range, Expr *expr) {
    Expr *e = ast_alloc(package, EXPR_PAREN, 0, range, ast_size(Expr, eparen));
    e->eparen = expr;
    return e;
}

Expr *new_expr_int(Package *package, Range range, u64 val) {
    Expr *e = ast_alloc(package, EXPR_INT, 0, range, ast_size(Expr, eint));
    e->eint = val;
    return e;
}

Expr *new_expr_float(Package *package, Range range, f64 val) {
    Expr *e = ast_alloc(package, EXPR_FLOAT, 0, range, ast_size(Expr, efloat));
    e->efloat = val;
    return e;
}

Expr *new_expr_str(Package *package, Range range, const char *str, u32 len, bool mapped) {
    Expr *e = ast_alloc(package, EXPR_STR, 0, range, ast_size(Expr, estr));
    e->estr.str = (char *) str;
    e->estr.len = len;
    e->estr.mapped = mapped;
    return e;
}

Expr *new_expr_name(Package *package, Range range, const char *name) {
    Expr *e = ast_alloc(package, EXPR_NAME, 0, range, ast_size(Expr, ename));
    e->ename = name;
    return e;
}

Expr *new_expr_compound(Package *package, Range range, Expr *type, CompoundField *fields) {
    Expr *e = ast_alloc(package, EXPR_COMPOUND, 0, range, ast_size(Expr, ecompound));
    e->ecompound.type = type;
    e->ecompound.fields = fields;
    return e;
}

Expr *new_expr_cast(Package *package, Range range, Expr *type, Expr *expr) {
    Expr *e = ast_alloc(package, EXPR_CAST, 0, range, ast_size(Expr, ecast));
    e->ecast.type = type;
    e->ecast.expr = expr;
    return e;
}

Expr *new_expr_unary(Package *package, Range range, Op op, Expr *expr) {
    Expr *e = ast_alloc(package, EXPR_UNARY, op, range, ast_size(Expr, eunary));
    e->eunary = expr;
    return e;
}

Expr *new_expr_binary(Package *package, Range range, Op op, Expr *left, Expr *right) {
    Expr *e = ast_alloc(package, EXPR_BINARY, op, range, ast_size(Expr, ebinary));
    e->ebinary.elhs = left;
    e->ebinary.erhs = right;
    return e;
}

Expr *new_expr_ternary(Package *package, Range range, Expr *cond, Expr *pass, Expr *fail) {
    Expr *e = ast_alloc(package, EXPR_TERNARY, 0, range, ast_size(Expr, eternary));
    e->eternary.econd = cond;
    e->eternary.epass = pass;
    e->eternary.efail = fail;
    return e;
}

Expr *new_expr_call(Package *package, Range range, Expr *expr, CallArg *args) {
    Expr *e = ast_alloc(package, EXPR_CALL, 0, range, ast_size(Expr, ecall));
    e->ecall.expr = expr;
    e->ecall.args = args;
    return e;
}

Expr *new_expr_field(Package *package, Range range, Expr *expr, Expr *name) {
    Expr *e = ast_alloc(package, EXPR_FIELD, 0, range, ast_size(Expr, efield));
    e->efield.expr = expr;
    e->efield.name = name;
    return e;
}

Expr *new_expr_index(Package *package, Range range, Expr *expr, Expr *index) {
    Expr *e = ast_alloc(package, EXPR_INDEX, 0, range, ast_size(Expr, eindex));
    e->eindex.expr = expr;
    e->eindex.index = index;
    return e;
}

Expr *new_expr_slice(Package *package, Range range, Expr *base, Expr *lo, Expr *hi) {
    Expr *e = ast_alloc(package, EXPR_SLICE, 0, range, ast_size(Expr, eslice));
    e->eslice.base = base;
    e->eslice.lo = lo;
    e->eslice.hi = hi;
    return e;
}

Expr *new_expr_func(Package *package, Range range, FuncFlags flags, Expr *type, Stmt *body) {
    Expr *e = ast_alloc(package, EXPR_FUNC, flags, range, ast_size(Expr, efunc));
    e->efunc.type = type;
    e->efunc.body = body;
    return e;
}

Expr *new_expr_functype(Package *package, Range range, FuncFlags flags, FuncParam *params, FuncParam *result) {
    Expr *e = ast_alloc(package, EXPR_FUNCTYPE, flags, range, ast_size(Expr, efunctype));
    e->efunctype.params = params;
    e->efunctype.result = result;
    return e;
}

Expr *new_expr_array(Package *package, Range range, Expr *base, Expr *len) {
    Expr *e = ast_alloc(package, EXPR_ARRAY, 0, range, ast_size(Expr, earray));
    e->earray.base = base;
    e->earray.len = len;
    return e;
}

Expr *new_expr_vector(Package *package, Range range, Expr *base, Expr *len) {
    Expr *e = ast_alloc(package, EXPR_VECTOR, 0, range, ast_size(Expr, evector));
    e->evector.base = base;
    e->evector.len = len;
    return e;
}

Expr *new_expr_slicetype(Package *package, Range range, Expr *base) {
    Expr *e = ast_alloc(package, EXPR_SLICETYPE, 0, range, ast_size(Expr, eslicetype));
    e->eslicetype = base;
    return e;
}

Expr *new_expr_pointer(Package *package, Range range, Expr *base) {
    Expr *e = ast_alloc(package, EXPR_POINTER, 0, range, ast_size(Expr, epointer));
    e->epointer.base = base;
    return e;
}

Expr *new_expr_struct(Package *package, Range range, AggregateField *fields, u8 flags) {
    Expr *e = ast_alloc(package, EXPR_STRUCT, 0, range, ast_size(Expr, estruct));
    e->estruct.fields = fields;
    e->estruct.flags = flags;
    return e;
}

Expr *new_expr_union(Package *package, Range range, AggregateField *fields) {
    Expr *e = ast_alloc(package, EXPR_UNION, 0, range, ast_size(Expr, eunion));
    e->eunion.fields = fields;
    return e;
}

Expr *new_expr_enum(Package *package, Range range, EnumFlags flags, Expr *type, EnumItem *items) {
    Expr *e = ast_alloc(package, EXPR_ENUM, flags, range, ast_size(Expr, eenum));
    e->eenum.type = type;
    e->eenum.items = items;
    return e;
}

Decl *new_decl_file(Package *package, Source *file) {
    Range range = {file->start, file->start + file->len};
    Decl *d = ast_alloc(package, DECL_FILE, 0, range, ast_size(Decl, dfile));
    d->dfile = file;
    return d;
}

Decl *new_decl_val(Package *package, Range range, Expr *name, Expr *type, Expr *val) {
    Decl *d = ast_alloc(package, DECL_VAL, 0, range, ast_size(Decl, dval));
    d->dval.name = name;
    d->dval.type = type;
    d->dval.val = val;
    return d;
}

Decl *new_decl_var(Package *package, Range range, Expr **names, Expr *type, Expr **vals) {
    Decl *d = ast_alloc(package, DECL_VAR, 0, range, ast_size(Decl, dvar));
    d->dvar.names = names;
    d->dvar.type = type;
    d->dvar.vals = vals;
    return d;
}

Decl *new_decl_import(Package *package, Range range, Expr *path, Expr *alias, ImportItem *items) {
    Decl *d = ast_alloc(package, DECL_IMPORT, 0, range, ast_size(Decl, dimport));
    d->dimport.path = path;
    d->dimport.alias = alias;
    d->dimport.items = items;
    return d;
}

Decl *new_decl_library(Package *package, Range range, Expr *path, Expr *alias) {
    Decl *d = ast_alloc(package, DECL_LIBRARY, 0, range, ast_size(Decl, dlibrary));
    d->dlibrary.path = path;
    d->dlibrary.alias = alias;
    return d;
}

Decl *new_decl_foreign(Package *package, Range range, DeclFlags flags, 
                       Expr *name, Expr *library, Expr *type, 
                       Expr *linkname, Expr *callconv, Decl *block)
{
    Decl *d = ast_alloc(package, DECL_FOREIGN, flags, range, ast_size(Decl, dforeign));
    d->dforeign.name = name;
    d->dforeign.library = library;
    d->dforeign.type = type;
    d->dforeign.linkname = linkname;
    d->dforeign.callconv = callconv;
    d->dforeign.block = block;
    return d;
}

Decl *new_decl_foreign_block(Package *package, Range range, Decl **decls,
                             Expr *linkprefix, Expr *callconv)
{
    Decl *d = ast_alloc(package, DECL_FOREIGN_BLOCK, 0, range, ast_size(Decl, dforeign_block));
    d->dforeign_block.linkprefix = linkprefix;
    d->dforeign_block.callconv = callconv;
    d->dforeign_block.decls = decls;
    return d;
}

Stmt *new_stmt_label(Package *package, Range range, Expr *label) {
    Stmt *s = ast_alloc(package, STMT_LABEL, 0, range, ast_size(Stmt, slabel));
    s->slabel = label;
    return s;
}

Stmt *new_stmt_assign(Package *package, Range range, Expr **lhs, Expr **rhs) {
    Stmt *s = ast_alloc(package, STMT_ASSIGN, 0, range, ast_size(Stmt, sassign));
    s->sassign.lhs = lhs;
    s->sassign.rhs = rhs;
    return s;
}

Stmt *new_stmt_return(Package *package, Range range, Expr **exprs) {
    Stmt *s = ast_alloc(package, STMT_RETURN, 0, range, ast_size(Stmt, sreturn));
    s->sreturn = exprs;
    return s;
}

Stmt *new_stmt_defer(Package *package, Range range, Stmt *stmt) {
    Stmt *s = ast_alloc(package, STMT_DEFER, 0, range, ast_size(Stmt, sdefer));
    s->sdefer = stmt;
    return s;
}

Stmt *new_stmt_using(Package *package, Range range, Expr **exprs) {
    Stmt *s = ast_alloc(package, STMT_USING, 0, range, ast_size(Stmt, susing));
    s->susing = exprs;
    return s;
}

Stmt *new_stmt_goto(Package *package, Range range, GotoKind kind, Expr *expr) {
    Stmt *s = ast_alloc(package, STMT_GOTO, kind, range, ast_size(Stmt, sgoto));
    s->sgoto = expr;
    return s;
}

Stmt *new_stmt_block(Package *package, Range range, Stmt **stmts) {
    Stmt *s = ast_alloc(package, STMT_BLOCK, 0, range, ast_size(Stmt, sblock));
    s->sblock = stmts;
    return s;
}

Stmt *new_stmt_if(Package *package, Range range, Expr *cond, Stmt *pass, Stmt *fail) {
    Stmt *s = ast_alloc(package, STMT_IF, 0, range, ast_size(Stmt, sif));
    s->sif.cond = cond;
    s->sif.pass = pass;
    s->sif.fail = fail;
    return s;
}

Stmt *new_stmt_for(Package *package, Range range, Stmt *init, Expr *cond, Stmt *step, Stmt *body) {
    Stmt *s = ast_alloc(package, STMT_FOR, FOR_REGULAR, range, ast_size(Stmt, sfor));
    s->sfor.init = init;
    s->sfor.cond = cond;
    s->sfor.step = step;
    s->sfor.body = body;
    return s;
}

Stmt *new_stmt_for_aggregate(Package *package, Range range, Expr *value, Expr *index, Expr *aggregate, Stmt *body) {
    Stmt *s = ast_alloc(package, STMT_FOR, FOR_AGGREGATE, range, ast_size(Stmt, sfor));
    s->sfor.value_name = value;
    s->sfor.index_name = index;
    s->sfor.aggregate = aggregate;
    s->sfor.body = body;
    return s;
}

Stmt *new_stmt_switch(Package *package, Range range, Expr *match, SwitchCase *cases) {
    Stmt *s = ast_alloc(package, STMT_SWITCH, 0, range, ast_size(Stmt, sswitch));
    s->sswitch.subject = match;
    s->sswitch.cases = cases;
    return s;
}

// Temp node! Needs free!
Stmt *new_stmt_names(Package *package, Range range, Expr **names) {
    size_t size = ast_size(Stmt, snames);
    Stmt *s = xmalloc(size);
    s->kind = STMT_NAMES;
    s->range = range;
    s->snames = names;
    return s;
}

const char *describe_goto_kind(int kind) {
    switch (kind) {
        case GOTO_CONTINUE:    return "continue";
        case GOTO_FALLTHROUGH: return "fallthrough";
        case GOTO_BREAK:       return "break";
        case GOTO_GOTO:        return "goto";
        default: fatal("Unknown goto kind %d", kind);
    }
}

const char *describe_ast_kind(int kind) {
    switch (kind) {
        case INVALID:           return "invalid";
        case EXPR_NIL:          return "nil";
        case EXPR_INT:          return "int";
        case EXPR_FLOAT:        return "float";
        case EXPR_STR:          return "str";
        case EXPR_NAME:         return "name";
        case EXPR_COMPOUND:     return "compound";
        case EXPR_CAST:         return "cast";
        case EXPR_PAREN:        return "paren";
        case EXPR_UNARY:        return "unary";
        case EXPR_BINARY:       return "binary";
        case EXPR_TERNARY:      return "ternary";
        case EXPR_CALL:         return "call";
        case EXPR_FIELD:        return "field";
        case EXPR_INDEX:        return "index";
        case EXPR_SLICE:        return "slice";
        case EXPR_FUNC:         return "func";
        case EXPR_FUNCTYPE:     return "functype";
        case EXPR_SLICETYPE:    return "slicetype";
        case EXPR_ARRAY:        return "array";
        case EXPR_POINTER:      return "pointer";
        case EXPR_STRUCT:       return "struct";
        case EXPR_UNION:        return "union";
        case EXPR_ENUM:         return "enum";
        case DECL_VAR:          return "var";
        case DECL_VAL:          return "val";
        case DECL_IMPORT:       return "import";
        case DECL_LIBRARY:      return "library";
        case DECL_FOREIGN:      return "foreign";
        case DECL_FOREIGN_BLOCK: return "foreign_block";
        case STMT_LABEL:        return "label";
        case STMT_ASSIGN:       return "assign";
        case STMT_RETURN:       return "return";
        case STMT_DEFER:        return "defer";
        case STMT_USING:        return "using";
        case STMT_GOTO:         return "goto";
        case STMT_BLOCK:        return "block";
        case STMT_IF:           return "if";
        case STMT_FOR:          return "for";
        case STMT_SWITCH:       return "switch";
        case STMT_NAMES:
            return NULL;
        default:
            fatal("Unknown ast kind %p", kind);
    }
}

const char *describe_ast(Package *package, void *p) {
    Ast *ast = p;
    Source *source = package_source(package, ast->range.start);
    if (!source) return describe_ast_kind(ast->kind);
    u32 start = ast->range.start - source->start;
    u32 end = ast->range.end - source->start;
    return str_intern_range(source->code + start, source->code + end);
}

const char *describe_op(Op op) {
    switch (op) {
        case OP_ADD:  return "+";
        case OP_SUB:  return "-";
        case OP_MUL:  return "*";
        case OP_DIV:  return "/";
        case OP_REM:  return "%";
        case OP_AND:  return "&";
        case OP_OR:   return "|";
        case OP_XOR:  return "^";
        case OP_SHL:  return "<<";
        case OP_SHR:  return ">>";
        case OP_NOT:  return "!";
        case OP_BNOT: return "~";
        case OP_LAND: return "a";
        case OP_LOR:  return "o";
        case OP_LSS:  return "<";
        case OP_GTR:  return ">";
        case OP_EQL:  return "==";
        case OP_NEQ:  return "!=";
        case OP_LEQ:  return "<=";
        case OP_GEQ:  return ">=";
        case NUM_OPS: 
        case OP_NONE: fatal("Bad op");
    }
}

int ast_sizes[] = {
    [INVALID]            = sizeof(Stmt),
    [EXPR_NIL]           = ast_size(Expr, enil),
    [EXPR_INT]           = ast_size(Expr, eint),
    [EXPR_FLOAT]         = ast_size(Expr, efloat),
    [EXPR_STR]           = ast_size(Expr, estr),
    [EXPR_NAME]          = ast_size(Expr, ename),
    [EXPR_COMPOUND]      = ast_size(Expr, ecompound),
    [EXPR_CAST]          = ast_size(Expr, ecast),
    [EXPR_PAREN]         = ast_size(Expr, eparen),
    [EXPR_UNARY]         = ast_size(Expr, eunary),
    [EXPR_BINARY]        = ast_size(Expr, ebinary),
    [EXPR_TERNARY]       = ast_size(Expr, eternary),
    [EXPR_CALL]          = ast_size(Expr, ecall),
    [EXPR_FIELD]         = ast_size(Expr, efield),
    [EXPR_INDEX]         = ast_size(Expr, eindex),
    [EXPR_SLICE]         = ast_size(Expr, eslice),
    [EXPR_FUNC]          = ast_size(Expr, efunc),
    [EXPR_FUNCTYPE]      = ast_size(Expr, efunctype),
    [EXPR_SLICETYPE]     = ast_size(Expr, eslicetype),
    [EXPR_ARRAY]         = ast_size(Expr, earray),
    [EXPR_POINTER]       = ast_size(Expr, epointer),
    [EXPR_STRUCT]        = ast_size(Expr, estruct),
    [EXPR_UNION]         = ast_size(Expr, eunion),
    [EXPR_ENUM]          = ast_size(Expr, eenum),
    [DECL_VAR]           = ast_size(Decl, dvar),
    [DECL_VAL]           = ast_size(Decl, dval),
    [DECL_IMPORT]        = ast_size(Decl, dimport),
    [DECL_LIBRARY]       = ast_size(Decl, dlibrary),
    [DECL_FOREIGN]       = ast_size(Decl, dforeign),
    [DECL_FOREIGN_BLOCK] = ast_size(Decl, dforeign_block),
    [DECL_FILE]          = ast_size(Decl, dfile),
    [STMT_LABEL]         = ast_size(Stmt, slabel),
    [STMT_ASSIGN]        = ast_size(Stmt, sassign),
    [STMT_RETURN]        = ast_size(Stmt, sreturn),
    [STMT_DEFER]         = ast_size(Stmt, sdefer),
    [STMT_USING]         = ast_size(Stmt, susing),
    [STMT_GOTO]          = ast_size(Stmt, sgoto),
    [STMT_BLOCK]         = ast_size(Stmt, sblock),
    [STMT_IF]            = ast_size(Stmt, sif),
    [STMT_FOR]           = ast_size(Stmt, sfor),
    [STMT_SWITCH]        = ast_size(Stmt, sswitch),
    [STMT_NAMES]         = ast_size(Stmt, snames),
};

void *ast_copy(Package *package, void *ast) {
    ASSERT(ast);
    Ast *old = ast;
    int size = ast_sizes[old->kind];

#define copy(ast) (ast_copy(package, (ast)))
#define copy_can_null(ast) ((ast) ? ast_copy(package, (ast)) : NULL)

    Ast *new = arena_alloc(&package->arena, size);
    memcpy(new, old, size);

    switch (old->kind) {
        case INVALID: return ast;
        case EXPR_NIL: return new;
        case EXPR_INT: return new;
        case EXPR_FLOAT: return new;
        case EXPR_STR: return new;
        case EXPR_NAME: return new;
        case EXPR_COMPOUND: {
            new->ecompound.fields = NULL;
            for (i64 i = 0; i < arrlen(old->ecompound.fields); i++) {
                CompoundField field = old->ecompound.fields[i];
                field.key = copy_can_null(field.key);
                field.val = copy(field.val);
                arrput(new->ecompound.fields, field);
            }
            new->ecompound.type = copy_can_null(old->ecompound.type);
            return new;
        }
        case EXPR_CAST: {
            new->ecast.type = copy_can_null(old->ecast.type);
            new->ecast.expr = copy_can_null(old->ecast.expr);
            return new;
        }
        case EXPR_PAREN: {
            new->eparen = copy(old->eparen);
            return new;
        }
        case EXPR_UNARY: {
            new->eunary = copy(old->eparen);
            return new;
        }
        case EXPR_BINARY: {
            new->ebinary.elhs = copy(old->ebinary.elhs);
            new->ebinary.erhs = copy(old->ebinary.erhs);
            return new;
        }
        case EXPR_TERNARY: {
            new->eternary.econd = copy(old->eternary.econd);
            new->eternary.epass = copy_can_null(old->eternary.epass);
            new->eternary.efail = copy(old->eternary.efail);
            return new;
        }
        case EXPR_CALL: {
            new->ecall.expr = copy(old->ecall.expr);
            new->ecall.args = NULL;
            for (i64 i = 0; i < arrlen(old->ecall.args); i++) {
                CallArg arg = old->ecall.args[i];
                arg.name = copy_can_null(arg.name);
                arg.expr = copy(arg.expr);
                arrput(new->ecall.args, arg);
            }
            return new;
        }
        case EXPR_FIELD: {
            new->efield.expr = copy(old->efield.expr);
            new->efield.name = copy(old->efield.name);
            return new;
        }
        case EXPR_INDEX: {
            new->eindex.expr = copy(old->eindex.expr);
            new->eindex.index = copy(old->eindex.index);
            return new;
        }
        case EXPR_SLICE: {
            new->eslice.base = copy(old->eslice.base);
            new->eslice.lo = copy_can_null(old->eslice.lo);
            new->eslice.hi = copy_can_null(old->eslice.hi);
            return new;
        }
        case EXPR_FUNC: {
            new->efunc.type = copy(old->efunc.type);
            new->efunc.body = copy(old->efunc.body);
            return new;
        }
        case EXPR_FUNCTYPE: {
            new->efunctype.params = NULL;
            for (i64 i = 0; i < arrlen(old->efunctype.params); i++) {
                FuncParam param = old->efunctype.params[i];
                param.name = copy_can_null(param.name);
                param.type = copy(param.type);
                arrput(new->efunctype.params, param);
            }
            new->efunctype.result = NULL;
            for (i64 i = 0; i < arrlen(old->efunctype.result); i++) {
                FuncParam result = old->efunctype.result[i];
                result.name = copy_can_null(result.name);
                result.type = copy(result.type);
                arrput(new->efunctype.result, result);
            }
            return new;
        }
        case EXPR_SLICETYPE: {
            new->eslicetype = copy(old->eslicetype);            
            return new;
        }
        case EXPR_ARRAY: {
            new->earray.base = copy(old->earray.base);
            new->earray.len = copy_can_null(old->earray.len);
            return new;
        }
        case EXPR_VECTOR: {
            new->evector.base = copy(old->evector.base);
            new->evector.base = copy(old->evector.len);
            return new;
        }
        case EXPR_POINTER: {
            new->epointer.base = copy(old->epointer.base);
            return new;
        }
        case EXPR_STRUCT: {
            new->estruct.fields = NULL;
            for (i64 i = 0; i < arrlen(old->estruct.fields); i++) {
                AggregateField field = old->estruct.fields[i];
                Expr **names = NULL;
                for (i64 i = 0; i < arrlen(field.names); i++) {
                    Expr *name = copy(field.names[i]);
                    arrput(names, name);
                }
                field.names = names;
                field.type = copy(field.type);
                arrput(new->estruct.fields, field);
            }
            return new;
        }
        case EXPR_UNION: {
            new->eunion.fields = NULL;
            for (i64 i = 0; i < arrlen(old->estruct.fields); i++) {
                AggregateField field = old->eunion.fields[i];
                Expr **names = NULL;
                for (i64 i = 0; i < arrlen(field.names); i++) {
                    Expr *name = copy(field.names[i]);
                    arrput(names, name);
                }
                field.names = names;
                field.type = copy(field.type);
                arrput(new->eunion.fields, field);
            }
            return new;
        }
        case EXPR_ENUM: {
            new->eenum.items = NULL;
            for (i64 i = 0; i < arrlen(old->eenum.items); i++) {
                EnumItem item = old->eenum.items[i];
                item.name = copy(item.name);
                item.init = copy_can_null(item.init);
                arrput(new->eenum.items, item);
            }
            new->eenum.type = copy(old->eenum.type);
            return new;
        }
        case DECL_VAR: {
            new->dvar.names = NULL;
            for (i64 i = 0; i < arrlen(old->dvar.names); i++) {
                Expr *name = copy(old->dvar.names[i]);
                arrput(new->dvar.names, name);
            }
            new->dvar.type = copy_can_null(old->dvar.type);
            new->dvar.vals = NULL;
            for (i64 i = 0; i < arrlen(old->dvar.vals); i++) {
                Expr *val = copy(old->dvar.vals[i]);
                arrput(new->dvar.vals, val);
            }
            return new;
        }
        case DECL_VAL: {
            new->dval.name = copy(old->dval.name);
            new->dval.type = copy(old->dval.type);
            new->dval.val = copy(old->dval.val);
            return new;
        }
        case DECL_IMPORT:
        case DECL_LIBRARY:
        case DECL_FOREIGN: // TODO: Make sure when we implement polymorphing that we forbid foreign
        case DECL_FOREIGN_BLOCK:
        case DECL_FILE: return new; // N/A currently very little reason to polymorph these
        case STMT_LABEL: {
            new->slabel = copy(old->slabel);
            return new;
        }
        case STMT_ASSIGN: {
            new->sassign.lhs = NULL;
            new->sassign.rhs = NULL;
            for (i64 i = 0; i < arrlen(old->sassign.lhs); i++) {
                Expr *lhs = copy(old->sassign.lhs[i]);
                Expr *rhs = copy(old->sassign.rhs[i]);
                arrput(new->sassign.lhs, lhs);
                arrput(new->sassign.rhs, rhs);
            }
            return new;
        }
        case STMT_RETURN: {
            new->sreturn = NULL;
            for (i64 i = 0; i < arrlen(old->sreturn); i++) {
                Expr *expr = copy(old->sreturn[i]);
                arrput(new->sreturn, expr);
            }
            return new;
        }
        case STMT_DEFER: {
            new->sdefer = copy(old->sdefer);
            return new;
        }
        case STMT_USING: fatal("Unimplemented");
        case STMT_GOTO: {
            new->sgoto = copy_can_null(old->sgoto);
            return new;
        }
        case STMT_BLOCK: {
            new->sblock = NULL;
            for (i64 i = 0; i < arrlen(old->sblock); i++) {
                Stmt *stmt = copy(old->sblock[i]);
                arrput(new->sblock, stmt);
            }
            return new;
        }
        case STMT_IF: {
            new->sif.cond = copy(old->sif.cond);
            new->sif.pass = copy(old->sif.pass);
            new->sif.fail = copy_can_null(old->sif.fail);
            return new;
        }
        case STMT_FOR: {
            new->sfor.init = copy_can_null(old->sfor.init);
            new->sfor.cond = copy_can_null(old->sfor.cond);
            new->sfor.step = copy_can_null(old->sfor.step);
            new->sfor.body = copy(old->sfor.body);
            return new;
        }
        case STMT_SWITCH: {
            new->sswitch.subject = copy(old->sswitch.subject);
            new->sswitch.cases = NULL;
            for (i64 i = 0; i < arrlen(old->sswitch.cases); i++) {
                SwitchCase cse = old->sswitch.cases[i];
                cse.body = copy(cse.body);
                Expr **matches = NULL;
                for (i64 i = 0; i < arrlen(cse.matches); i++) {
                    Expr *match = copy(cse.matches[i]);
                    arrput(matches, match);
                }
                cse.matches = matches;
                arrput(new->sswitch.cases, cse);
            }
            return new;
        }
        case STMT_NAMES:
        default:
            fatal("Unrecognized ast case %s", describe_ast(package, ast));
    }

#undef copy
#undef copy_can_null
}
