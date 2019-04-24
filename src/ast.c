
#include "all.h"
#include "ast.h"
#include "arena.h"
#include "package.h"

STATIC_ASSERT(offsetof(Ast, range) == offsetof(Expr, range));
STATIC_ASSERT(offsetof(Ast, range) == offsetof(Stmt, range));
STATIC_ASSERT(offsetof(Ast, range) == offsetof(Decl, range));

void *ast_alloc(Package *package, int kind, int flags, Range range, size_t size) {
    ASSERT(size >= sizeof(Ast));
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

Expr *new_expr_struct(Package *package, Range range, AggregateField *fields) {
    Expr *e = ast_alloc(package, EXPR_STRUCT, 0, range, ast_size(Expr, estruct));
    e->estruct.fields = fields;
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
    Decl *d = ast_alloc(package, DECL_FOREIGNBLOCK, 0, range, ast_size(Decl, dforeign_block));
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
        case DECL_FOREIGNBLOCK: return "foreign_block";
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
            fatal("Unknown ast kind %hc", kind);
    }
}

const char *describe_ast(void *p) {
    Ast *ast = p;
    return describe_ast_kind(ast->kind);
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

