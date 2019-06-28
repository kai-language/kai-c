
#include "all.h"
#include "ast.h"
#include "types.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "checker.h"
#include "compiler.h"
#include "llvm.h"
#include "llvm_wrap.hpp"

typedef struct Emitter Emitter;
struct Emitter {
    Package *package;
    IRContext *context;
    IRFunction *function;
    Sym *symbol;
    bool return_address;
};

Emitter *llvm_init(Package *pkg) {
    Emitter *emitter = malloc(sizeof *emitter);
    emitter->context = llvm_backend_init(pkg);
    return emitter;
}

IRType *llvm_type(IRContext *c, Type *type) {
    switch (type->kind) {
        case TYPE_INVALID:
        case TYPE_COMPLETING: fatal("Invalid type in backend");
        case TYPE_VOID:       return llvm_void(c);
        case TYPE_BOOL:       return llvm_integer(c, 1);
        case TYPE_ENUM:    // fallthrough
        case TYPE_INT:        return llvm_integer(c, type->size);
        case TYPE_FLOAT:      return llvm_float(c, type->size);
        case TYPE_PTR:        return llvm_pointer(c, llvm_type(c, type->tptr.base));
        case TYPE_FUNC: {
            IRType **params = NULL;
            arrsetcap(params, arrlen(type->tfunc.params));
            for (int i = 0; i < arrlen(type->tfunc.params); i++) {
                IRType *param = llvm_type(c, type->tfunc.params[i]);
                arrput(params, param);
            }
            IRType *result = llvm_type(c, type->tfunc.result);
            return llvm_function(c, params, result, (type->flags&FUNC_CVARGS) != 0);
        }
        case TYPE_ARRAY: {
            IRType *base = llvm_type(c, type->tarray.eltype);
            return llvm_array(c, base, type->tarray.length);
        }
        case TYPE_SLICE:      return NULL;
        case TYPE_STRUCT: {
            IRType **elements = NULL;
            arrsetcap(elements, arrlen(type->taggregate.fields));
            for (int i = 0; i < arrlen(type->taggregate.fields); i++) {
                IRType *element = llvm_type(c, type->taggregate.fields[i].type);
                arrput(elements, element);
            }
            return llvm_struct(c, elements);
        }
        case TYPE_UNION: {
            IRType *data = llvm_integer(c, type->size);
            IRType **elements = NULL;
            arrput(elements, data);
            IRType *type = llvm_struct(c, elements);
            arrfree(elements);
            return type;
        }
        case TYPE_ANY: return NULL;
        default:
            fatal("Unhandled type");
    }
}

DBGType *llvm_debug_type(IRContext *c, Type *type) {
    switch (type->kind) {
        case TYPE_INVALID:
        case TYPE_COMPLETING:
        case TYPE_VOID:
//            return llvm_debug_void(c);
        case TYPE_BOOL:
        case TYPE_ENUM:
        case TYPE_INT:
            return llvm_debug_type_integer(c, (type->flags&SIGNED) != 0, type->size);
        case TYPE_FLOAT:
        case TYPE_PTR:
        case TYPE_FUNC: {
            DBGType **params = NULL;
            for (i64 i = 0; i < arrlen(type->tfunc.params); i++) {
                DBGType *param = llvm_debug_type(c, type->tfunc.params[i]);
                arrput(params, param);
            }
            DBGType *result = llvm_debug_type(c, type->tfunc.result);
            return llvm_debug_type_function(c, params, result, (u32) arrlen(params));
        }
        case TYPE_ARRAY:
        case TYPE_SLICE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ANY:
        default:
            fatal("Unhandled type");
    }
}

IRValue *emit_expr(Emitter *self, Expr *expr);
void emit_decl(Emitter *self, Decl *decl);
void emit_stmt(Emitter *self, Stmt *stmt);

IRValue *emit_expr_nil(Emitter *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    IRType *type = llvm_type(self->context, operand.type);
    return llvm_constant_null(self->context, type);
}

IRValue *emit_expr_int(Emitter *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    IRType *type = llvm_type(self->context, operand.type);
    if (operand.type->kind == TYPE_FLOAT) {
        return llvm_constant_float(self->context, type, (f64) operand.val.u);
    }
    return llvm_constant_integer(self->context, type, operand.val.u, is_signed(operand.type));
}

IRValue *emit_expr_float(Emitter *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    IRType *type = llvm_type(self->context, operand.type);
    return llvm_constant_float(self->context, type, operand.val.f);
}

IRValue *emit_expr_str(Emitter *self, Expr *expr) {
    return llvm_global_string_pointer(self->context, expr->estr.str, expr->estr.len);
}

IRValue *emit_expr_name(Emitter *self, Expr *expr) {
    Sym *sym = hmget(self->package->symbols, expr);
    if (!sym->userdata) {
        emit_decl(self, sym->decl);
        ASSERT(sym->userdata);
    }
    if (self->return_address) return sym->userdata;
    return llvm_create_load(self->context, sym->userdata, sym->type->align);
}

IRValue *emit_expr_compound(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_cast(Emitter *self, Expr *expr) { return NULL; }

IRValue *emit_expr_paren(Emitter *self, Expr *expr) {
    return emit_expr(self, expr->eparen);
}

IRValue *emit_expr_unary(Emitter *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    bool prev_return_address = self->return_address;
    self->return_address = true;
    IRValue *value = emit_expr(self, expr->eunary);
    self->return_address = prev_return_address;
    PosInfo pos = package_posinfo(self->package, expr->range.start);
    llvm_debug_set_pos(self->context, pos.line, pos.column);
    switch ((Op) expr->flags) {
        case OP_ADD:
        case OP_AND:
            return value;
        case OP_SUB:
            if (operand.type->kind == TYPE_FLOAT) {
                return llvm_create_fneg(self->context, value);
            } else {
                return llvm_create_neg(self->context, value);
            }
        case OP_NOT:
        case OP_BNOT:
            return llvm_create_not(self->context, value);
        case OP_LSS:
            if (self->return_address) return value;
            return llvm_create_load(self->context, value, operand.type->tptr.base->align);
        default:
            fatal("Unhandled unary op case %d", expr->flags);
    }
}

IRValue *emit_expr_binary(Emitter *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    bool is_int = is_integer(operand.type);
    IRType *type = llvm_type(self->context, operand.type);
    bool prev_return_address = self->return_address;
    self->return_address = true;
    IRValue *lhs = emit_expr(self, expr->ebinary.elhs);
    IRValue *rhs = emit_expr(self, expr->ebinary.erhs);
    self->return_address = prev_return_address;
    PosInfo pos = package_posinfo(self->package, expr->range.start);
    llvm_debug_set_pos(self->context, pos.line, pos.column);
    IRContext *c = self->context;
    switch (expr->flags) {
        case OP_ADD: return is_int ? llvm_create_add(c, lhs, rhs) : llvm_create_fadd(c, lhs, rhs);
        case OP_SUB: return is_int ? llvm_create_sub(c, lhs, rhs) : llvm_create_fsub(c, lhs, rhs);
        case OP_MUL: return is_int ? llvm_create_mul(c, lhs, rhs) : llvm_create_fmul(c, lhs, rhs);
        case OP_DIV:
            return is_int ?
                is_signed(operand.type) ?
                    llvm_create_sdiv(c, lhs, rhs) : llvm_create_udiv(c, lhs, rhs) :
                    llvm_create_fdiv(c, lhs, rhs);
        case OP_REM:
            return is_int ?
                is_signed(operand.type) ?
                    llvm_create_sdiv(c, lhs, rhs) : llvm_create_udiv(c, lhs, rhs) :
                    llvm_create_fdiv(c, lhs, rhs);
        case OP_AND: return llvm_create_and(c, lhs, rhs);
        case OP_OR:  return llvm_create_or (c, lhs, rhs);
        case OP_XOR: return llvm_create_xor(c, lhs, rhs);
        case OP_LOR:
            return llvm_create_trunc_or_bitcast(c, llvm_create_or(c, lhs, rhs), type);
        case OP_LAND:
            return llvm_create_trunc_or_bitcast(c, llvm_create_and(c, lhs, rhs), type);
        case OP_SHR:
            return is_signed(operand.type) ?
                llvm_create_ashr(c, lhs, rhs) : llvm_create_lshr(c, lhs, rhs);
        case OP_SHL:
            return llvm_create_shl(c, lhs, rhs);
        case OP_LSS:
            return is_int ?
                is_signed(operand.type) ?
                    llvm_create_icmp_slt(c, lhs, rhs) : llvm_create_icmp_ult(c, lhs, rhs) :
                    llvm_create_fcmp_olt(c, lhs, rhs);
        case OP_LEQ:
            return is_int ?
                is_signed(operand.type) ?
                    llvm_create_icmp_sle(c, lhs, rhs) : llvm_create_icmp_ule(c, lhs, rhs) :
                    llvm_create_fcmp_ole(c, lhs, rhs);
        case OP_GTR:
            return is_int ?
                is_signed(operand.type) ?
                    llvm_create_icmp_sgt(c, lhs, rhs) : llvm_create_icmp_ugt(c, lhs, rhs) :
                    llvm_create_fcmp_ogt(c, lhs, rhs);
        case OP_GEQ:
            return is_int ?
                is_signed(operand.type) ?
                    llvm_create_icmp_sge(c, lhs, rhs) : llvm_create_icmp_uge(c, lhs, rhs) :
                    llvm_create_fcmp_oge(c, lhs, rhs);
        case OP_EQL:
            return is_int ? llvm_create_icmp_eq(c, lhs, rhs) : llvm_create_fcmp_eq(c, lhs, rhs);
        case OP_NEQ:
            return is_int ? llvm_create_icmp_ne(c, lhs, rhs) : llvm_create_fcmp_ne(c, lhs, rhs);
        default:
            fatal("Unhandled binary op case %d", expr->flags);
    }
}

IRValue *emit_expr_ternary(Emitter *self, Expr *expr) {
    bool is_pointer = is_ptr(hmget(self->package->operands, expr->eternary.econd).type);
    bool prev_return_address = self->return_address;
    self->return_address = true;
    IRValue *cond = emit_expr(self, expr->eternary.econd);
    IRValue *pass = cond;
    if (expr->eternary.epass)
        pass = emit_expr(self, expr->eternary.epass);
    IRValue *fail = emit_expr(self, expr->eternary.efail);
    self->return_address = prev_return_address;
    if (is_pointer) {
        cond = llvm_create_ptr_to_int(self->context, cond, llvm_i1(self->context));
    } else {
        cond = llvm_create_trunc_or_bitcast(self->context, cond, llvm_i1(self->context));
    }
    return llvm_create_select(self->context, cond, pass, fail);
}

IRValue *emit_expr_call(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_field(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_index(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_slice(Emitter *self, Expr *expr) { return NULL; }

IRFunction *emit_expr_func(Emitter *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    IRFunction *prev_fn = self->function;

    IRType *type = llvm_type(self->context, operand.type);
    const char *name = NULL;
    if (self->symbol) name = self->symbol->external_name ?: self->symbol->name;
    IRFunction *fn = llvm_create_function(self->context, type, name);
    self->function = fn;

    if (compiler.flags.debug) {
        DBGType *dbg_type = llvm_debug_type(self->context, operand.type);
        PosInfo pos = package_posinfo(self->package, expr->range.start);
        llvm_debug_function(self->context, fn, dbg_type, name, pos.line);
    }

    bool is_void = operand.type->tfunc.result->kind == TYPE_VOID;
    IRBlock *block = llvm_create_block(self->context, "entry", self->function);
    llvm_set_insert_block(self->context, block);

    IRFunctionArgs *args = llvm_function_args_start(self->context, fn);

    for (i64 i = 0; i < arrlen(expr->efunc.type->efunctype.params); i++) {
        FuncParam param = expr->efunc.type->efunctype.params[i];

        llvm_param_set_name(args, param.name->estr.str);

    }

    self->function = prev_fn;

    return (IRFunction *) fn;
}

IRValue *emit_expr_functype(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_slicetype(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_array(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_pointer(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_struct(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_union(Emitter *self, Expr *expr) { return NULL; }
IRValue *emit_expr_enum(Emitter *self, Expr *expr) { return NULL; }

IRValue *emit_expr(Emitter *self, Expr *expr) {
    switch (expr->kind) {
        case EXPR_NIL:       return emit_expr_nil(self, expr);
        case EXPR_INT:       return emit_expr_int(self, expr);
        case EXPR_FLOAT:     return emit_expr_float(self, expr);
        case EXPR_STR:       return emit_expr_str(self, expr);
        case EXPR_NAME:      return emit_expr_name(self, expr);
        case EXPR_COMPOUND:  return emit_expr_compound(self, expr);
        case EXPR_CAST:      return emit_expr_cast(self, expr);
        case EXPR_PAREN:     return emit_expr_paren(self, expr);
        case EXPR_UNARY:     return emit_expr_unary(self, expr);
        case EXPR_BINARY:    return emit_expr_binary(self, expr);
        case EXPR_TERNARY:   return emit_expr_ternary(self, expr);
        case EXPR_CALL:      return emit_expr_call(self, expr);
        case EXPR_FIELD:     return emit_expr_field(self, expr);
        case EXPR_INDEX:     return emit_expr_index(self, expr);
        case EXPR_SLICE:     return emit_expr_slice(self, expr);
        case EXPR_FUNC:      return (IRValue *) emit_expr_func(self, expr);
        case EXPR_FUNCTYPE:  return emit_expr_functype(self, expr);
        case EXPR_SLICETYPE: return emit_expr_slicetype(self, expr);
        case EXPR_ARRAY:     return emit_expr_array(self, expr);
        case EXPR_POINTER:   return emit_expr_pointer(self, expr);
        case EXPR_STRUCT:    return emit_expr_struct(self, expr);
        case EXPR_UNION:     return emit_expr_union(self, expr);
        case EXPR_ENUM:      return emit_expr_enum(self, expr);
        default:
            fatal("Unrecognized ExprKind %s", describe_ast_kind(expr->kind));
    }
}

void emit_decl_var(Emitter *self, Decl *decl) {}
void emit_decl_val(Emitter *self, Decl *decl) {}
void emit_decl_import(Emitter *self, Decl *decl) {}
void emit_decl_library(Emitter *self, Decl *decl) {}
void emit_decl_foreign(Emitter *self, Decl *decl) {}
void emit_decl_foreignblock(Emitter *self, Decl *decl) {}

void emit_decl(Emitter *self, Decl *decl) {
    switch (decl->kind) {
        case DECL_VAR: return emit_decl_var(self, decl);
        case DECL_VAL: return emit_decl_val(self, decl);
        case DECL_IMPORT: return emit_decl_import(self, decl);
        case DECL_LIBRARY: return emit_decl_library(self, decl);
        case DECL_FOREIGN: return emit_decl_foreign(self, decl);
        case DECL_FOREIGNBLOCK: return emit_decl_foreignblock(self, decl);
        default:
            fatal("Unrecognized DeclKind %s", describe_ast_kind(decl->kind));
    }
}

void llvm_emit_stmt(Emitter *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_VAL:
            emit_decl_val(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_IMPORT:
            break;
        default:
            if (stmt->kind < STMT_KIND_BASE)
                emit_expr(self, (Expr *) stmt);
            fatal("Unrecognized StmtKind %s", describe_ast_kind(stmt->kind));
    }
}

bool llvm_build_module(Package *package) {
    IRContext *context = llvm_backend_init(package);
    package->emitter = malloc(sizeof *package->emitter);
    package->emitter->package = package;
    package->emitter->context = context;
    for (int i = 0; i < arrlen(package->stmts); i++) {
        llvm_emit_stmt(package->emitter, package->stmts[i]);
    }
    return llvm_validate(context);
}

void llvm_emit_object(Package *package) {

}
