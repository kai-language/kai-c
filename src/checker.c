
#include "all.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "compiler.h"
#include "ast.h"
#include "types.h"
#include "checker.h"
#include "string.h"

typedef struct Checker Checker;
struct Checker {
    Package *package;
    u32 flags;

    Scope *scope;
    Ty *wanted_type;

    Expr **efuncs; // arr
    Stmt **sgotos; // arr StmtGoto (stack of goto's needing resolve @ end of checking func)
    Stmt **sdefer; // arr

    // TODO: Improve naming
    Decl *current_decl;

    Stmt **sswitch; // arr
    SwitchCase *snext_case;

    Stmt **sfor; // arr
    Ty *current_type; // TODO: Use this for detecting cycles for types
};

Operand check_stmt(Checker *self, Stmt *stmt);
Operand check_expr(Checker *self, Expr *expr);
Operand check_expr_func_type(Checker *self, Expr *expr);
Operand check_decl_var(Checker *self, Decl *decl);
Operand check_decl_val(Checker *self, Decl *decl);
Operand check_decl_foreign(Checker *self, Decl *decl);
Operand check_decl_foreign_block(Checker *self, Decl *decl);

#define error(self, range, fmt, ...) add_error(self->package, range, fmt, ##__VA_ARGS__)
#define note(self, range, fmt, ...) add_note(self->package, range, fmt, ##__VA_ARGS__)

bool (*unary_predicates[NUM_OPS])(Ty *) = {
    [OP_ADD]  = is_arithmetic,
    [OP_SUB]  = is_arithmetic,
    [OP_BNOT] = is_integer,
    [OP_NOT]  = is_arithmetic,
    [OP_LSS]  = is_ptr,
};

bool (*binary_predicates[NUM_OPS])(Ty *) = {
    [OP_ADD] = is_arithmetic,
    [OP_SUB] = is_arithmetic,
    [OP_MUL] = is_arithmetic,
    [OP_DIV] = is_arithmetic,
    [OP_REM] = is_integer,

    [OP_AND] = is_bitwisable,
    [OP_OR]  = is_bitwisable,
    [OP_XOR] = is_bitwisable,
    [OP_SHL] = is_bitwisable,
    [OP_SHR] = is_bitwisable,

    [OP_EQL] = is_equatable,
    [OP_NEQ] = is_equatable,

    [OP_LSS] = is_comparable,
    [OP_GTR] = is_comparable,
    [OP_LEQ] = is_comparable,
    [OP_GEQ] = is_comparable,

    [OP_LOR]  = is_logical,
    [OP_LAND] = is_logical,
};

Operand operand_ok = { NULL, .flags = OPERAND_OK };
Operand bad_operand = { &(Ty){ TYPE_INVALID }, .flags = BAD_VALUE };
Operand operand_unchecked = { &(Ty){ TYPE_INVALID }, .flags = UNCHECKED };

bool check(Package *package, Stmt *stmt) {
    TRACE1(CHECKING, STR("package.path", package->path));
    Checker checker = {
        .package = package,
        .flags = NONE,
        .scope = package->scope,
        .current_decl = (Decl *) stmt,
    };
    return check_stmt(&checker, stmt).flags == UNCHECKED; // requeue
}

INLINE
void push_scope(Checker *self) {
    TRACE(CHECKING);
    Scope *scope = arena_calloc(&self->package->arena, sizeof *scope);
    scope->parent = self->scope;
    self->scope = scope;
}

INLINE
void pop_scope(Checker *self) {
    TRACE(CHECKING);
    self->scope = self->scope->parent;
}

Scope *scope_push(Package *package, Scope *parent) {
    TRACE(CHECKING);
    Scope *scope = arena_calloc(&package->arena, sizeof *scope);
    scope->parent = parent;
    return scope;
}

void scope_declare(Scope *scope, Sym *sym) {
    SymMapEntry entry = {sym->name, sym};
    hmputs(scope->members, entry);
}

void symbol_mark_checked(Sym *sym, Operand op) {
    if (op.flags == TYPE) {
        sym->kind = SYM_TYPE;
    }
    sym->type = op.type;
    sym->state = SYM_CHECKED;
}

Sym *scope_lookup(Scope *scope, const char *name) {
    do {
        Sym *sym = hmget(scope->members, name);
        if (sym) return sym;
        scope = scope->parent;
    } while (scope);
    return NULL;
}

Sym *checker_sym(Checker *self, Expr *name, Ty *type, SymKind kind) {
    ASSERT(name->kind == EXPR_NAME);
    Sym *sym;
    if (self->scope == self->package->scope) {
        sym = scope_lookup(self->scope, name->ename);
        sym->type = type;
        ASSERT(sym && sym->kind == kind);
    } else {
        sym = arena_calloc(&self->package->arena, sizeof *sym);
        sym->state = SYM_CHECKED;
        sym->name = name->ename;
        sym->type = type;
        sym->kind = kind;
        sym->owning_package = self->package;
        scope_declare(self->scope, sym);
    }
    hmput(self->package->symbols, name, sym);
    return sym;
}

i64 sext(Ty *src, Val val) {
    if (src->size == 8) return val.i;
    u64 v = val.u & ((1ull << (src->size * 8)) - 1);
    u64 mask = 1ull << ((src->size * 8) - 1);
    return (v ^ mask) - mask;
}

Val resolve_value(Package *package, Expr *expr) {
    Val val = {0};
    switch (expr->kind) {
        case EXPR_NIL:
            val.p = 0;
            break;
        case EXPR_INT:
            val.u = expr->eint;
            break;
        case EXPR_STR:
            if (expr->estr.mapped) {
                char *mem = arena_alloc(&package->arena, expr->estr.len + 1);
                memcpy(mem, expr->estr.str, expr->estr.len);
                mem[expr->estr.len] = '\0';
                expr->estr.str = mem;
                expr->estr.mapped = false;
            }
            val.p = (void *) expr->estr.str;
            break;
        case EXPR_FLOAT:
            val.f = expr->efloat;
            break;
        default:
            break;
    }
    return val;
}

void eval_unary(Operand *operand, Op op) {

}

void eval_binary(Operand *lhs, Operand rhs, Op op) {

}

INLINE
Operand operand(Checker *self, Expr *expr, Ty *type, OperandFlags flags) {
    Operand op = { type, flags, .val.i = 0 };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandi(Checker *self, Expr *expr, Ty *type, OperandFlags flags, i64 val) {
    Operand op = { type, flags, .val.i = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandu(Checker *self, Expr *expr, Ty *type, OperandFlags flags, i64 val) {
    Operand op = { type, flags, .val.u = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandf(Checker *self, Expr *expr, Ty *type, OperandFlags flags, f64 val) {
    Operand op = { type, flags, .val.f = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandp(Checker *self, Expr *expr, Ty *type, OperandFlags flags, void *val) {
    Operand op = { type, flags, .val.p = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandv(Checker *self, Expr *expr, Ty *type, OperandFlags flags, Val val) {
    Operand op = { type, flags, val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
bool ret_operand(Operand op) {
    if (op.flags == UNCHECKED)
        EVENT(CHECKING, "Encountered unchecked work will be lost");
    return op.flags == UNCHECKED || op.flags == BAD_VALUE;
}

INLINE void expect_operand_is_a_type(Checker *self, Operand operand, Expr *expr) {
    TRACE(CHECKING);
    if (operand.flags != TYPE) {
        error(self, expr->range, "'%s' cannot be used as a type",
              describe_ast(self->package, expr));
    }
}

INLINE bool operand_coerces(Checker *self, Operand operand, Ty *dst) {
    TRACE(CHECKING);
    Ty *src = operand.type;
    if (src == dst) return true;
    if (dst->kind == TYPE_ANY) return true;
    if (src->kind == TYPE_STRUCT && (src->flags&SINGLE) != 0) {
        src = src->taggregate.fields->type;
    }
    switch (src->kind) {
        case TYPE_INVALID:
            return true; // avoid further errors
        case TYPE_COMPLETING:
            return true; // avoid further errors
        case TYPE_VOID:
            return false; // void can coerce to nothing.
        case TYPE_BOOL:
            return false;
        case TYPE_INT:
            switch (dst->kind) {
                case TYPE_BOOL: return true;
                case TYPE_ENUM: // fallthrough;
                case TYPE_INT: {
                    bool val_const = (operand.flags&CONST) != 0;
                    bool dst_signed = (dst->flags&SIGNED) != 0;
                    bool src_signed = (src->flags&SIGNED) != 0;
                    if (src_signed && !dst_signed) { // signed to unsigned
                        // conversion allow constants with no LOI
                        if (val_const && operand.val.u <= type_max_value(dst)) {
                            i64 val = sext(src, operand.val);
                            if (val >= 0) {
                                operand.val.i = val;
                                return true;
                            }
                        }
                        return false;
                    }
                    if (!src_signed && dst_signed) { // unsigned to signed
                        // constant where value is fit's within the dst type.
                        if (val_const && operand.val.u <= type_max_value(dst)) return true;
                        // widening is allowed.
                        return src->size < dst->size;
                    }
                    // Same signedness allow same width or widening
                    return src->size <= dst->size;
                }
                case TYPE_FLOAT: return true;
                default: return false;
            }
        case TYPE_FLOAT: return (dst->kind == TYPE_FLOAT) && src->size <= dst->size;
        case TYPE_PTR:   return src == type_rawptr || dst == type_rawptr;
        case TYPE_FUNC:
        case TYPE_ARRAY:
        case TYPE_SLICE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ANY:
            return false;
        default: fatal("Unhandled case");
    }
}

INLINE void expect_operand_casts(Checker *self, Operand op, Ty *type, Expr *expr) {
    TRACE(CHECKING);
}

INLINE void expect_type_coerces(Checker *self, Ty *type, Ty *target, Expr *expr) {
    TRACE(CHECKING);
}

INLINE void expect_operand_coerces(Checker *self, Operand op, Ty *type, Expr *expr) {
    TRACE(CHECKING);
}

INLINE void expect_constant(Checker *self, Operand op, Expr *expr) {
    TRACE(CHECKING);
}

INLINE void expect_operand_lvalue(Checker *self, Operand op, Expr *expr) {
    TRACE(CHECKING);
}

Operand check_expr_nil(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    if (self->wanted_type && !is_ptr(self->wanted_type)) {
        add_error(self->package, expr->range,
                  "'nil' is not convertable to '%s'", tyname(self->wanted_type));
        return bad_operand;
    }
    Ty *type = self->wanted_type ?: type_rawptr;
    return operandi(self, expr, type, CONST, 0);
}

Operand check_expr_int(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Val val = {expr->eint};
    Ty *type = smallest_unsigned_int_for_value(val.u);
    if (self->wanted_type) {
        if (is_integer(self->wanted_type) || is_ptr(self->wanted_type)) {
            if (val.u > type_max_value(self->wanted_type)) {
                error(self, expr->range,
                      "Cannot coerce value '%d' to type %s as loss of information would occur",
                      val.u, tyname(self->wanted_type));
                note(self, expr->range, "Cast to force conversion with overflow");
            }
            type = self->wanted_type;
        }
    } else if (is_float(self->wanted_type)) {
        val.f = (f64)val.u;
        type = self->wanted_type;
    }
    return operandv(self, expr, type, CONST, val);
}

Operand check_expr_float(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Ty *type = type_f64;
    if (is_float(self->wanted_type)) type = self->wanted_type;
    return operandf(self, expr, type, CONST, expr->efloat);
}

Operand check_expr_str(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Ty *type = type_string;
    if (self->wanted_type == type_rawptr) type = type_rawptr;
    return operandp(self, expr, type, CONST, expr->estr.str);
}

Operand check_expr_name(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Sym *sym = scope_lookup(self->scope, expr->ename);
    if (!sym) {
        error(self, expr->range, "Undefined name '%s'", expr->ename);
        return bad_operand;
    }
    sym->reachable = REACHABLE_NATURAL;
    if (sym->state == SYM_UNCHECKED) return operand_unchecked;
    if (sym->state == SYM_CHECKING) {
        error(self, sym->decl->range, "Cyclic dependency for symbol '%s'", sym->name);
        return bad_operand;
    }
    hmput(self->package->symbols, expr, sym);
    OperandFlags flags = NONE;
    switch (sym->kind) {
        case SYM_VAL: flags |= CONST; break;
        case SYM_VAR: flags |= LVALUE; break;
        case SYM_ARG: flags |= LVALUE; break;
        case SYM_PKG: flags |= PACKAGE; goto special;
        case SYM_LIB: flags |= LIBRARY; goto special;
        case SYM_TYPE: flags = TYPE; break;
        case SYM_LABEL: flags |= LABEL; break;
        default: fatal("Unhandled SymKind %d", sym->kind);
    }
    return operandv(self, expr, sym->type, flags, sym->val);
special:
    return operandp(self, expr, sym->type, flags, sym);
}

Operand check_expr_compound(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Ty *type = self->wanted_type;
    if (expr->ecompound.type) {
        Operand op = check_expr(self, expr->ecompound.type);
        if (ret_operand(op)) return op;
        type = op.type;
        expect_operand_is_a_type(self, op, expr);
    } else if (type == NULL) {
        error(self, expr->range,
              "Implicitely type compound literal used in context without expected type");
        return bad_operand;
    }
    switch (type->kind) {
        case TYPE_UNION:
        case TYPE_STRUCT: {
            u32 index = 0;
            for (int i = 0; i < arrlen(expr->ecompound.fields); i++) {
                CompoundField field = expr->ecompound.fields[i];
                if (field.kind == FIELD_INDEX) {
                    error(self, field.key->range,
                          "Cannot initialize struct/union members by index");
                    goto check_agg_value;
                } else if (field.kind == FIELD_NAME) {
                    index = aggregate_field_index(type, field.key->ename);
                    if (index < 0) {
                        error(self, field.key->range, "No field named '%s' exists in type %s",
                              field.key->ename, tyname(type));
                        goto check_agg_value;
                    }
                }
                if (index >= arrlen(type->taggregate.fields)) {
                    error(self, field.val->range,
                          "No field at index '%ld' in type %s", index, tyname(type));
                    goto check_agg_value;
                }
            check_agg_value:;
                self->wanted_type = type->taggregate.fields[index].type;
                Operand op = check_expr(self, field.val);
                if (ret_operand(op)) return op;
                expect_operand_coerces(self, op, type->taggregate.fields[index].type, field.val);
                index++;
            }
            break;
        }
        case TYPE_ARRAY: {
            u32 index = 0;
            u32 max_index = 0;
            for (u32 i = 0; i < arrlen(expr->ecompound.fields); i++) {
                CompoundField field = expr->ecompound.fields[i];
                if (field.kind == FIELD_NAME) {
                    error(self, field.key->range, "Cannot initialize array members with names");
                    note(self, field.key->range, "Array or Slice key should be surrounded in `[]`");
                    goto check_arr_value;
                } else if (field.kind == FIELD_INDEX) {
                    Operand op = check_expr(self, field.key);
                    if (ret_operand(op)) return op;
                    expect_operand_coerces(self, op, type_u64, field.key);
                    expect_constant(self, op, field.key);
                    if (op.val.i < 0) {
                        error(self, field.key->range, "Index cannot be negative");
                        goto check_arr_value;
                    }
                    index = (u32) op.val.i;
                }
                if (type->tarray.length && index >= type->tarray.length) {
                    error(self, field.key->range,
                          "Array index %lu is beyond the max index of %lu for type %s",
                          index, type->tarray.length, tyname(type));
                }
            check_arr_value:;
                self->wanted_type = type->tarray.eltype;
                Operand op = check_expr(self, field.val);
                if (ret_operand(op)) return op;
                expect_operand_coerces(self, op, type->tarray.eltype, field.val);
                max_index = MAX(max_index, index);
                index++;
            }
            break;
        }
    }
    return operand(self, expr, type, LVALUE);
}

Operand check_expr_cast(Checker *self, Expr *expr) { return bad_operand; }

Operand check_expr_paren(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand op = check_expr(self, expr->eparen);
    if (ret_operand(op)) return op;
    return operandv(self, expr, op.type, op.flags, op.val);
}

Operand check_expr_unary(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand value = check_expr(self, expr->eunary);
    if (ret_operand(value)) return value;
    Op op = (Op) expr->flags;
    Ty *type = NULL;
    switch (op) {
        case OP_AND: {
            if ((value.flags & LVALUE) == 0) {
                error(self, expr->range, "Cannot take address of '%s'",
                      describe_ast(self->package, expr));
                return bad_operand;
            }
            Ty *type = type_ptr(value.type, NONE);
            return operand(self, expr, type, NONE);
        }
        default: {
            if (!unary_predicates[op](value.type)) {
                error(self, expr->range, "Operation '%s' undefined for type %s",
                      describe_op(expr->flags), tyname(value.type));
                return bad_operand;
            }
            if (expr->flags == OP_NOT) {
                type = type_bool;
            } else if (expr->flags == OP_LSS) {
                type = value.type->tptr.base;
            }
        }
    }
    if (value.flags&CONST) {
        eval_unary(&value, op);
        return operandv(self, expr, type, CONST, value.val);
    }
    return operand(self, expr, type, NONE);
}

Operand check_expr_binary(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Op op = (Op) expr->flags;
    self->wanted_type = NULL;
    Operand lhs = check_expr(self, expr->ebinary.elhs);
    if (ret_operand(lhs)) return lhs;
    Operand rhs = check_expr(self, expr->ebinary.erhs);
    if (ret_operand(rhs)) return rhs;
    if (!(operand_coerces(self, lhs, rhs.type) || operand_coerces(self, rhs, lhs.type))) {
        error(self, expr->range, "No coercion can occur to make %s and %s identical types",
              tyname(lhs.type), tyname(rhs.type));
        return bad_operand;
    }
    Ty *type = lhs.type; // both sides of the expression have this type.
    if (!binary_predicates[expr->flags](type)) {
        error(self, expr->range, "Operation '%s' undefined for type %s",
              describe_op(expr->flags), tyname(type));
        return bad_operand;
    }
    switch (op) {
        case OP_EQL: case OP_NEQ: case OP_LEQ: case OP_GEQ:
        case OP_LSS: case OP_GTR: case OP_LOR: case OP_LAND:
            type = type_bool;
            break;
        default:
            break;
    }
    if ((op == OP_DIV || op == OP_REM) && rhs.flags&CONST && rhs.val.i == 0)
        error(self, expr->range, "Division by zero");
    if (lhs.flags&CONST && rhs.flags&CONST) {
        eval_binary(&lhs, rhs, op);
        return operandv(self, expr, type, CONST, lhs.val);
    }
    return operand(self, expr, type, NONE);
}

Operand check_expr_ternary(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Ty *wanted_type = self->wanted_type;
    self->wanted_type = type_bool;
    Operand cond = check_expr(self, expr->eternary.econd);
    if (ret_operand(cond)) return cond;
    Operand pass = cond;
    if (expr->eternary.epass) {
        self->wanted_type = wanted_type;
        pass = check_expr(self, expr->eternary.epass);
        if (ret_operand(pass)) return pass;
    }
    self->wanted_type = wanted_type;
    Operand fail = check_expr(self, expr->eternary.efail);
    if (ret_operand(fail)) return fail;
    if (!is_bool(cond.type) && !is_arithmetic(cond.type) && !is_ptr(cond.type)) {
        error(self, expr->range, "Expected numeric or pointer type as condition");
        return bad_operand;
    }
    if (!operand_coerces(self, fail, pass.type)) {
        error(self, expr->eternary.efail->range, "Expected type %s got type %s",
              tyname(pass.type), tyname(fail.type));
        return bad_operand;
    }
    if (cond.flags&CONST && pass.flags&CONST && fail.flags&CONST) {
        return operandv(self, expr, pass.type, CONST, cond.val.u ? pass.val : fail.val);
    }
    return operand(self, expr, pass.type, NONE);
}

Operand check_expr_call(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand callee = check_expr(self, expr->ecall.expr);
    if (ret_operand(callee)) return callee;
    if (callee.flags&TYPE) { // Call is a cast
        if (arrlen(expr->ecall.args) < 1 || arrlen(expr->ecall.args) > 1) {
            error(self, expr->range, "Expected 1 argument in cast to '%s'", tyname(callee.type));
            return bad_operand;
        }
        ExprCall call = expr->ecall;
        if (call.args->name != NULL) {
            error(self, call.args->name->range, "Unexpected argument label for cast to '%s'");
        }
        expr->kind = EXPR_CAST;
        expr->ecast.expr = call.args->expr;
        expr->ecast.type = call.expr;
        self->wanted_type = callee.type;
        Operand arg = check_expr(self, expr->ecast.expr);
        if (ret_operand(arg)) return arg;
        expect_operand_casts(self, arg, callee.type, expr->ecast.expr);
        return operand(self, expr, callee.type, NONE);
    }
    if (callee.type->kind != TYPE_FUNC) {
        error(self, expr->ecall.expr->range,
              "Cannot call non function type '%s'", tyname(callee.type));
        return bad_operand;
    }
    u32 num_params = (u32) arrlen(callee.type->tfunc.params);
    for (u32 i = 0; i < arrlen(expr->ecall.args); i++) {
        CallArg call_arg = expr->ecall.args[i];
        if (i >= num_params && (callee.type->flags & FUNC_VARGS) == 0) {
            error(self, call_arg.expr->range, "Too many arguments in call to '%s', expected %ld",
                  tyname(callee.type), num_params);
            break;
        }
        self->wanted_type = callee.type->tfunc.params[MIN(num_params - 1, i)];
        if (self->wanted_type->kind == TYPE_SLICE && self->wanted_type->flags & FUNC_VARGS) {
            self->wanted_type = self->wanted_type->tslice.eltype;
        }
        Operand arg = check_expr(self, call_arg.expr);
        if (ret_operand(arg)) return arg;
        expect_operand_coerces(self, arg, self->wanted_type, call_arg.expr);
    }
    Ty *result = callee.type->tfunc.result;
    if (arrlen(result->taggregate.fields) == 1) result = result->taggregate.fields[0].type;
    return operand(self, expr, result, NONE);
}

Operand check_expr_field(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand base = check_expr(self, expr->efield.expr);
    if (ret_operand(base)) return base;
    const char *name = expr->efield.name->ename;
    if (base.flags == PACKAGE) {
        Sym *file = base.val.p;
        Package *import = file->package;
        if (!import) return bad_operand;
        Sym *sym = hmget(import->scope->members, name);
        if (!sym) {
            for (u32 i = 0; i < arrlen(sym->decl->dimport.items); i++) {
                ImportItem item = sym->decl->dimport.items[i];
                if (item.alias->ename == expr->efield.name->ename) {
                    goto found;
                }
            }
            error(self, expr->efield.name->range, "Import has no member '%s'", name);
            return bad_operand;
        }
    found:;
        switch (sym->state) {
            case SYM_UNCHECKED:
                return operand_unchecked;
            case SYM_CHECKING:
                error(self, expr->range, "Declaration initial value refers to itself");
                return bad_operand;
            case SYM_CHECKED: break;
        }
        OperandFlags flags = NONE;
        switch (sym->kind) {
            case SYM_TYPE:  flags |= TYPE;   break;
            case SYM_VAR:   flags |= LVALUE; break;
            case SYM_VAL:   flags |= CONST;  break;
            case SYM_ARG:   flags |= LVALUE; break;
            case SYM_PKG:   fatal("Expected to be unable to reference imports from imports");
            case SYM_LIB:   fatal("Unhandled");
            case SYM_LABEL: fatal("Unhandled");
            default:        fatal("Unhandled");
        }
        hmput(self->package->symbols, expr->efield.name, sym);
        return operandv(self, expr, sym->type, flags, sym->val);
    }
    if (base.flags == LIBRARY) {
        error(self, expr->efield.expr->range, "Libraries types have no members");
        return bad_operand;
    }
    if (is_aggregate(base.type)) {
        u32 index = 0;
        TyField *field = NULL;
        for (u32 i = 0; i < arrlen(base.type->taggregate.fields); i++) {
            if (base.type->taggregate.fields[i].name == name) {
                index = i;
                field = base.type->taggregate.fields + i;
                goto found_aggregate_field;
            }
        }
        error(self, expr->range, "Aggregate type %s has no field '%s'", tyname(base.type), name);
        return bad_operand;
    found_aggregate_field:;
        return operand(self, expr, field->type, base.flags);
    }
    error(self, expr->range, "%s has no field '%s'", tyname(base.type), name);
    return bad_operand;
}

Operand check_expr_index(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand base = check_expr(self, expr->eindex.expr);
    if (ret_operand(base)) return base;
    Operand index = check_expr(self, expr->eindex.index);
    if (ret_operand(index)) return index;
    if (!is_integer(index.type)) {
        error(self, expr->eindex.index->range, "Cannot index with non integer type '%s'",
              tyname(index.type));
        return bad_operand;
    }
    Ty *type;
    switch (base.type->kind) {
        case TYPE_ARRAY: {
            type = base.type->tarray.eltype;
            if (index.flags&CONST && (index.val.i < 0 || index.val.u >= base.type->tarray.length)) {
                error(self, expr->range, "Index %d is out of the bounds for type %s",
                      tyname(base.type));
                return bad_operand;
            }
            break;
        }
        case TYPE_SLICE: type = base.type->tslice.eltype; break;
        case TYPE_PTR: type = base.type->tptr.base; break;
        default:
            error(self, expr->eindex.expr->range, "Unable to index type '%s'", tyname(base.type));
            return bad_operand;
    }
    return operand(self, expr, type, NONE);
}

Operand check_expr_func(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    push_scope(self); // allow shadowing by declaring a separate params scope
    Operand type = check_expr_func_type(self, expr->efunc.type);
    if (ret_operand(type)) return type;
    for (u32 i = 0; i < arrlen(type.type->tfunc.params); i++) {
        TyFunc func = type.type->tfunc;
        Expr *name = expr->efunc.type->efunctype.params[i].name;
        Sym *sym = checker_sym(self, name, func.params[i], SYM_ARG);
        sym->type = type.type->tfunc.params[i];
        scope_declare(self->scope, sym);
    }
    push_scope(self);
    Stmt **prev_gotos = self->sgotos;
    arrpush(self->efuncs, expr);
    Operand body = check_stmt(self, expr->efunc.body);
    if (ret_operand(body)) return body;
    arrpop(self->efuncs);
    for (i64 i = 0; i < arrlen(self->sgotos); i++) { // resolve goto's
        Operand op = check_expr_name(self, self->sgotos[i]->sgoto);
        if (op.flags != LABEL) error(self, self->sgotos[i]->sgoto->range, "Expected label");
    }
    pop_scope(self);
    pop_scope(self);
    self->sgotos = prev_gotos;
    return operand(self, expr, type.type, NONE);
}

Operand check_expr_func_type(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Ty **params = NULL;
    const char **labels = NULL;
    TyField *result_fields = NULL;
    arrsetcap(params, arrlen(expr->efunctype.params));
    arrsetcap(result_fields, arrlen(expr->efunctype.result));
    for (u32 i = 0; i < arrlen(expr->efunctype.params); i++) {
        Operand param = check_expr(self, expr->efunctype.params[i].type);
        if (ret_operand(param)) return param;
        expect_operand_is_a_type(self, param, expr->efunctype.params[i].type);
        if (expr->efunctype.params[i].name)
            arrput(labels, expr->efunctype.params[i].name->ename);
        arrput(params, param.type);
    }
    for (u32 i = 0; i < arrlen(expr->efunctype.result); i++) {
        Operand result = check_expr(self, expr->efunctype.result[i].type);
        if (ret_operand(result)) return result;
        expect_operand_is_a_type(self, result, expr->efunctype.result[i].type);
        TyField field = { NULL, result.type };
        arrput(result_fields, field);
    }
    Ty *result = NULL;
    if (result_fields && result_fields[0].type == type_void) {
        arrfree(result_fields);
        result = type_void;
    } else {
        result = type_struct(result_fields, 0, 0, TUPLE);
    }
    Ty *type = type_func(labels, params, result, (FuncFlags) expr->flags);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_array(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand element = check_expr(self, expr->earray.base);
    if (ret_operand(element)) return element;
    expect_operand_is_a_type(self, element, expr->earray.base);
    if (!expr->earray.len) fatal("Unimplemented"); // TODO: Implicitly sized arrays
    Operand length = check_expr(self, expr->earray.len);
    if (ret_operand(length)) return length;
    expect_constant(self, length, expr->earray.len);
    Ty *type = type_array(element.type, length.val.u, TYPE);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_pointer(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    // FIXME: ??? ALLOW UNRESOLVED TYPES
    Operand base = check_expr(self, expr->epointer.base);
    if (ret_operand(base)) return base;
    expect_operand_is_a_type(self, base, expr->epointer.base);
    Ty *type = type_ptr(base.type, NONE);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_struct(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    TyField *fields = NULL;
    arrsetcap(fields, arrlen(expr->estruct.fields));
    u64 align = 0;
    u64 width = 0;
    for (i64 i = 0; i < arrlen(expr->estruct.fields); i++) {
        Operand type = check_expr(self, expr->estruct.fields->type);
        if (ret_operand(type)) return type;
        align = MAX(align, type.type->align);
        for (i64 j = 0; j < arrlen(expr->estruct.fields->names); j++) {
            u64 offset = ALIGN_UP(width, type.type->align);
            TyField field = {expr->estruct.fields->names[j]->ename, type.type, offset};
            width = offset + type.type->size;
            arrput(fields, field);
        }
    }
    Ty *type = type_struct(fields, (u32) width, (u32) align, NONE);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_union(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    TyField *fields = NULL;
    arrsetcap(fields, arrlen(expr->estruct.fields));
    u64 align = 0;
    u64 width = 0;
    for (i64 i = 0; i < arrlen(expr->estruct.fields); i++) {
        Operand type = check_expr(self, expr->estruct.fields->type);
        if (ret_operand(type)) return type;
        align = MAX(align, type.type->align);
        for (i64 j = 0; j < arrlen(expr->estruct.fields->names); j++) {
            TyField field = {expr->estruct.fields->names[i]->ename, type.type};
            width = MAX(width, type.type->size);
            arrput(fields, field);
        }
    }
    Ty *type = type_union(fields, (u32) width, (u32) align, NONE);
    return operand(self, expr, type, NONE);
}

Operand check_expr_enum(Checker *self, Expr *expr) { return bad_operand; }

Operand check_decl_val(Checker *self, Decl *decl) {
    TRACE1(CHECKING, STR("val", decl->dval.name->ename));
    Decl *prev_current_decl = self->current_decl;
    self->current_decl = decl;
    Sym *sym = checker_sym(self, decl->dval.name, NULL, SYM_VAL);
    Ty *type = NULL;
    if (decl->dval.type) {
        Operand ty = check_expr(self, decl->dval.type);
        if (ret_operand(ty)) return ty;
        expect_operand_is_a_type(self, ty, decl->dval.type);
        type = ty.type;
    }
    Operand op = check_expr(self, decl->dval.val);
    if (ret_operand(op)) return op;
    if (type) expect_operand_coerces(self, op, type, decl->dval.val);
    sym->type = op.type;
    sym->state = SYM_CHECKED;
    symbol_mark_checked(sym, op);
    self->current_decl = prev_current_decl;
    return operand_ok;
}

Operand check_decl_var(Checker *self, Decl *decl) {
    TRACE(CHECKING);
    Ty *type = NULL;
    if (decl->dvar.type) {
        Operand ty = check_expr(self, decl->dvar.type);
        if (ret_operand(ty)) return ty;
        expect_operand_is_a_type(self, ty, decl->dvar.type);
        type = ty.type;
    }
    i64 num_names = arrlen(decl->dvar.names);
    i64 num_values = arrlen(decl->dvar.vals);
    if (num_names > 1 && num_values == 1 && decl->dvar.vals[0]->kind == EXPR_CALL) {
        Operand call = check_expr_call(self, decl->dvar.vals[0]);
        if (ret_operand(call)) return call;
        if (call.type->kind != TYPE_STRUCT || call.type->flags != TUPLE) {
            error(self, decl->range,
                  "Assignment count mismatch %lld names but call returns %lld value",
                  num_names, 1);
            return bad_operand;
        }
        num_values = arrlen(call.type->taggregate.fields);
        if (arrlen(call.type->taggregate.fields) != num_names) {
            error(self, decl->range,
                  "Assignment count mismatch %lld names but call returns %lld values",
                  num_names, num_values);
        }
        for (i64 i = 0; i < num_names; i++) {
            Ty *rhs_type = call.type->taggregate.fields[i].type;
            if (type) expect_type_coerces(self, rhs_type, type, decl->dvar.vals[0]);
            Sym *sym = checker_sym(self, decl->dvar.vals[i], type ?: rhs_type, SYM_VAR);
            sym->state = SYM_CHECKED;
        }
        return operand_ok;
    } else if (type && num_values == 0) {
        for (i64 i = 0; i < num_names; i++) {
            Sym *sym = checker_sym(self, decl->dvar.names[i], type, SYM_VAR);
            sym->state = SYM_CHECKED;
        }
        return operand_ok;
    } else if (num_names > num_values || num_names < num_values) {
        error(self, decl->range, "Assignment count mismatch %lld = %lld", num_names, num_values);
    }
    for (i64 i = 0; i < num_names; i++) {
        Expr *expr = decl->dvar.vals[MIN(i, num_values)];
        Sym *sym = checker_sym(self, decl->dvar.names[i], NULL, SYM_VAR);
        Operand op = check_expr(self, expr);
        if (ret_operand(op)) return op;
        symbol_mark_checked(sym, op);
        if (type) expect_operand_coerces(self, op, type, expr);
    }
    return operand_ok;
}

Operand check_decl_foreign(Checker *self, Decl *decl) {
    TRACE1(CHECKING, STR("val", decl->dforeign.name->ename));
    Operand type = check_expr(self, decl->dforeign.type);
    if (ret_operand(type)) return type;
    expect_operand_is_a_type(self, type, decl->dforeign.type);
    SymKind kind = decl->flags&DECL_CONSTANT ? SYM_VAL : SYM_VAR;
    Sym *sym = checker_sym(self, decl->dforeign.name, type.type, kind);
    if (decl->dforeign.linkname) {
        sym->external_name = resolve_value(self->package, decl->dforeign.linkname).p;
    } else if (decl->dforeign.block && decl->dforeign.block->dforeign_block.linkprefix) {
        const char *prefix = resolve_value(
            self->package, decl->dforeign.block->dforeign_block.linkprefix).p;
        sym->external_name = str_join(prefix, decl->dforeign.name->ename);
    } else {
        sym->external_name = sym->name;
    }
    sym->state = SYM_CHECKED;
    // TODO: Call conv stored on sym?
    return operand_ok;
}

Operand check_decl_foreign_block(Checker *self, Decl *decl) {
    TRACE(CHECKING);
    for (i64 i = 0; i < arrlen(decl->dforeign_block.decls); i++) {
        Operand op = check_decl_foreign(self, decl->dforeign_block.decls[i]);
        if (ret_operand(op)) return op;
    }
    return operand_ok;
}

Operand check_decl_import(Checker *self, Decl *decl) { // Nothing to do?
    TRACE(CHECKING);
    return operand_ok;
}

Operand check_decl_library(Checker *self, Decl *decl) { // Check valid object
    TRACE(CHECKING);
    return operand_ok;
}

Operand check_stmt_label(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    Sym *sym = checker_sym(self, stmt->slabel, NULL, SYM_LABEL);
    sym->state = SYM_CHECKED;
    return operand_ok;
}

Operand check_stmt_assign(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    i64 num_names = arrlen(stmt->sassign.lhs);
    i64 num_values = arrlen(stmt->sassign.rhs);
    if (num_names > 1 && num_values == 1 && stmt->sassign.rhs[0]->kind == EXPR_CALL) {
        Operand call = check_expr_call(self, stmt->sassign.rhs[0]);
        if (ret_operand(call)) return call;
        if (call.type->kind != TYPE_STRUCT || call.type->flags != TUPLE) {
            error(self, stmt->range,
                  "Assignment count mismatch %lld names but call returns %lld value",
                  num_names, 1);
            return bad_operand;
        }
        num_values = arrlen(call.type->taggregate.fields);
        if (arrlen(call.type->taggregate.fields) != num_names) {
            error(self, stmt->range,
                  "Assignment count mismatch %lld names but call returns %lld values",
                  num_names, num_values);
        }
        for (i64 i = 0; i < num_names; i++) {
            Operand lhs = check_expr(self, stmt->sassign.lhs[i]);
            if (ret_operand(lhs)) return lhs;
            expect_operand_lvalue(self, lhs, stmt->sassign.lhs[i]);
            Ty *rhs = call.type->taggregate.fields[i].type;
            expect_type_coerces(self, rhs, lhs.type, stmt->sassign.rhs[0]);
        }
    } else if (num_names > num_values || num_names < num_values) {
        error(self, stmt->range, "Assignment count mismatch %lld = %lld", num_names, num_values);
    }
    for (i64 i = 0; i < num_names; i++) {
        Expr *expr = stmt->sassign.rhs[MIN(i, num_values)];
        Operand lhs = check_expr(self, stmt->sassign.lhs[i]);
        if (ret_operand(lhs)) return lhs;
        Operand rhs = check_expr(self, expr);
        if (ret_operand(rhs)) return rhs;
        expect_operand_lvalue(self, lhs, stmt->sassign.lhs[i]);
        expect_operand_coerces(self, rhs, lhs.type, expr);
    }
    return operand_ok;
}

Operand check_stmt_return(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    if (!arrlen(self->efuncs)) {
        error(self, stmt->range, "Return outside of function body");
        return bad_operand;
    }
    if (arrlen(self->sdefer)) {
        error(self, stmt->range, "Return within defer");
        return bad_operand;
    }
    Expr *func = arrlast(self->efuncs);
    Operand functype = hmget(self->package->operands, func->efunc.type);
    TyField *fields = functype.type->tfunc.result->taggregate.fields;
    for (i64 i = 0; i < arrlen(stmt->sreturn); i++) {
        Ty *expected = fields[i].type;
        Operand op = check_expr(self, stmt->sreturn[i]);
        if (ret_operand(op)) return op;
        expect_operand_coerces(self, op, expected, stmt->sreturn[i]);
    }
    return operand_ok;
}

Operand check_stmt_defer(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    push_scope(self);
    arrput(self->sdefer, stmt);
    Operand op = check_stmt(self, stmt->sdefer);
    if (ret_operand(op)) return op;
    arrpop(self->sdefer);
    pop_scope(self);
    return operand_ok;
}

Operand check_stmt_using(Checker *self, Stmt *stmt) { fatal("Unimplemented"); }

Operand check_stmt_goto(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    if (!arrlen(self->efuncs)) {
        error(self, stmt->range, "%s outside of function body", describe_goto_kind(stmt->flags));
        return bad_operand;
    }
    switch ((GotoKind) stmt->flags) {
        case GOTO_GOTO:
            if (arrlen(self->sdefer))
                error(self, stmt->range, "goto within defer");
            arrput(self->sgotos, stmt); // Check later
            return operand_ok;
        case GOTO_FALLTHROUGH:
            if (!arrlen(self->sswitch))
                error(self, stmt->range, "fallthrough outside of switch");
            return operand_ok;
        case GOTO_CONTINUE:
            if (!arrlen(self->sfor))
                error(self, stmt->range, "continue outside of for");
            break;
        case GOTO_BREAK:
            if (!arrlen(self->sswitch) && !arrlen(self->sfor))
                error(self, stmt->range, "break outside of for/switch");
            break;
        default: fatal("Unrecognized goto kind %d", stmt->flags);
    }
    if (stmt->sgoto) {
        Operand op = check_expr_name(self, stmt->sgoto);
        if (op.flags != LABEL) error(self, stmt->sgoto->range, "Expected label");
    }
    return operand_ok;
}

Operand check_stmt_block(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    push_scope(self);
    for (u32 i = 0; i < arrlen(stmt->sblock); i++) {
        Operand op = check_stmt(self, stmt->sblock[i]);
        if (ret_operand(op)) return op;
    }
    pop_scope(self);
    return operand_ok;
}

Operand check_stmt_if(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    push_scope(self);
    self->wanted_type = type_bool;
    Operand cond = check_expr(self, stmt->sif.cond);
    if (ret_operand(cond)) return cond;
    expect_operand_coerces(self, cond, type_bool, stmt->sif.cond);
    Operand pass = check_stmt(self, stmt->sif.pass);
    if (ret_operand(pass)) return pass;
    if (stmt->sif.fail) {
        Operand fail = check_stmt(self, stmt->sif.fail);
        if (ret_operand(fail)) return fail;
    }
    pop_scope(self);
    return operand_ok;
}

Operand check_stmt_for(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    push_scope(self);
    arrput(self->sfor, stmt);
    switch ((ForKind) stmt->flags) {
        case FOR_REGULAR: {
            if (stmt->sfor.init) {
                Operand init = check_stmt(self, stmt->sfor.init);
                if (ret_operand(init)) return init;
            }
            if (stmt->sfor.cond) {
                self->wanted_type = type_bool;
                Operand cond = check_expr(self, stmt->sfor.cond);
                if (ret_operand(cond)) return cond;
                expect_operand_coerces(self, cond, type_bool, stmt->sfor.cond);
            }
            if (stmt->sfor.step) {
                Operand step = check_stmt(self, stmt->sfor.step);
                if (ret_operand(step)) return step;
            }
            break;
        }
        case FOR_AGGREGATE: {
            Operand aggregate = check_expr(self, stmt->sfor.aggregate);
            if (ret_operand(aggregate)) return aggregate;
            STATIC_ASSERT(offsetof(Ty, tarray.eltype) == offsetof(Ty, tslice.eltype));
            if (!is_array(aggregate.type) && !is_slice(aggregate.type)) {
                error(self, stmt->sfor.aggregate->range, "Cannot iterate over type %s",
                      tyname(aggregate.type));
                return bad_operand;
            }
            Ty *value_type = aggregate.type->tslice.eltype;
            if (stmt->sfor.value_name) {
                Sym *sym = checker_sym(self, stmt->sfor.value_name, value_type, SYM_VAL);
                sym->state = SYM_CHECKED;
            }
            if (stmt->sfor.index_name) {
                Ty *index_type = type_u64;
                if (is_array(aggregate.type))
                    index_type = smallest_unsigned_int_for_value(aggregate.type->tarray.length);
                Sym *sym = checker_sym(self, stmt->sfor.index_name, index_type, SYM_VAL);
                sym->state = SYM_CHECKED;
            }
            break;
        }
        default: fatal("Unhandled for kind %lld", stmt->flags);
    }
    arrpop(self->sfor);
    pop_scope(self);
    return operand_ok;
}

Operand check_stmt_switch(Checker *self, Stmt *stmt) {
    TRACE(CHECKING);
    arrput(self->sswitch, stmt);
    Operand subject = check_expr(self, stmt->sswitch.subject);
    if (ret_operand(subject)) return subject;
    SwitchCase *default_case = NULL;
    for (i64 i = 0; i < arrlen(stmt->sswitch.cases); i++) {
        SwitchCase c = stmt->sswitch.cases[i];
        for (i64 j = 0; j < arrlen(c.matches); j++) {
            Operand match = check_expr(self, c.matches[i]);
            if (ret_operand(match)) return match;
            expect_constant(self, match, c.matches[j]);
            expect_operand_coerces(self, match, subject.type, c.matches[i]);
        }
        if (!c.matches) {
            if (default_case) error(self, c.body->range, "Duplicate default case");
            default_case = stmt->sswitch.cases + i;
        }
        SwitchCase *prev_next_case = self->snext_case;
        if (i + 1 < arrlen(stmt->sswitch.cases)) self->snext_case = stmt->sswitch.cases + i + 1;
        ASSERT(c.body->kind == STMT_BLOCK);
        Operand body = check_stmt_block(self, c.body);
        if (ret_operand(body)) return body;
        self->snext_case = prev_next_case;
    }
    arrpop(self->sswitch);
    return operand_ok;
}

Operand check_stmt(Checker *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_VAL:          return check_decl_val(self, (Decl *) stmt);
        case (StmtKind) DECL_VAR:          return check_decl_var(self, (Decl *) stmt);
        case (StmtKind) DECL_FOREIGN:      return check_decl_foreign(self, (Decl *) stmt);
        case (StmtKind) DECL_FOREIGNBLOCK: return check_decl_foreign_block(self, (Decl *) stmt);
        case (StmtKind) DECL_IMPORT:       return check_decl_import(self, (Decl *) stmt);
        case (StmtKind) DECL_LIBRARY:      return check_decl_library(self, (Decl *) stmt);
        case (StmtKind) DECL_FILE:         return operand_ok;
        case STMT_LABEL:                   return check_stmt_label(self, stmt);
        case STMT_ASSIGN:                  return check_stmt_assign(self, stmt);
        case STMT_RETURN:                  return check_stmt_return(self, stmt);
        case STMT_DEFER:                   return check_stmt_defer(self, stmt);
        case STMT_USING:                   return check_stmt_using(self, stmt);
        case STMT_GOTO:                    return check_stmt_goto(self, stmt);
        case STMT_BLOCK:                   return check_stmt_block(self, stmt);
        case STMT_IF:                      return check_stmt_if(self, stmt);
        case STMT_FOR:                     return check_stmt_for(self, stmt);
        case STMT_SWITCH:                  return check_stmt_switch(self, stmt);
        default:
            if (stmt->kind < STMT_KIND_BASE)
                return check_expr(self, (Expr *) stmt);
            fatal("Unrecognized StmtKind %d", stmt->kind);
    }
}

Operand check_expr(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    switch (expr->kind) {
        case EXPR_NIL:      return check_expr_nil(self, expr);
        case EXPR_INT:      return check_expr_int(self, expr);
        case EXPR_FLOAT:    return check_expr_float(self, expr);
        case EXPR_STR:      return check_expr_str(self, expr);
        case EXPR_NAME:     return check_expr_name(self, expr);
        case EXPR_COMPOUND: return check_expr_compound(self, expr);
        case EXPR_CAST:     return check_expr_cast(self, expr);
        case EXPR_PAREN:    return check_expr_paren(self, expr);
        case EXPR_UNARY:    return check_expr_unary(self, expr);
        case EXPR_BINARY:   return check_expr_binary(self, expr);
        case EXPR_TERNARY:  return check_expr_ternary(self, expr);
        case EXPR_CALL:     return check_expr_call(self, expr);
        case EXPR_FIELD:    return check_expr_field(self, expr);
        case EXPR_INDEX:    return check_expr_index(self, expr);
        case EXPR_FUNC:     return check_expr_func(self, expr);
        case EXPR_FUNCTYPE: return check_expr_func_type(self, expr);
        case EXPR_ARRAY:    return check_expr_array(self, expr);
        case EXPR_POINTER:  return check_expr_pointer(self, expr);
        case EXPR_STRUCT:   return check_expr_struct(self, expr);
        case EXPR_UNION:    return check_expr_union(self, expr);
        case EXPR_ENUM:     return check_expr_enum(self, expr);
        default: fatal("Should not have gotten through parsing");
    }
}

#undef error
#undef note
