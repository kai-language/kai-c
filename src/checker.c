
#include "all.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "compiler.h"
#include "ast.h"
#include "types.h"
#include "checker.h"

typedef struct Checker Checker;
struct Checker {
    Package *package;
    u32 flags;

    Scope *scope;
    Type *wanted_type;

    Expr **efunc; // arr

    Stmt **sswitch; // arr
    Stmt **snext_case; // arr
    Stmt **sfor; // arr
    Type *current_type; // TODO: Use this for detecting cycles for types
};

void check_stmt(Checker *self, Stmt *stmt);
Operand check_expr(Checker *self, Expr *expr);
Operand check_expr_func_type(Checker *self, Expr *expr);
Type *check_decl_var(Checker *self, Decl *decl);
Type *check_decl_val(Checker *self, Decl *decl);
Type *check_decl_foreign(Checker *self, Decl *decl);

#define error(self, range, fmt, ...) add_error(self->package, range, fmt, ##__VA_ARGS__)
#define note(self, range, fmt, ...) add_note(self->package, range, fmt, ##__VA_ARGS__)

bool (*unary_predicates[NUM_OPS])(Type *) = {
    [OP_ADD]  = is_arithmetic,
    [OP_SUB]  = is_arithmetic,
    [OP_BNOT] = is_integer,
    [OP_NOT]  = is_bool,
    [OP_LSS]  = is_ptr,
};

bool (*binary_predicates[NUM_OPS])(Type *) = {
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

Operand bad_operand = { &(Type){ TYPE_INVALID }, .flags = BAD_VALUE };

void check(Package *package, Stmt *stmt) {
    TRACE(CHECKING);
    Checker checker = {
        .package = package,
        .flags = NONE,
        .scope = package->scope,
    };
    check_stmt(&checker, stmt);
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

Sym *checker_new_sym(Checker *self, const char *name, SymKind kind) {
    Sym *sym = arena_calloc(&self->package->arena, sizeof *sym);
    sym->name = name;
    sym->owning_package = self->package;
    return sym;
}

Sym *scope_lookup(Scope *scope, const char *name) {
    do {
        Sym *sym = hmget(scope->members, name);
        if (sym) return sym;
        scope = scope->parent;
    } while (scope);
    return NULL;
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
Operand operand(Checker *self, Expr *expr, Type *type, OperandFlags flags) {
    Operand op = { type, flags, .val.i = 0 };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandi(Checker *self, Expr *expr, Type *type, OperandFlags flags, i64 val) {
    Operand op = { type, flags, .val.i = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandu(Checker *self, Expr *expr, Type *type, OperandFlags flags, i64 val) {
    Operand op = { type, flags, .val.u = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandf(Checker *self, Expr *expr, Type *type, OperandFlags flags, f64 val) {
    Operand op = { type, flags, .val.f = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandp(Checker *self, Expr *expr, Type *type, OperandFlags flags, void *val) {
    Operand op = { type, flags, .val.p = val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE
Operand operandv(Checker *self, Expr *expr, Type *type, OperandFlags flags, Val val) {
    Operand op = { type, flags, val };
    OperandMapEntry entry = {expr, op};
    hmputs(self->package->operands, entry);
    return op;
}

INLINE void expect_operand_is_a_type(Checker *self, Operand operand, Expr *expr) {
    TRACE(CHECKING);
    if ((operand.flags&TYPE) == 0) {
        error(self, expr->range, "'%s' cannot be used as a type", describe_ast(expr));
    }
}

INLINE bool operand_coerces(Checker *self, Operand operand, Type *wanted) {
    TRACE(CHECKING);
}

INLINE void expect_operand_casts(Checker *self, Operand op, Type *type, Expr *expr) {
    TRACE(CHECKING);
}

INLINE void expect_operand_coerces(Checker *self, Operand op, Type *type, Expr *expr) {
    TRACE(CHECKING);
}

INLINE void expect_constant(Checker *self, Operand op, Expr *expr) {
    TRACE(CHECKING);
}

Type *check_sym_decl(Checker *self, Sym *sym) {
    TRACE(CHECKING);
    Checker checker = {sym->owning_package, .scope = sym->package->scope};
    switch (sym->decl->kind) {
        case DECL_VAR:
            return check_decl_var(&checker, sym->decl);
        case DECL_VAL:
            return check_decl_val(&checker, sym->decl);
        case DECL_FOREIGN:
            return check_decl_foreign(&checker, sym->decl);
        case DECL_IMPORT:
        case DECL_LIBRARY:
        case DECL_FOREIGNBLOCK:
            return NULL;
        default:
            fatal("Unhandled case");
    }
}

void sym_resolve(Checker *self, Sym *sym) {
    TRACE(CHECKING);
    if (sym->state == SYM_RESOLVED) return;
    else if (sym->state == SYM_RESOLVING) {
        error(self, sym->decl->range, "Cyclic dependency");
        return;
    }
    ASSERT(sym->state == SYM_UNRESOLVED);
    ASSERT(!sym->reachable);
    sym->reachable = REACHABLE_NATURAL;
    sym->state = SYM_RESOLVING;
    sym->type = check_sym_decl(self, sym);
    switch (sym->kind) {
        case SYM_VAR: {
            Checker checker = {sym->owning_package};
            sym->type = check_decl_var(&checker, sym->decl);
        }
        case SYM_VAL: {
            Checker checker = {sym->owning_package};
            sym->type = check_decl_val(&checker, sym->decl);
            break;
        }
        case SYM_PACKAGE:
            break;
    }
    sym->state = SYM_RESOLVED;
    arrput(compiler.ordered_symbols, sym);
}

Operand check_expr_nil(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    if (self->wanted_type && !is_ptr(self->wanted_type)) {
        add_error(self->package, expr->range, "'nil' is not convertable to '%s'", typename(self->wanted_type));
        return bad_operand;
    }
    Type *type = self->wanted_type ?: type_rawptr;
    return operandi(self, expr, type, CONST, 0);
}

Operand check_expr_int(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Val val = {expr->eint};
    Type *type = smallest_unsigned_int_for_value(val.u);
    if (self->wanted_type) {
        if (is_integer(self->wanted_type) || is_ptr(self->wanted_type)) {
            if (val.u > type_max_value(self->wanted_type)) {
                error(self, expr->range,
                      "Cannot coerce value '%d' to type %s as loss of information would occur",
                      val.u, typename(self->wanted_type));
                note(self, expr->range, "Cast to force conversion with overflow");
            }
        }
    } else if (is_float(self->wanted_type)) {
        val.f = (f64)val.u;
    }
    return operandv(self, expr, type, CONST, val);
}

Operand check_expr_float(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    return operandf(self, expr, type_f64, CONST, expr->efloat);
}

Operand check_expr_str(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Type *type = type_string;
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
    OperandFlags flags = 0;
    sym_resolve(self, sym);
    switch (sym->kind) {
        case SYM_VAL: flags |= CONST; break;
        case SYM_VAR: flags |= LVALUE; break;
        case SYM_PACKAGE: flags |= PACKAGE | SPECIAL; goto special;
        case SYM_LIBRARY: flags |= LIBRARY | SPECIAL; goto special;
        default: fatal("Unhandled");
    }
    return operandv(self, expr, sym->type, flags, sym->val);
special:
    return operandp(self, expr, sym->type, flags, sym);
}

Operand check_expr_compound(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Type *type = self->wanted_type;
    if (expr->ecompound.type) {
        Operand op = check_expr(self, expr->ecompound.type);
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
                              field.key->ename, typename(type));
                        goto check_agg_value;
                    }
                }
                if (index >= arrlen(type->taggregate.fields)) {
                    error(self, field.val->range,
                          "No field at index '%ld' in type %s", index, typename(type));
                    goto check_agg_value;
                }
            check_agg_value:;
                self->wanted_type = type->taggregate.fields[index].type;
                Operand op = check_expr(self, field.val);
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
                          index, type->tarray.length, typename(type));
                }
            check_arr_value:;
                self->wanted_type = type->tarray.eltype;
                Operand op = check_expr(self, field.val);
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
    return operandv(self, expr, op.type, op.flags, op.val);
}

Operand check_expr_unary(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand value = check_expr(self, expr->eunary);
    if (value.flags == BAD_VALUE) return bad_operand;
    Op op = (Op) expr->flags;
    Type *type = NULL;
    switch (op) {
        case OP_AND: {
            if ((value.flags & LVALUE) == 0) {
                error(self, expr->range, "Cannot take address of '%s'", describe_ast(expr));
                return bad_operand;
            }
            Type *type = type_ptr(value.type, NONE);
            return operand(self, expr, type, NONE);
        }
        default: {
            if (!unary_predicates[op](value.type)) {
                error(self, expr->range, "Operation '%s' undefined for type %s",
                      describe_op(expr->flags), typename(value.type));
                return bad_operand;
            }
            if (expr->flags == OP_NOT) {
                type = type_bool;
            } else if (expr->flags == OP_LSS) {
                type = type->tptr.base;
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
    Operand rhs = check_expr(self, expr->ebinary.erhs);
    if (lhs.flags == BAD_VALUE || rhs.flags == BAD_VALUE) return bad_operand;
    if (!(operand_coerces(self, lhs, rhs.type) || operand_coerces(self, rhs, lhs.type))) {
        error(self, expr->range, "No coercion can occur to make %s and %s identical types",
              typename(lhs.type), typename(rhs.type));
        return bad_operand;
    }
    Type *type = lhs.type; // both sides of the expression have this type.
    if (!binary_predicates[expr->flags](type)) {
        error(self, expr->range, "Operation '%s' undefined for type %s",
              describe_op(expr->flags), typename(type));
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
    Type *wanted_type = self->wanted_type;
    self->wanted_type = type_bool;
    Operand cond = check_expr(self, expr->eternary.econd);
    Operand pass = cond;
    if (expr->eternary.epass) {
        self->wanted_type = wanted_type;
        pass = check_expr(self, expr->eternary.epass);
    }
    self->wanted_type = wanted_type;
    Operand fail = check_expr(self, expr->eternary.efail);
    if (cond.flags&BAD_VALUE || pass.flags&BAD_VALUE || fail.flags&BAD_VALUE) return bad_operand;
    if (!is_bool(cond.type) && !is_arithmetic(cond.type) && !is_ptr(cond.type)) {
        error(self, expr->range, "Expected numeric or pointer type as condition");
        return bad_operand;
    }
    if (!operand_coerces(self, fail, pass.type)) {
        error(self, expr->eternary.efail->range, "Expected type %s got type %s",
              typename(pass.type), typename(fail.type));
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
    if (callee.flags == BAD_VALUE) return bad_operand;
    if (callee.flags&TYPE) { // Call is a cast
        if (arrlen(expr->ecall.args) < 1 || arrlen(expr->ecall.args) > 1) {
            error(self, expr->range, "Expected 1 argument in cast to '%s'", typename(callee.type));
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
        expect_operand_casts(self, arg, callee.type, expr->ecast.expr);
        return operand(self, expr, callee.type, NONE);
    }
    if (callee.type->kind != TYPE_FUNC) {
        error(self, expr->ecall.expr->range,
              "Cannot call non function type '%s'", typename(callee.type));
        return bad_operand;
    }
    u32 num_params = (u32) arrlen(callee.type->tfunc.params);
    for (u32 i = 0; i < arrlen(expr->ecall.args); i++) {
        CallArg call_arg = expr->ecall.args[i];
        if (i >= num_params && (callee.type->flags & FUNC_VARGS) == 0) {
            error(self, call_arg.expr->range, "Too many arguments in call to '%s', expected %ld",
                  typename(callee.type), num_params);
            break;
        }
        self->wanted_type = callee.type->tfunc.params[MIN(num_params - 1, i)];
        if (self->wanted_type->kind == TYPE_SLICE && self->wanted_type->flags & FUNC_VARGS) {
            self->wanted_type = self->wanted_type->tslice.eltype;
        }
        Operand arg = check_expr(self, call_arg.expr);
        if (arg.flags == BAD_VALUE) return bad_operand;
        expect_operand_coerces(self, arg, self->wanted_type, call_arg.expr);
    }
    return operand(self, expr, callee.type, NONE);
}

Operand check_expr_field(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand base = check_expr(self, expr->efield.expr);
    if (base.flags == BAD_VALUE) return bad_operand;
    const char *name = expr->efield.name->ename;
    if (base.flags == PACKAGE) {
        Sym *file = base.val.p;
        Package *import = file->package;
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
            case SYM_UNRESOLVED:
                return bad_operand;
            case SYM_RESOLVING:
                error(self, expr->range, "Declaration initial value refers to itself");
                return bad_operand;
            case SYM_RESOLVED: break;
        }
        OperandFlags flags = NONE;
        switch (sym->kind) {
            case SYM_VAL: flags |= CONST; break;
            case SYM_VAR: flags |= LVALUE; break;
            case SYM_PACKAGE: fatal("Expected to be unable to reference imports from imports");
            case SYM_LIBRARY:
            default: fatal("Unhandled");
        }
        return operandv(self, expr, sym->type, flags, sym->val);
    }
    if (base.flags == LIBRARY) {
        error(self, expr->efield.expr->range, "Libraries types have no members");
        return bad_operand;
    }
    if (is_aggregate(base.type)) {
        u32 index = 0;
        TypeField *field = NULL;
        for (u32 i = 0; i < arrlen(base.type->taggregate.fields); i++) {
            if (base.type->taggregate.fields[i].name == name) {
                index = i;
                field = base.type->taggregate.fields + i;
                break;
            }
        }
        error(self, expr->range, "Aggregate type %s has no field '%s'", typename(base.type), name);
        return bad_operand;
    found_aggregate_field:;
        return operand(self, expr, field->type, base.flags);
    }
    error(self, expr->range, "%s has no field '%s'", typename(base.type), name);
    return bad_operand;
}

Operand check_expr_index(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand base = check_expr(self, expr->eindex.expr);
    if (base.flags&BAD_VALUE) return bad_operand;
    Operand index = check_expr(self, expr->eindex.index);
    if (base.flags&BAD_VALUE) return bad_operand;
    if (!is_integer(index.type)) {
        error(self, expr->eindex.index->range, "Cannot index with non integer type '%s'",
              typename(index.type));
        return bad_operand;
    }
    Type *type;
    switch (base.type->kind) {
        case TYPE_ARRAY: {
            type = base.type->tarray.eltype;
            if (index.flags&CONST && (index.val.i < 0 || index.val.u >= base.type->tarray.length)) {
                error(self, expr->range, "Index %d is out of the bounds for type %s",
                      typename(base.type));
                return bad_operand;
            }
            break;
        }
        case TYPE_SLICE: type = base.type->tslice.eltype; break;
        case TYPE_PTR: type = base.type->tptr.base; break;
        default:
            error(self, expr->eindex.expr->range, "Unable to index type '%s'", typename(base.type));
            return bad_operand;
    }
    return operand(self, expr, type, NONE);
}

Operand check_expr_func(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    push_scope(self); // allow shadowing by declaring a separate params scope
    Operand type = check_expr_func_type(self, expr->efunc.type);
    for (u32 i = 0; i < arrlen(type.type->tfunc.params); i++) {
        Sym *sym = checker_new_sym(self, type.type->tfunc.labels[i], SYM_VAR);
        sym->type = type.type->tfunc.params[i];
        scope_declare(self->scope, sym);
    }
    push_scope(self);
    arrpush(self->efunc, expr);
    check_stmt(self, expr->efunc.body);
    arrpop(self->efunc);
    pop_scope(self);
    pop_scope(self);
    return operand(self, expr, type.type, NONE);
}

Operand check_expr_func_type(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Type **params = NULL;
    const char **labels = NULL;
    TypeField *result_fields = NULL;
    arrsetcap(params, arrlen(expr->efunc.type->efunctype.params));
    arrsetcap(labels, arrlen(expr->efunc.type->efunctype.params));
    arrsetcap(result_fields, arrlen(expr->efunc.type->efunctype.result));
    for (u32 i = 0; i < arrlen(expr->efunctype.params); i++) {
        Operand param = check_expr(self, expr->efunctype.params[i].type);
        expect_operand_is_a_type(self, param, expr->efunctype.params[i].type);
        arrput(labels, expr->efunctype.params[i].name->ename);
        arrput(params, param.type);
    }
    for (u32 i = 0; i < arrlen(expr->efunctype.result); i++) {
        Operand result = check_expr(self, expr->efunctype.result[i].type);
        expect_operand_is_a_type(self, result, expr->efunctype.result[i].type);
        TypeField field = { NULL, result.type };
        arrput(result_fields, field);
    }
    Type *result = NULL;
    if (result_fields && result_fields[0].type == type_void) {
        arrfree(result_fields);
        result = type_void;
    } else {
        result = type_struct(result_fields, TUPLE);
    }
    Type *type = type_func(labels, params, result, (FuncFlags) expr->flags);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_array(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    Operand element = check_expr(self, expr->earray.base);
    expect_operand_is_a_type(self, element, expr->earray.base);
    if (!expr->earray.len) fatal("Unimplemented"); // TODO
    Operand length = check_expr(self, expr->earray.len);
    expect_constant(self, length, expr->earray.len);
    Type *type = type_array(element.type, length.val.u, TYPE);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_pointer(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    // FIXME: ??? ALLOW UNRESOLVED TYPES
    Operand base = check_expr(self, expr->epointer.base);
    expect_operand_is_a_type(self, base, expr->epointer.base);
    Type *type = type_ptr(base.type, NONE);
    return operand(self, expr, type, NONE);
}

Operand check_expr_struct(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    TypeField *fields = NULL;
    arrsetcap(fields, arrlen(expr->estruct.fields));
    u64 align = 0;
    u64 width = 0;
    for (i64 i = 0; i < arrlen(expr->estruct.fields); i++) {
        Operand type = check_expr(self, expr->estruct.fields->type);
        align = MAX(align, type.type->align);
        for (i64 j = 0; j < arrlen(expr->estruct.fields->names); j++) {
            u64 offset = ALIGN_UP(width, type.type->align);
            TypeField field = {expr->estruct.fields->names[i]->ename, type.type, offset};
            width = offset + type.type->size;
            arrput(fields, field);
        }
    }
    Type *type = type_struct(fields, NONE);
    return operand(self, expr, type, TYPE);
}

Operand check_expr_union(Checker *self, Expr *expr) {
    TRACE(CHECKING);
    TypeField *fields = NULL;
    arrsetcap(fields, arrlen(expr->estruct.fields));
    u64 align = 0;
    for (i64 i = 0; i < arrlen(expr->estruct.fields); i++) {
        Operand type = check_expr(self, expr->estruct.fields->type);
        align = MAX(align, type.type->align);
        for (i64 j = 0; j < arrlen(expr->estruct.fields->names); j++) {
            TypeField field = {expr->estruct.fields->names[i]->ename, type.type};
            arrput(fields, field);
        }
    }
    Type *type = type_union(fields, NONE);
    return operand(self, expr, type, NONE);
}

Operand check_expr_enum(Checker *self, Expr *expr) { return bad_operand; }

Type *check_decl_val(Checker *self, Decl *decl) {
    TRACE(CHECKING);
    Sym *sym = NULL;
    if (self->scope == self->package->scope) { // Symbol is predeclared

    }
    return NULL;
}

Type *check_decl_var(Checker *self, Decl *decl) { return NULL; }
Type *check_decl_foreign(Checker *self, Decl *decl) { return NULL; }
void check_decl_foreign_block(Checker *self, Decl *decl) { }
void check_decl_import(Checker *self, Decl *decl) { }

void check_stmt_label(Checker *self, Stmt *stmt) { }
void check_stmt_assign(Checker *self, Stmt *stmt) { }
void check_stmt_return(Checker *self, Stmt *stmt) { }
void check_stmt_defer(Checker *self, Stmt *stmt) { }
void check_stmt_using(Checker *self, Stmt *stmt) { }
void check_stmt_goto(Checker *self, Stmt *stmt) { }

void check_stmt_block(Checker *self, Stmt *stmt) {
    push_scope(self);
    for (u32 i = 0; i < arrlen(stmt->sblock); i++) {
        check_stmt(self, stmt->sblock[i]);
    }
    pop_scope(self);
}

void check_stmt_if(Checker *self, Stmt *stmt) { }
void check_stmt_for(Checker *self, Stmt *stmt) { }
void check_stmt_switch(Checker *self, Stmt *stmt) { }

void check_stmt(Checker *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_VAL:
            check_decl_val(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_VAR:
            check_decl_var(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_FOREIGN:
            check_decl_foreign(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_FOREIGNBLOCK:
            check_decl_foreign_block(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_IMPORT:
            check_decl_import(self, (Decl *) stmt);
            break;
        case STMT_LABEL:
            check_stmt_label(self, stmt);
            break;
        case STMT_ASSIGN:
            check_stmt_assign(self, stmt);
            break;
        case STMT_RETURN:
            check_stmt_return(self, stmt);
            break;
        case STMT_DEFER:
            check_stmt_defer(self, stmt);
            break;
        case STMT_USING:
            check_stmt_using(self, stmt);
            break;
        case STMT_GOTO:
            check_stmt_goto(self, stmt);
            break;
        case STMT_BLOCK:
            check_stmt_block(self, stmt);
            break;
        case STMT_IF:
            check_stmt_if(self, stmt);
            break;
        case STMT_FOR:
            check_stmt_for(self, stmt);
            break;
        case STMT_SWITCH:
            check_stmt_switch(self, stmt);
            break;
        default:
            fatal("Should not have gotten through parsing");
    }
}

Operand check_expr(Checker *self, Expr *expr) {
    switch (expr->kind) {
        case EXPR_NIL:
            return check_expr_nil(self, expr);
        case EXPR_INT:
            return check_expr_int(self, expr);
        case EXPR_FLOAT:
            return check_expr_float(self, expr);
        case EXPR_STR:
            return check_expr_str(self, expr);
        case EXPR_NAME:
            return check_expr_name(self, expr);
        case EXPR_COMPOUND:
            return check_expr_compound(self, expr);
        case EXPR_CAST:
            return check_expr_cast(self, expr);
        case EXPR_PAREN:
            return check_expr_paren(self, expr);
        case EXPR_UNARY:
            return check_expr_unary(self, expr);
        case EXPR_BINARY:
            return check_expr_binary(self, expr);
        case EXPR_TERNARY:
            return check_expr_ternary(self, expr);
        case EXPR_CALL:
            return check_expr_call(self, expr);
        case EXPR_FIELD:
            return check_expr_field(self, expr);
        case EXPR_INDEX:
            return check_expr_index(self, expr);
        case EXPR_FUNC:
            return check_expr_func(self, expr);
        case EXPR_FUNCTYPE:
            return check_expr_func_type(self, expr);
        case EXPR_ARRAY:
            return check_expr_array(self, expr);
        case EXPR_POINTER:
            return check_expr_pointer(self, expr);
        case EXPR_STRUCT:
            return check_expr_struct(self, expr);
        case EXPR_UNION:
            return check_expr_union(self, expr);
        case EXPR_ENUM:
            return check_expr_enum(self, expr);
        default:
            fatal("Should not have gotten through parsing");
    }
}

#undef error
#undef note
