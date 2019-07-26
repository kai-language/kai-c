
#include "all.h"
#include "ast.h"
#include "types.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "checker.h"
#include "string.h"
#include "compiler.h"

typedef struct InternedType InternedType;
struct InternedType {
    void *key;
    Ty *value;
};

Ty *type_invalid = &(Ty){ TYPE_INVALID };

Ty *type_any  = &(Ty){ TYPE_ANY, NONE };
Ty *type_cvarg = &(Ty){ TYPE_ANY, CVARG };
Ty *type_void = &(Ty){ TYPE_VOID, NONE };
Ty *type_bool = &(Ty){ TYPE_BOOL, NONE, 1, 1 };

Ty *type_i8  = &(Ty){ TYPE_INT, SIGNED, 1, 1 };
Ty *type_i16 = &(Ty){ TYPE_INT, SIGNED, 2, 2 };
Ty *type_i32 = &(Ty){ TYPE_INT, SIGNED, 4, 4 };
Ty *type_i64 = &(Ty){ TYPE_INT, SIGNED, 8, 8 };

Ty *type_u8  = &(Ty){ TYPE_INT, NONE, 1, 1 };
Ty *type_u16 = &(Ty){ TYPE_INT, NONE, 2, 2 };
Ty *type_u32 = &(Ty){ TYPE_INT, NONE, 4, 4 };
Ty *type_u64 = &(Ty){ TYPE_INT, NONE, 8, 8 };

Ty *type_f32 = &(Ty){ TYPE_FLOAT, NONE, 4, 4 };
Ty *type_f64 = &(Ty){ TYPE_FLOAT, NONE, 8, 8 };

Ty *type_int  = &(Ty){ TYPE_INT, NONE, 8, 8 };
Ty *type_uint = &(Ty){ TYPE_INT, NONE, 8, 8 };
Ty *type_intptr  = &(Ty){ TYPE_INT, NONE, 8, 8 };
Ty *type_uintptr = &(Ty){ TYPE_INT, NONE, 8, 8 }; // FIXME: Non constant sizes

Ty *type_u8ptr;
Ty *type_rawptr;
Ty *type_string;

void init_types() {
    type_u8ptr = type_ptr(type_u8, NONE);
    type_rawptr = type_u8ptr;
    type_string = type_slice(type_u8, STRING);

#define DECLARE_BUILTIN_TYPE(TYPE, NAME) \
{ \
Sym *sym = arena_calloc(&compiler.arena, sizeof *sym); \
sym->name = str_intern(NAME); \
sym->state = SYM_CHECKED; \
sym->kind = SYM_TYPE; \
sym->type = TYPE; \
TYPE->sym = sym; \
scope_declare(compiler.global_scope, sym); \
}
    DECLARE_BUILTIN_TYPE(type_any, "any");
    DECLARE_BUILTIN_TYPE(type_void, "void");
    DECLARE_BUILTIN_TYPE(type_bool, "bool");
    DECLARE_BUILTIN_TYPE(type_i8, "i8");
    DECLARE_BUILTIN_TYPE(type_i16, "i16");
    DECLARE_BUILTIN_TYPE(type_i32, "i32");
    DECLARE_BUILTIN_TYPE(type_i64, "i64");
    DECLARE_BUILTIN_TYPE(type_u8, "u8");
    DECLARE_BUILTIN_TYPE(type_u16, "u16");
    DECLARE_BUILTIN_TYPE(type_u32, "u32");
    DECLARE_BUILTIN_TYPE(type_u64, "u64");
    DECLARE_BUILTIN_TYPE(type_f32, "f32");
    DECLARE_BUILTIN_TYPE(type_f64, "f64");
    DECLARE_BUILTIN_TYPE(type_int, "int");
    DECLARE_BUILTIN_TYPE(type_uint, "uint");
    DECLARE_BUILTIN_TYPE(type_intptr, "intptr");
    DECLARE_BUILTIN_TYPE(type_uintptr, "uintptr");
    DECLARE_BUILTIN_TYPE(type_rawptr, "rawptr");
    DECLARE_BUILTIN_TYPE(type_string, "string");
}

bool is_ptr(Ty *type) {
    return type && type->kind == TYPE_PTR;
}

bool is_func(Ty *type) {
    return type && type->kind == TYPE_FUNC;
}

bool is_ptr_like_type(Ty *type) {
    return type && (type->kind == TYPE_PTR || type->kind == TYPE_FUNC);
}

bool is_array(Ty *type) {
    return type && type->kind == TYPE_ARRAY;
}

bool is_slice(Ty *type) {
    return type && type->kind == TYPE_SLICE;
}

bool is_bool(Ty *type) {
    return type && type->kind == TYPE_PTR;
}

bool is_integer(Ty *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_ENUM);
}

bool is_float(Ty *type) {
    return type && type->kind == TYPE_FLOAT;
}

bool is_arithmetic(Ty *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_FLOAT);
}

bool is_arithmetic_or_ptr(Ty *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_PTR);
}

bool is_scalar(Ty *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_FUNC);
}

bool is_aggregate(Ty *type) {
    return type->kind == TYPE_STRUCT || type->kind == TYPE_UNION;
}

bool is_signed(Ty *type) {
    return is_integer(type) && (type->flags & SIGNED) != 0;
}

bool is_bitwisable(Ty *type) {
    return is_integer(type) || (type && type->kind == TYPE_ENUM && (type->flags & ENUM_FLAGS) != 0);
}

bool is_equatable(Ty *type) {
    static b32 equatables[NUM_TYPE_KINDS] = {
        [TYPE_INT] = true,
        [TYPE_PTR] = true,
        [TYPE_ENUM] = true,
        [TYPE_FLOAT] = true,
    };
    return equatables[type->kind];
}

bool is_comparable(Ty *type) {
    if (!type) return false;
    switch (type->kind) {
        case TYPE_INT:
        case TYPE_PTR:
        case TYPE_ENUM:
        case TYPE_FLOAT:
            return true;
        default:
            return false;
    }
}

bool is_logical(Ty *type) {
    if (!type) return false;
    switch (type->kind) {
        case TYPE_INT:
        case TYPE_PTR:
        case TYPE_BOOL:
            return true;
        default:
            return false;
    }
}

Ty *smallest_signed_int_for_value(i64 val) {
    val = MIN(val, INT8_MIN);
    if (val == INT8_MIN) return type_i8;
    val = MIN(val, INT16_MIN);
    if (val == INT16_MIN) return type_i16;
    val = MIN(val, INT32_MIN);
    if (val == INT32_MIN) return type_i32;
    return type_i64;
}

Ty *smallest_unsigned_int_for_value(u64 val) {
    val = MAX(val, UINT8_MAX);
    if (val == UINT8_MAX) return type_u8;
    val = MAX(val, UINT16_MAX);
    if (val == UINT16_MAX) return type_u16;
    val = MAX(val, UINT32_MAX);
    if (val == UINT32_MAX) return type_u32;
    return type_u64;
}

u32 aggregate_field_index(Ty *type, const char *name) {
    ASSERT(is_aggregate(type));
    for (u32 i = 0; i < arrlen(type->taggregate.fields); i++) {
        if (type->taggregate.fields[i].name == name) return i;
    }
    return -1;
}

u64 type_max_value(Ty *type) {
    ASSERT(is_integer(type) || is_ptr(type));
    if (type->flags & SIGNED)
        return powi(2, type->size * 8) / 2 - 1;
    return powi(2, type->size * 8) - 1;
}

u64 powi(u64 base, u64 exp) {
    u64 result = 1;
    for (;;) {
        if (exp & 1) result *= base;
        exp >>= 1;
        if (!exp) break;
        base *= base;
    }
    return result;
}

Ty *min_int_for_neg_value(i64 val) {
    val = MIN(val, INT8_MIN);
    if (val == INT8_MIN) return type_i8;
    val = MIN(val, INT16_MIN);
    if (val == INT16_MIN) return type_i16;
    val = MIN(val, INT32_MIN);
    if (val == INT32_MIN) return type_i32;
    return type_i64;
}

Ty *min_int_for_pos_value(u64 val) {
    val = MAX(val, UINT8_MAX);
    if (val == UINT8_MAX) return type_u8;
    val = MAX(val, UINT16_MAX);
    if (val == UINT16_MAX) return type_u16;
    val = MAX(val, UINT32_MAX);
    if (val == UINT32_MAX) return type_u32;
    return type_u64;
}

Ty *type_base(Ty *a) {
    while (a->base) a = a->base;
    return a;
}

bool types_eql(Ty *a, Ty *b) {
    return type_base(a) == type_base(b);
}

#define type_size(type, member) offsetof(type, member) + sizeof(((type *)0)->member)

Ty *type_alloc(TyKind kind, u8 flags, size_t size) {
    Ty *type = arena_alloc(&compiler.arena, size);
    Ty template = {kind, flags};
    memcpy(type, &template, size);
    return type;
}

InternedType *interned_func_tys;

Ty *type_func(Ty **params, Ty *result, FuncFlags flags) {
    TRACE(CHECKING);
    void *params_hash = (void *) stbds_hash_bytes(params, arrlen(params) * sizeof *params, 0x31415926);
    void *bytes[] = {params_hash, result, (void *) flags};
    void *hash = (void *) stbds_hash_bytes(&bytes, sizeof bytes, 0x31415926);
    Ty *type = hmget(interned_func_tys, hash);
    if (!type) {
        type = type_alloc(TYPE_FUNC, (u8) flags, type_size(Ty, tfunc));
        type->size = compiler.target_metrics.width;
        type->align = compiler.target_metrics.align;
        type->tfunc.params = params;
        type->tfunc.result = result;
        hmput(interned_func_tys, hash, type);
    }
    return type;
}

InternedType *interned_struct_tuple_tys;

Ty *type_struct(TyField *fields, u32 size, u32 align, u8 flags) {
    TRACE(CHECKING);
    void *hash;
    Ty *type;
    if (flags&TUPLE) {
        hash = (void *) stbds_hash_bytes(fields, arrlen(fields) * sizeof *fields, 0x31415926);
        type = hmget(interned_struct_tuple_tys, hash);
        if (type) return type;
    }
    type = type_alloc(TYPE_STRUCT, flags, type_size(Ty, taggregate));
    type->size = size;
    type->align = align;
    type->taggregate.fields = fields;
    if (flags&TUPLE) hmput(interned_struct_tuple_tys, hash, type);
    return type;
}

Ty *type_union(TyField *fields, u32 size, u32 align, u8 flags) {
    TRACE(CHECKING);
    Ty *type = type_alloc(TYPE_UNION, flags, type_size(Ty, taggregate));
    type->taggregate.fields = fields;
    return type;
}

Ty *type_enum(u8 flags) {
    TRACE(CHECKING);
    Ty *type = type_alloc(TYPE_ENUM, flags, type_size(Ty, tenum));
    return type;
}

InternedType *interned_ptr_tys;

Ty *type_ptr(Ty *base, u8 flags) {
    TRACE(CHECKING);
    Ty *type = hmget(interned_ptr_tys, base);
    if (!type) {
        type = type_alloc(TYPE_PTR, flags, type_size(Ty, tptr));
        type->size = compiler.target_metrics.width;
        type->align = compiler.target_metrics.align;
        type->tptr.base = base;
        hmput(interned_ptr_tys, base, type);
    }
    return type;
}

InternedType *interned_array_tys;

Ty *type_array(Ty *eltype, u64 length, u8 flags) {
    TRACE(CHECKING);
    void *bytes[] = {eltype, (void *) length};
    void *hash = (void *) stbds_hash_bytes(bytes, sizeof *bytes, 0x31415926);
    Ty *type = hmget(interned_array_tys, hash);
    if (!type) {
        type = type_alloc(TYPE_ARRAY, flags, type_size(Ty, tarray));
        u64 size = eltype->size * length;
        ASSERT(size <= UINT32_MAX);
        type->size = (u32) size;
        type->align = eltype->align;
        type->tarray.eltype = eltype;
        type->tarray.length = length;
        hmput(interned_array_tys, hash, type);
    }
    return type;
}

InternedType *interned_slice_tys;

Ty *type_slice(Ty *eltype, u8 flags) {
    TRACE(CHECKING);
    Ty *type = hmget(interned_slice_tys, eltype);
    if (!type) {
        type = type_alloc(TYPE_SLICE, flags, type_size(Ty, tslice));
        type->size = compiler.target_metrics.width * 3; // ptr, len, cap
        type->align = compiler.target_metrics.align;
        type->tslice.eltype = eltype;
        hmput(interned_slice_tys, eltype, type);
    }
    return type;
}

int type_kind_alloc_sizes[] = {
    [TYPE_INVALID] = 0,
    [TYPE_COMPLETING] = 0,
    [TYPE_VOID] = sizeof(Ty),
    [TYPE_BOOL] = sizeof(Ty),
    [TYPE_INT] = sizeof(Ty),
    [TYPE_ENUM] = type_size(Ty, tenum),
    [TYPE_FLOAT] = sizeof(Ty),
    [TYPE_PTR] = type_size(Ty, tptr),
    [TYPE_FUNC] = type_size(Ty, tfunc),
    [TYPE_ARRAY] = type_size(Ty, tarray),
    [TYPE_SLICE] = type_size(Ty, tslice),
    [TYPE_STRUCT] = type_size(Ty, taggregate),
    [TYPE_UNION] = type_size(Ty, taggregate),
    [TYPE_ANY] = sizeof(Ty),
};

Ty *type_alias(Ty *base, Sym *sym) {
    TRACE(CHECKING);
    if (base->kind == TYPE_INVALID) return base;
    int size = type_kind_alloc_sizes[base->kind];
    Ty *type = arena_alloc(&compiler.arena, size);
    memcpy(type, base, size);
    type->base = base;
    type->sym = sym;
    return type;
}

const char *tyname(Ty *type) {
    if (type->sym) return type->sym->name;
    switch (type->kind) {
        case TYPE_INVALID:    return "invalid";
        case TYPE_COMPLETING: return "completing";
        case TYPE_VOID:       return "void";
        case TYPE_BOOL:       return "bool";
        case TYPE_INT:        return "int";
        case TYPE_ENUM:       return "enum";
        case TYPE_FLOAT:      return "float";
        case TYPE_PTR: return str_join("*", tyname(type->tptr.base));
        case TYPE_FUNC: {
            char buf[1024];
            int len = 0;
            len += snprintf(buf + len, sizeof buf - len, "fn(");
            for (int i = 0; i < arrlen(type->tfunc.params); i++) {
                len += snprintf(buf + len, sizeof buf - len, "%s", tyname(type->tfunc.params[i]));
                if (i != arrlen(type->tfunc.params) - 1)
                    len += snprintf(buf + len, sizeof buf - len, ", ");
            }
            len += snprintf(buf + len, sizeof buf - len, ")");
            if (type->tfunc.result) {
                len += snprintf(buf + len, sizeof buf - len, " -> %s", tyname(type->tfunc.result));
            }
            buf[len] = '\0';
            return str_intern(buf);
        }
        case TYPE_ARRAY:      return "array";
        case TYPE_SLICE: return str_join("[]", tyname(type->tslice.eltype));
        case TYPE_STRUCT: {
            if (type->flags&OPAQUE) return "struct #opaque";
            char buf[1024];
            int len = 0;
            if (type->flags&TUPLE) {
                if (arrlen(type->taggregate.fields) == 1)
                    return tyname(type->taggregate.fields->type);
                len += snprintf(buf + len, sizeof buf - len, "(");
                for (int i = 0; i < arrlen(type->taggregate.fields); i++) {
                    TyField field = type->taggregate.fields[i];
                    len += snprintf(buf + len, sizeof buf - len, "%s", tyname(field.type));
                    if (i != arrlen(type->taggregate.fields) - 1)
                        len += snprintf(buf + len, sizeof buf - len, ", ");
                }
                len += snprintf(buf + len, sizeof buf - len, ")");
            }
            len += snprintf(buf + len, sizeof buf - len, "struct {");
            for (int i = 0; i < arrlen(type->taggregate.fields); i++) {
                TyField field = type->taggregate.fields[i];
                if (field.name)
                    len += snprintf(buf + len, sizeof buf - len, "%s: ", field.name);
                len += snprintf(buf + len, sizeof buf - len, "%s", tyname(field.type));
                if (i != arrlen(type->taggregate.fields) - 1)
                    len += snprintf(buf + len, sizeof buf - len, "; ");
            }
            len += snprintf(buf + len, sizeof buf - len, "}");
            buf[len] = '\0';
            return str_intern(buf);
        }
        case TYPE_UNION:      return "union";
        case TYPE_ANY:        return "any";
        default:
            fatal("Unhandled type");
    }
}
