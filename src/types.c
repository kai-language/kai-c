
#include "all.h"
#include "ast.h"
#include "types.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "compiler.h"

Type *type_invalid = &(Type){ TYPE_INVALID };

Type *type_any  = &(Type){ TYPE_ANY };
Type *type_void = &(Type){ TYPE_VOID };
Type *type_bool = &(Type){ TYPE_BOOL };

Type *type_i8  = &(Type){ TYPE_INT,  8,  8, SIGNED };
Type *type_i16 = &(Type){ TYPE_INT, 16, 16, SIGNED };
Type *type_i32 = &(Type){ TYPE_INT, 32, 32, SIGNED };
Type *type_i64 = &(Type){ TYPE_INT, 64, 64, SIGNED };

Type *type_u8  = &(Type){ TYPE_INT,  8,  8 };
Type *type_u16 = &(Type){ TYPE_INT, 16, 16 };
Type *type_u32 = &(Type){ TYPE_INT, 32, 32 };
Type *type_u64 = &(Type){ TYPE_INT, 64, 64 };

Type *type_f32 = &(Type){ TYPE_FLOAT, 32, 32 };
Type *type_f64 = &(Type){ TYPE_FLOAT, 64, 64 };

Type *type_int  = &(Type){ TYPE_INT };
Type *type_uint = &(Type){ TYPE_INT };

Type *type_intptr  = &(Type){ TYPE_INT };
Type *type_uintptr = &(Type){ TYPE_INT };
Type *type_rawptr  = &(Type){ TYPE_PTR };

Type *type_string;

bool is_ptr(Type *type) {
    return type && type->kind == TYPE_PTR;
}

bool is_func(Type *type) {
    return type && type->kind == TYPE_FUNC;
}

bool is_ptr_like_type(Type *type) {
    return type && (type->kind == TYPE_PTR || type->kind == TYPE_FUNC);
}

bool is_array(Type *type) {
    return type && type->kind == TYPE_ARRAY;
}

bool is_bool(Type *type) {
    return type && type->kind == TYPE_PTR;
}

bool is_integer(Type *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_ENUM);
}

bool is_float(Type *type) {
    return type && type->kind == TYPE_FLOAT;
}

bool is_arithmetic(Type *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_FLOAT);
}

bool is_scalar(Type *type) {
    return type && (TYPE_BOOL <= type->kind && type->kind <= TYPE_FUNC);
}

bool is_aggregate(Type *type) {
    return type->kind == TYPE_STRUCT || type->kind == TYPE_UNION;
}

bool is_signed(Type *type) {
    return is_integer(type) && (type->flags & SIGNED) != 0;
}

bool is_bitwisable(Type *type) {
    return is_integer(type) || (type && type->kind == TYPE_ENUM && (type->flags & ENUM_FLAGS) != 0);
}

bool is_equatable(Type *type) {
    static b32 equatables[NUM_TYPE_KINDS] = {
        [TYPE_INT] = true,
        [TYPE_PTR] = true,
        [TYPE_ENUM] = true,
        [TYPE_FLOAT] = true,
    };
    return equatables[type->kind];
}

bool is_comparable(Type *type) {
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

bool is_logical(Type *type) {
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

Type *smallest_signed_int_for_value(i64 val) {
    val = MIN(val, INT8_MIN);
    if (val == INT8_MIN) return type_i8;
    val = MIN(val, INT16_MIN);
    if (val == INT16_MIN) return type_i16;
    val = MIN(val, INT32_MIN);
    if (val == INT32_MIN) return type_i32;
    return type_i64;
}

Type *smallest_unsigned_int_for_value(u64 val) {
    val = MAX(val, UINT8_MAX);
    if (val == UINT8_MAX) return type_u8;
    val = MAX(val, UINT16_MAX);
    if (val == UINT16_MAX) return type_u16;
    val = MAX(val, UINT32_MAX);
    if (val == UINT32_MAX) return type_u32;
    return type_u64;
}

u32 aggregate_field_index(Type *type, const char *name) {
    ASSERT(is_aggregate(type));
    for (u32 i = 0; i < arrlen(type->taggregate.fields); i++) {
        if (type->taggregate.fields[i].name == name) return i;
    }
    return -1;
}

u64 type_max_value(Type *type) {
    ASSERT(is_integer(type) || is_ptr(type));
    if (type->flags & SIGNED)
        return powi(2, type->size) / 2 - 1;
    return powi(2, type->size) - 1;
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

Type *type_alloc(TypeKind kind, u8 flags, size_t size) {
    Type *type = arena_alloc(&compiler.arena, size);
    type->kind = kind;
    type->flags = flags;
    return type;
}

#define type_size(type, member) offsetof(type, member) + sizeof(((type *)0)->member)

Type *type_func(const char **labels, Type **params, Type *result, FuncFlags flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_FUNC, (u8) flags, type_size(Type, tfunc));
    type->tfunc.labels = labels;
    type->tfunc.params = params;
    type->tfunc.result = result;
    return type;
}

Type *type_struct(TypeField *fields, u8 flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_STRUCT, flags, type_size(Type, taggregate));
    type->taggregate.fields = fields;
    return type;
}

Type *type_union(TypeField *fields, u8 flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_UNION, flags, type_size(Type, taggregate));
    type->taggregate.fields = fields;
    return type;
}

Type *type_enum(u8 flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_ENUM, flags, type_size(Type, tenum));
    return type;
}

Type *type_ptr(Type *base, u8 flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_PTR, flags, type_size(Type, tptr));
    type->tptr.base = base;
    return type;
}

Type *type_array(Type *eltype, u64 length, u8 flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_ARRAY, flags, type_size(Type, tarray));
    type->tarray.eltype = eltype;
    type->tarray.length = length;
    return type;
}

Type *type_slice(Type *eltype, u64 length, u8 flags) {
    TRACE(CHECKING);
    Type *type = type_alloc(TYPE_SLICE, flags, type_size(Type, tslice));
    type->tslice.eltype = eltype;
    return type;
}

const char *typename(Type *type) {
    switch (type->kind) {
    case TYPE_INVALID:    return "invalid";
    case TYPE_COMPLETING: return "completing";
    case TYPE_VOID:       return "void";
    case TYPE_BOOL:       return "bool";
    case TYPE_INT:        return "int";
    case TYPE_ENUM:       return "enum";
    case TYPE_FLOAT:      return "float";
    case TYPE_PTR:        return "ptr";
    case TYPE_FUNC:       return "func";
    case TYPE_ARRAY:      return "array";
    case TYPE_SLICE:      return "slice";
    case TYPE_STRUCT:     return "struct";
    case TYPE_UNION:      return "union";
    case TYPE_ANY:        return "any";
    default:
        fatal("Unhandled type");
    }
}
