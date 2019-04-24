#pragma once

// Requires ast.h

// checker.h
typedef struct Sym Sym;

typedef struct Type Type;

typedef enum IntegerFlags IntegerFlags;
enum IntegerFlags {
    INTEGER_FLAGS_NONE = 0,
    SIGNED = 0x01,
};

typedef enum StructFlags StructFlags;
enum StructFlags {
    STRUCT_FLAGS_NONE = 0,
    TUPLE = 0x01,
};

typedef enum TypeKind TypeKind;
enum TypeKind {
    TYPE_INVALID = 0,
    TYPE_COMPLETING,
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_ENUM,
    TYPE_FLOAT,
    TYPE_PTR,
    TYPE_FUNC,
    TYPE_ARRAY,
    TYPE_SLICE,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ANY,
    NUM_TYPE_KINDS,
};

typedef struct TypeEnum TypeEnum;
struct TypeEnum {

};

typedef struct TypeField TypeField;
struct TypeField {
    const char *name;
    Type *type;
    u64 offset;
};

typedef struct TypeAggregate TypeAggregate;
struct TypeAggregate {
    TypeField *fields;
};

typedef struct TypePointer TypePointer;
struct TypePointer {
    Type *base;
};

typedef struct TypeSlice TypeSlice;
struct TypeSlice {
    Type *eltype;
};

typedef struct TypeArray TypeArray;
struct TypeArray {
    Type *eltype;
    u64 length;
};

typedef struct TypeFunc TypeFunc;
struct TypeFunc {
    const char **labels;
    Type **params; // arr
    Type *result; // TypeStruct
};

struct Type {
    TypeKind kind : 8;
    u8 flags;
    size_t size;
    size_t align;
    Sym *symbol;
    Type *base;
    u32 typeid;
    union {
        TypeFunc tfunc;
        TypeEnum tenum;
        TypeArray tarray;
        TypeSlice tslice;
        TypePointer tptr;
        TypeAggregate taggregate;
    };
};

extern Type *type_string;
extern Type *type_rawptr;
extern Type *type_uintptr;
extern Type *type_intptr;
extern Type *type_uint;
extern Type *type_int;
extern Type *type_f64;
extern Type *type_f32;
extern Type *type_u64;
extern Type *type_u32;
extern Type *type_u16;
extern Type *type_u8;
extern Type *type_i64;
extern Type *type_i32;
extern Type *type_i16;
extern Type *type_i8;
extern Type *type_bool;
extern Type *type_void;
extern Type *type_any;
extern Type *type_invalid;

Type *type_func(const char **labels, Type **params, Type *result, FuncFlags flags);
Type *type_struct(TypeField *fields, u8 flags);
Type *type_union(TypeField *fields, u8 flags);
Type *type_enum(u8 flags);
Type *type_ptr(Type *base, u8 flags);
Type *type_array(Type *eltype, u64 length, u8 flags);
Type *type_slice(Type *eltype, u8 flags);

const char *typename(Type *type);
u64 powi(u64 base,u64 exp);
u64 type_max_value(Type *type);
u32 aggregate_field_index(Type *type,const char *name);
Type *smallest_unsigned_int_for_value(u64 val);
Type *smallest_signed_int_for_value(i64 val);
bool is_logical(Type *type);
bool is_comparable(Type *type);
bool is_equatable(Type *type);
bool is_bitwisable(Type *type);
bool is_signed(Type *type);
bool is_aggregate(Type *type);
bool is_scalar(Type *type);
bool is_arithmetic(Type *type);
bool is_float(Type *type);
bool is_bool(Type *type);
bool is_integer(Type *type);
bool is_slice(Type *type);
bool is_array(Type *type);
bool is_ptr_like_type(Type *type);
bool is_func(Type *type);
bool is_ptr(Type *type);
