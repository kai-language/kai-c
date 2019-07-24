#pragma once

// Requires ast.h

// checker.h
typedef struct Sym Sym;

typedef struct Ty Ty;

typedef enum IntegerFlags {
    INTEGER_FLAGS_NONE = 0,
    SIGNED = 0x01,
} IntegerFlags;

typedef enum StructFlags {
    STRUCT_FLAGS_NONE = 0,
    TUPLE  = 0x01,
    OPAQUE = 0x04,
} StructFlags;

typedef enum SliceFlags {
    SLICE_FLAGS_NONE = 0,
    STRING = 0x01, // allows coercion to *u8
} SliceFlags;

typedef enum AnyFlags {
    ANY_FLAGS_NONE = 0,
    CVARG = 0x01, // indicates to llvm backend not to perform the coercion
} AnyFlags;

typedef enum TyKind {
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
} TyKind;

typedef struct TyEnum TyEnum;
struct TyEnum {
    u32 filler;
};

typedef struct TyField TyField;
struct TyField {
    const char *name;
    Ty *type;
    u64 offset;
};

typedef struct TyAggregate TyAggregate;
struct TyAggregate {
    TyField *fields;
};

typedef struct TyPointer TyPointer;
struct TyPointer {
    Ty *base;
};

typedef struct TySlice TySlice;
struct TySlice {
    Ty *eltype;
};

typedef struct TyArray TyArray;
struct TyArray {
    Ty *eltype;
    u64 length;
};

typedef struct TyFunc TyFunc;
struct TyFunc {
    Ty **params; // arr
    Ty *result; // TypeStruct
};

struct Ty {
    TyKind kind : 8;
    u8 flags;
    u32 size;
    u32 align;
    u8 bitmask;
    Sym *sym;
    Ty *base;
    u32 tyid;
    union {
        TyFunc tfunc;
        TyEnum tenum;
        TyArray tarray;
        TySlice tslice;
        TyPointer tptr;
        TyAggregate taggregate;
    };
};

extern Ty *type_string;
extern Ty *type_u8ptr;
extern Ty *type_rawptr;
extern Ty *type_uintptr;
extern Ty *type_intptr;
extern Ty *type_uint;
extern Ty *type_int;
extern Ty *type_f64;
extern Ty *type_f32;
extern Ty *type_u64;
extern Ty *type_u32;
extern Ty *type_u16;
extern Ty *type_u8;
extern Ty *type_i64;
extern Ty *type_i32;
extern Ty *type_i16;
extern Ty *type_i8;
extern Ty *type_bool;
extern Ty *type_void;
extern Ty *type_any;
extern Ty *type_cvarg;
extern Ty *type_invalid;

void init_types(void);

Ty *type_func(Ty **params, Ty *result, FuncFlags flags);
Ty *type_struct(TyField *fields, u32 size, u32 align, u8 flags);
Ty *type_union(TyField *fields, u32 size, u32 align, u8 flags);
Ty *type_enum(u8 flags);
Ty *type_ptr(Ty *base, u8 flags);
Ty *type_array(Ty *eltype, u64 length, u8 flags);
Ty *type_slice(Ty *eltype, u8 flags);
Ty *type_alias(Ty *base, Sym *sym);
bool types_eql(Ty *a, Ty *b);

const char *tyname(Ty *type);
u64 type_max_value(Ty *type);
u64 powi(u64 base, u64 exp);
Ty *min_int_for_neg_value(i64 val);
Ty *min_int_for_pos_value(u64 val);
u32 aggregate_field_index(Ty *type,const char *name);
Ty *smallest_unsigned_int_for_value(u64 val);
Ty *smallest_signed_int_for_value(i64 val);
bool is_logical(Ty *type);
bool is_comparable(Ty *type);
bool is_equatable(Ty *type);
bool is_bitwisable(Ty *type);
bool is_signed(Ty *type);
bool is_aggregate(Ty *type);
bool is_scalar(Ty *type);
bool is_arithmetic(Ty *type);
bool is_arithmetic_or_ptr(Ty *type);
bool is_float(Ty *type);
bool is_bool(Ty *type);
bool is_integer(Ty *type);
bool is_slice(Ty *type);
bool is_array(Ty *type);
bool is_ptr_like_type(Ty *type);
bool is_func(Ty *type);
bool is_ptr(Ty *type);

