
extern struct TargetMetrics *TargetTypeMetrics;

extern Type *InvalidType;
extern Type *AnyType;
extern Type *VoidType;

extern Type *BoolType;

extern Type *I8Type;
extern Type *I16Type;
extern Type *I32Type;
extern Type *I64Type;

extern Type *U8Type;
extern Type *U16Type;
extern Type *U32Type;
extern Type *U64Type;

extern Type *F32Type;
extern Type *F64Type;

extern Type *IntType;
extern Type *UintType;
extern Type *IntptrType;
extern Type *UintptrType;
extern Type *RawptrType;

extern Symbol *FalseSymbol;
extern Symbol *TrueSymbol;

#define TYPE_KINDS                  \
    FOR_EACH(Invalid, "invalid")    \
    FOR_EACH(Int, "int")            \
    FOR_EACH(Float, "float")        \
    FOR_EACH(Pointer, "pointer")    \
    FOR_EACH(Array, "array")        \
    FOR_EACH(Slice, "slice")        \
    FOR_EACH(Any, "any")            \
    FOR_EACH(Struct, "struct")      \
    FOR_EACH(Union, "union")        \
    FOR_EACH(Enum, "enum")          \
    FOR_EACH(Function, "function")  \
    FOR_EACH(Tuple, "tuple")        \

typedef u8 TypeKind;
enum Enum_TypeKind {
#define FOR_EACH(kind, ...) TypeKind_##kind,
    TYPE_KINDS
#undef FOR_EACH
    NUM_TYPE_KINDS,
};

#define FOR_EACH(kind, ...) typedef struct Type_##kind Type_##kind;
    TYPE_KINDS
#undef FOR_EACH

typedef u8 TypeFlag;
#define TypeFlag_None 0
#define TypeFlag_Alias    0x80

// Integer
#define TypeFlag_Signed   0x1
#define TypeFlag_Boolean  0x2

// Slice & Function
#define TypeFlag_Variadic 0x1
#define TypeFlag_CVargs   0x2

// Enum
#define TypeFlag_EnumFlags 0x1

// Struct
#define TypeFlag_StructTuple 0x1

// Tuple
#define TypeFlag_NoReturn 0x1

struct Type_Pointer {
    TypeFlag Flags;
    Type *pointeeType;
};

struct Type_Slice {
    TypeFlag Flags;
    Type *elementType;
};

struct Type_Array {
    TypeFlag Flags;
    Type *elementType;
    u64 length;
};

struct Type_Function {
    TypeFlag Flags;
    DynamicArray(Type *) params;
    DynamicArray(Type *) results;
};

typedef struct TypeField TypeField;
struct TypeField {
    const char *name;
    Type *type;
    u32 offset;
};

typedef struct EnumField EnumField;
struct EnumField {
    const char *name;
    u64 val;
};

struct Type_Struct {
    TypeFlag Flags;
    // TODO: This shouldn't be an indirect type
    DynamicArray(TypeField *) members;
};

struct Type_Union {
    TypeFlag Flags;
    u32 tagWidth;
    u32 dataWidth;
    DynamicArray(Type *) cases;
};

struct Type_Enum {
    TypeFlag Flags;
    Type *backingType;
    DynamicArray(EnumField) cases;
};

struct Type_Tuple {
    TypeFlag Flags;
    DynamicArray(Type *) types;
};

STATIC_ASSERT(offsetof(Type_Pointer,  Flags) == 0, "Flags must be at offset 0");
STATIC_ASSERT(offsetof(Type_Array,    Flags) == 0, "Flags must be at offset 0");
STATIC_ASSERT(offsetof(Type_Slice,    Flags) == 0, "Flags must be at offset 0");
STATIC_ASSERT(offsetof(Type_Struct,   Flags) == 0, "Flags must be at offset 0");
STATIC_ASSERT(offsetof(Type_Union,    Flags) == 0, "Flags must be at offset 0");
STATIC_ASSERT(offsetof(Type_Function, Flags) == 0, "Flags must be at offset 0");
STATIC_ASSERT(offsetof(Type_Tuple,    Flags) == 0, "Flags must be at offset 0");

struct Type {
    TypeKind kind;
    u32 Width;
    u32 Align;
    u32 TypeId;
    Symbol *Symbol;

    union {
        TypeFlag Flags;
        Type_Pointer Pointer;
        Type_Array Array;
        Type_Slice Slice;
        Type_Enum Enum;
        Type_Struct Struct;
        Type_Union Union;
        Type_Function Function;
        Type_Tuple Tuple;
    };
};

typedef struct StructFieldLookupResult StructFieldLookupResult;
struct StructFieldLookupResult {
    u32 index;
    TypeField *field;
};

typedef struct EnumFieldLookupResult EnumFieldLookupResult;
struct EnumFieldLookupResult {
    u32 index;
    EnumField *field;
};

#ifdef __cplusplus
extern "C" {
#endif
const char *DescribeType(Type *type);
const char *DescribeTypeKind(TypeKind kind);
Type *SmallestIntTypeForPositiveValue(u64 val);
Type *SmallestIntTypeForNegativeValue(i64 val);
i64 SignExtend(Type *type, Type *target, Val val);
i64 SignExtendTo64Bits(Type *source, Val val);
b32 TypesIdentical(Type *type, Type *target);
u64 MaxValueForIntOrPointerType(Type *type);
StructFieldLookupResult StructFieldLookup(Type_Struct type, const char *name);
#ifdef __cplusplus
}
#endif
