
extern struct TargetMetrics *TargetTypeMetrics;

extern Type *InvalidType;
extern Type *FileType;

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


typedef u8 TypeKind;
enum Enum_TypeKind {
    TypeKind_Invalid,
    TypeKind_Int,
    TypeKind_Float,
    TypeKind_Pointer,
    TypeKind_Array,
    TypeKind_Slice,
    TypeKind_Any,
    TypeKind_Struct,
    TypeKind_Union,
    TypeKind_Enum,
    TypeKind_Function,
    TypeKind_Tuple,
    NUM_TYPE_KINDS,
};

typedef struct Type_Invalid Type_Invalid;
typedef struct Type_Int Type_Int;
typedef struct Type_Float Type_Float;
typedef struct Type_Pointer Type_Pointer;
typedef struct Type_Array Type_Array;
typedef struct Type_Slice Type_Slice;
typedef struct Type_Any Type_Any;
typedef struct Type_Struct Type_Struct;
typedef struct Type_Union Type_Union;
typedef struct Type_Enum Type_Enum;
typedef struct Type_Function Type_Function;
typedef struct Type_Tuple Type_Tuple;

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
    Type **params;
    Type **results;
    u32 numParams;
    u32 numResults;
};

typedef struct TypeField TypeField;
struct TypeField {
    const char *name;
    Type *type;
    u32 offset;
};

struct Type_Struct {
    TypeFlag Flags;
    TypeField *members;
    u32 numMembers;
};

struct Type_Union {
    TypeFlag Flags;
    u32 tagWidth;
    u32 dataWidth;
    Type **cases;
    u32 numCases;
};

struct Type_Enum {
    TypeFlag Flags;
};

struct Type_Tuple {
    TypeFlag Flags;
    Type **types;
    u32 numTypes;
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
void InitBuiltinTypes(Compiler *compiler);
#ifdef __cplusplus
}
#endif
