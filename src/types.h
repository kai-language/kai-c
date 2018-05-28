
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

#define TYPE_KINDS               \
    FOR_EACH(Invalid, "invalid") \
    FOR_EACH(Void, "void")       \
    FOR_EACH(Bool, "bool")       \
    FOR_EACH(Int, "int")         \
    FOR_EACH(Float, "float")     \
    FOR_EACH(Pointer, "pointer") \
    FOR_EACH(Array, "array")     \
    FOR_EACH(Slice, "slice")     \
    FOR_EACH(Any, "any")         \
    FOR_EACH(Struct, "struct")   \
    FOR_EACH(Union, "union")     \
    FOR_EACH(Metatype, "meta")   \
    FOR_EACH(Alias, "alias")     \

typedef enum TypeKind {
#define FOR_EACH(kind, ...) TypeKind_##kind,
    TYPE_KINDS
#undef FOR_EACH
} TypeKind;

struct TypeKind_Invalid {
    // NOTE: this is for VC++ that doesn't support empty structs
    b8 __PADDING__;
};

struct TypeKind_Void {
    b8 isNoReturn;
};

struct TypeKind_Bool {
    b8 flags;
};

struct TypeKind_Int {
    b8 isSigned;
};

struct TypeKind_Float {
    b8 flags;
};

struct TypeKind_Pointer {
    Type *pointeeType;
};

struct TypeKind_Array {
    u32 length;
    Type *elementType;
};

struct TypeKind_Slice {
    u32 flags;
    Type *elementType;
};

struct TypeKind_Any {
    b8 flags;
};

struct TypeKind_Struct {
    u32 flags;
    DynamicArray(Type *) members;
};

struct TypeKind_Union {
    u32 flags;
    u32 tagWidth;
    u32 dataWidth;
    DynamicArray(Type *) cases;
};

struct TypeKind_Metatype {
    Type *instanceType;
};

struct TypeKind_Alias {
    Symbol *symbol;
};

struct Type {
    TypeKind kind;
    u32 width;

    union {
    #define FOR_EACH(kind, ...) struct TypeKind_##kind kind;
        TYPE_KINDS
    #undef FOR_EACH
    };
};
