#include "types.h"

const char *TypeKindDescriptions[] = {
#define FOR_EACH(kind, text) [TypeKind_##kind] = "" #text "",
    TYPE_KINDS
#undef FOR_EACH
};

const char *DescribeTypeKind(TypeKind kind) {
    return TypeKindDescriptions[kind];
}

DynamicArray(const Type *) Types;
Map TypesMap;

#define TYPE(_kind, name, _type, _width) _kind##Type = TypeIntern((Type){.kind = TypeKind_##_type, .width = _width}); ArrayPush(Types, _kind##Type); MapSet(&TypesMap, name, _kind##Type)

Arena typeInternArena;

Type *TypeIntern(Type type) {
    Type *intern = ArenaAlloc(&typeInternArena, sizeof(Type));
    memcpy(intern, &type, sizeof(Type));
    return intern;
}

void InitBuiltinTypes() {
    static b32 init;
    if (init) return;

    TYPE(Invalid, "<invalid>", Invalid, 0);

    TYPE(Any,  "any",  Any,  0);
    TYPE(Void, "void", Void, 0);

    TYPE(Bool, "bool", Bool, 8);

    TYPE(I8,  "i8",  Int,  8);
    TYPE(I16, "i16", Int, 16);
    TYPE(I32, "i32", Int, 32);
    TYPE(I64, "i64", Int, 64);
    TYPE(U8,  "u8",  Int,  8);
    TYPE(U16, "u16", Int, 16);
    TYPE(U32, "u32", Int, 32);
    TYPE(U64, "u64", Int, 64);

    TYPE(F32, "f32", Float, 32);
    TYPE(F64, "f64", Float, 64);

    init = true;
}

#undef TYPE

#if TEST
void test_TypeIntern() {
    InitBuiltinTypes();

    ASSERT(InvalidType);
    ASSERT(AnyType);
    ASSERT(VoidType);
    ASSERT(BoolType);

    ASSERT(I8Type);
    ASSERT(I16Type);
    ASSERT(I32Type);
    ASSERT(I64Type);
    ASSERT(U8Type);
    ASSERT(U16Type);
    ASSERT(U32Type);
    ASSERT(U64Type);

    ASSERT(I8Type->width == 8);
    ASSERT(I16Type->width == 16);
    ASSERT(I32Type->width == 32);
    ASSERT(I64Type->width == 64);
    ASSERT(U8Type->width == 8);
    ASSERT(U16Type->width == 16);
    ASSERT(U32Type->width == 32);
    ASSERT(U64Type->width == 64);

    ASSERT(F32Type->width == 32);
    ASSERT(F64Type->width == 64);
}
#endif

#if TEST
void test_TypeInternMap() {
    InitBuiltinTypes();

    Type *type = MapGet(&TypesMap, "i32");
    ASSERT(type);
    ASSERT(type == I32Type);
}
#endif
