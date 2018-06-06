
#include "types.h"

Type *InvalidType;
Type *AnyType;
Type *VoidType;

Type *BoolType;

Type *I8Type;
Type *I16Type;
Type *I32Type;
Type *I64Type;

Type *U8Type;
Type *U16Type;
Type *U32Type;
Type *U64Type;

Type *F32Type;
Type *F64Type;

Type *IntType;
Type *UintType;

Type *IntptrType;
Type *UintptrType;
Type *RawptrType;

// TODO(Brett): figure out how I want to handle instance vs metatypes
Type *UntypedIntType;
Type *UntypedFloatType;

Symbol *FalseSymbol;
Symbol *TrueSymbol;

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

Arena typeInternArena;

Type *TypeIntern(Type type) {
    Type *intern = ArenaAlloc(&typeInternArena, sizeof(Type));
    memcpy(intern, &type, sizeof(Type));
    return intern;
}

Type *buildBuiltinIntern(Type type) {
    Type *t = ArenaAlloc(&typeInternArena, sizeof(Type));
    memcpy(t, &type, sizeof(Type));

    Type metatype = {.kind = TypeKind_Metatype};
    metatype.Metatype.instanceType = t;
    return TypeIntern(metatype);
}

Symbol *buildTypeSymbol(const char *name, Type *type) {
    Symbol *symbol = ArenaAlloc(&typeInternArena, sizeof(Symbol));
    symbol->name = name;
    symbol->kind = SymbolKind_Type;
    symbol->state = SymbolState_Resolved;
    symbol->type = type;
    return symbol;
}

Symbol *symbolIntern(Symbol symbol) {
    Symbol *intern = ArenaAlloc(&typeInternArena, sizeof(Symbol));
    memcpy(intern, &symbol, sizeof(Symbol));
    return intern;
}

Type *NewTypePointer(TypeFlag flags, Type *pointeeType) {
    return NULL;
}

Type *NewTypeArray(TypeFlag flags, i64 length, Type *elementType) {
    return NULL;
}

Type *NewTypeSlice(TypeFlag flags, Type *elementType)  {
    return NULL;
}

Type *NewTypeAny(TypeFlag flags) {
    return NULL;
}

Type *NewTypeStruct(TypeFlag flags, DynamicArray(Type *) members) {
    UNIMPLEMENTED();
    return NULL;
}

Type *NewTypeUnion(TypeFlag flags, DynamicArray(Type *) cases)  {
    UNIMPLEMENTED();
    return NULL;
}

Type *NewTypeFunction(TypeFlag flags, DynamicArray(Type *) params, DynamicArray(Type *) results) {
    return NULL;
}

#define TY(_global, _name, _kind, _width, _flags) \
    _global = buildBuiltinIntern((Type){ .kind = TypeKind_##_kind, .width = _width, .Flags = _flags }); \
    const char *intern##_global = StrIntern(_name); \
    ArrayPush(Types, _global); \
    MapSet(&TypesMap, intern##_global, buildTypeSymbol(intern##_global, _global))

void InitBuiltinTypes() {
    static b32 init;
    if (init) return;

    TY(InvalidType, "<invalid>", Invalid, 0, TypeFlag_None);

    TY(AnyType,   "any",  Any, 0, TypeFlag_None);
    TY(VoidType, "void", Void, 0, TypeFlag_None);
    TY(BoolType, "bool", Bool, 8, TypeFlag_None);

    TY(I8Type,  "i8",  Int,  8, TypeFlag_Signed);
    TY(I16Type, "i16", Int, 16, TypeFlag_Signed);
    TY(I32Type, "i32", Int, 32, TypeFlag_Signed);
    TY(I64Type, "i64", Int, 64, TypeFlag_Signed);
    TY(U8Type,  "u8",  Int,  8, TypeFlag_None);
    TY(U16Type, "u16", Int, 16, TypeFlag_None);
    TY(U32Type, "u32", Int, 32, TypeFlag_None);
    TY(U64Type, "u64", Int, 64, TypeFlag_None);

    TY(F32Type, "f32", Float, 32, TypeFlag_None);
    TY(F64Type, "f64", Float, 64, TypeFlag_None);

    TY(IntType,   "int", Int, 32, TypeFlag_Signed);
    TY(UintType, "uint", Int, 32, TypeFlag_None);

    TY(IntptrType,   "intptr", Int, 64, TypeFlag_Signed);
    TY(UintptrType, "uintptr", Int, 64, TypeFlag_None);
    TY(RawptrType,   "rawptr", Pointer, 64, TypeFlag_None);

    TY(UntypedIntType, "<integer>",   Int, 64, TypeFlag_Untyped);
    TY(UntypedFloatType, "<float>", Float, 64, TypeFlag_Untyped);

    FalseSymbol = symbolIntern((Symbol){
        .name = StrIntern("false"),
        .kind = SymbolKind_Constant,
        .state = SymbolState_Resolved,
        .type = BoolType
    });

    TrueSymbol = symbolIntern((Symbol){
        .name = StrIntern("true"),
        .kind = SymbolKind_Constant,
        .state = SymbolState_Resolved,
        .type = BoolType
    });

    TargetMetrics *TargetTypeMetrics = NULL;

    switch (TargetOs) {
        case Os_Linux:
            TargetTypeMetrics = Os_Linux_ArchSupport[TargetArch];
            break;
        case Os_Darwin:
            TargetTypeMetrics = Os_Darwin_ArchSupport[TargetArch];
            break;
        case Os_Windows:
            break;

        default:
            break;
    }
    if (!TargetTypeMetrics) {
        printf("Unsupported os & arch combination: %s/%s\n", OsNames[TargetOs], ArchNames[TargetArch]);
        exit(1);
    }

    IntType->width = TargetTypeMetrics[TargetMetrics_Int].Width;
    UintType->width = TargetTypeMetrics[TargetMetrics_Int].Width;

    UintptrType->width = TargetTypeMetrics[TargetMetrics_Pointer].Width;
    IntptrType->width = TargetTypeMetrics[TargetMetrics_Pointer].Width;
    RawptrType->width = TargetTypeMetrics[TargetMetrics_Pointer].Width;

    init = true;
}

const char *DescribeType(Type *type) {
    // FIXME(Brett): just temp output
    if (type) {
        return DescribeTypeKind(type->kind);
    }

    return DescribeTypeKind(TypeKind_Invalid);
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

    ASSERT(I8Type->Metatype.instanceType->width  ==  8);
    ASSERT(I16Type->Metatype.instanceType->width == 16);
    ASSERT(I32Type->Metatype.instanceType->width == 32);
    ASSERT(I64Type->Metatype.instanceType->width == 64);
    ASSERT(U8Type->Metatype.instanceType->width  ==  8);
    ASSERT(U16Type->Metatype.instanceType->width == 16);
    ASSERT(U32Type->Metatype.instanceType->width == 32);
    ASSERT(U64Type->Metatype.instanceType->width == 64);

    ASSERT(F32Type->Metatype.instanceType->width == 32);
    ASSERT(F64Type->Metatype.instanceType->width == 64);
}
#endif

#if TEST
void test_TypeInternMap() {
    InitKeywords();
    InitBuiltinTypes();

    Symbol *symbol = MapGet(&TypesMap, StrIntern("i32"));
    ASSERT(symbol);
    ASSERT(symbol->kind == SymbolKind_Type);
    ASSERT(symbol->type == I32Type);
}
#endif
