
#include "types.h"

TargetMetrics *TargetTypeMetrics = NULL;

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

Type *UntypedIntType;
Type *UntypedFloatType;

i32 TypeRank(Type *type) {

    // based off of Go's
//        InvalidType,
//        BoolType,
//        IntType,
//        I8Type,
//        I16Type,
//        I32Type,
//        I64Type,
//        IntptrType,
//        UintType,
//        U8Type,
//        U16Type,
//        U32Type,
//        U64Type,
//        UintptrType,
//        RawptrType,
//        F32Type,
//        F64Type,
//        UntypedIntType,
//        UntypedFloatType,

    // FIXME: This poorly emulates the above order. Made in a rush.
    //  We should add typeid's like bitwise has and use those for type promotion.
    if (type->Flags & TypeFlag_Untyped) return type->kind == TypeKind_Float ? 11000 : 10000;
    switch (type->kind) {
        case TypeKind_Invalid:
            return 1;
        case TypeKind_Int:
            return (type->Flags & TypeFlag_Signed) ? 1000 : 2000 + type->Width;
        case TypeKind_Float:
            return 3000 + type->Width;
        case TypeKind_Pointer:
            return 4000;
        default:
            return UINT32_MAX;
    }
}


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

b32 declareSymbol(Package *pkg, Scope *scope, const char *name, Symbol **symbol, u64 declId, Decl *decl);
void declareBuiltinType(const char *name, Type *type) {
    name = StrIntern(name);
    Symbol *symbol;
    declareSymbol(&builtinPackage, builtinPackage.scope, name, &symbol, 0, NULL);
    symbol->state = SymbolState_Resolved;
    symbol->type = type;
    symbol->kind = SymbolKind_Type;
}

Type *AllocType(TypeKind kind) {
    Type *type = Calloc(DefaultAllocator, 1, sizeof(Type));
    type->kind = kind;
    return type;
}

void completeType(Type *type) {
    // TODO: @CircularTypes
}

Map internPointerTypes;

Type *NewTypePointer(TypeFlag flags, Type *pointeeType) {
    Type *type = MapGet(&internPointerTypes, pointeeType);
    if (!type) {
        type = AllocType(TypeKind_Pointer);
        type->Width = RawptrType->Width;
        type->Flags = flags;
        type->Pointer.pointeeType = pointeeType;
        MapSet(&internPointerTypes, pointeeType, type);
    }
    return type;
}

Map internSliceTypes;

Type *NewTypeSlice(TypeFlag flags, Type *elementType)  {
    Type *type = MapGet(&internSliceTypes, elementType);
    if (!type) {
        type = AllocType(TypeKind_Pointer);
        type->Width = RawptrType->Width;
        type->Flags = flags;
        type->Slice.elementType = elementType;
        MapSet(&internSliceTypes, elementType, type);
    }
    return type;
}

typedef struct InternType InternType;
struct InternType {
    Type *type;
    InternType *next;
};

Map internArrayTypes;

Type *NewTypeArray(TypeFlag flags, u64 length, Type *elementType) {
    u64 hash = HashMix(HashPtr(elementType), HashU64(length));
    u64 key = hash ? hash : 1;
    InternType *intern = MapGet(&internArrayTypes, (void*) key);
    for (InternType *it = intern; it; it = it->next) {
        Type *type = it->type;
        if (type->Array.elementType == elementType && type->Array.length == type->Array.length) {
            return type;
        }
    }
    completeType(elementType);
    Type *type = AllocType(TypeKind_Array);
    ASSERT(length * elementType->Width < UINT32_MAX); // FIXME: Error for oversized arrays
    type->Width = (u32) length * elementType->Width;
    type->Align = elementType->Align;
    type->Flags = flags;
    type->Array.length = length;

    InternType *newIntern = Alloc(DefaultAllocator, sizeof(InternType));
    newIntern->type = type;
    newIntern->next = intern;
    MapSet(&internArrayTypes, (void*) key, newIntern);
    return type;
}

Map internFunctionTypes;

Type *NewTypeFunction(TypeFlag flags, DynamicArray(Type *) params, DynamicArray(Type *) results) {
    u64 hash = HashMix(HashBytes(params, ArrayLen(params)), HashBytes(results, ArrayLen(results)));
    u64 key = hash ? hash : 1;
    InternType *intern = MapGet(&internFunctionTypes, (void*) key);
    for (InternType *it = intern; it; it = it->next) {
        Type *type = it->type;
        if (ArraysEqual(params, type->Function.params) && ArraysEqual(results, type->Function.results) && flags == type->Function.Flags) {
            return type;
        }
    }
    Type *type = AllocType(TypeKind_Function);
    type->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width;
    type->Align = TargetTypeMetrics[TargetMetrics_Pointer].Align;
    type->Flags = flags;
    type->Function.params = params;
    type->Function.results = results;
    InternType *newIntern = Alloc(DefaultAllocator, sizeof(InternType));
    newIntern->type = type;
    newIntern->next = intern;
    MapSet(&internArrayTypes, (void*) key, newIntern);
    return type;
}

Type *NewTypeStruct(TypeFlag flags, DynamicArray(Type *) members) {
    UNIMPLEMENTED();
    return NULL;
}

Type *NewTypeUnion(TypeFlag flags, DynamicArray(Type *) cases)  {
    UNIMPLEMENTED();
    return NULL;
}

Scope *pushScope(Package *pkg, Scope *parent);
b32 declareSymbol(Package *pkg, Scope *scope, const char *name, Symbol **symbol, u64 declId, Decl *decl);

void declareBuiltinSymbol(const char *name, Symbol **symbol, SymbolKind kind, Type *type, Val val) {
    b32 dup = declareSymbol(&builtinPackage, builtinPackage.scope, StrIntern(name), symbol, 0, NULL);
    ASSERT(!dup);
    (*symbol)->type = type;
    (*symbol)->val = val;
    (*symbol)->used = true;
    (*symbol)->state = SymbolState_Resolved;
    (*symbol)->kind = kind;
}

bool HaveInitializedBuiltins = false;
void InitBuiltins() {
    if (HaveInitializedBuiltins) return;
    HaveInitializedBuiltins = true;

    builtinPackage.scope = pushScope(&builtinPackage, NULL);

#define TYPE(_global, _name, _kind, _width, _flags) \
    _global = AllocType(TypeKind_##_kind); \
    _global->Width = _width; \
    _global->Align = _width; \
    _global->Flags = _flags; \
    declareBuiltinType(_name, _global)

    TYPE(InvalidType, "<invalid>", Invalid, 0, TypeFlag_None);

    TYPE(AnyType,   "any",  Any, 128, TypeFlag_None);
    TYPE(VoidType, "void", Void, 0, TypeFlag_None);
    TYPE(BoolType, "bool", Int, 8, TypeFlag_Untyped);

    TYPE(I8Type,  "i8",  Int,  8, TypeFlag_Signed);
    TYPE(I16Type, "i16", Int, 16, TypeFlag_Signed);
    TYPE(I32Type, "i32", Int, 32, TypeFlag_Signed);
    TYPE(I64Type, "i64", Int, 64, TypeFlag_Signed);
    TYPE(U8Type,  "u8",  Int,  8, TypeFlag_None);
    TYPE(U16Type, "u16", Int, 16, TypeFlag_None);
    TYPE(U32Type, "u32", Int, 32, TypeFlag_None);
    TYPE(U64Type, "u64", Int, 64, TypeFlag_None);

    TYPE(F32Type, "f32", Float, 32, TypeFlag_None);
    TYPE(F64Type, "f64", Float, 64, TypeFlag_None);

    TYPE(IntType,   "int", Int, 32, TypeFlag_Signed);
    TYPE(UintType, "uint", Int, 32, TypeFlag_None);

    TYPE(IntptrType,   "intptr", Int, 64, TypeFlag_Signed);
    TYPE(UintptrType, "uintptr", Int, 64, TypeFlag_None);
    TYPE(RawptrType,   "rawptr", Pointer, 64, TypeFlag_None);

    TYPE(UntypedIntType, "<integer>",   Int, 64, TypeFlag_Untyped);
    TYPE(UntypedFloatType, "<float>", Float, 64, TypeFlag_Untyped);

#undef TYPE

    declareBuiltinSymbol("false", &FalseSymbol, SymbolKind_Constant, BoolType, (Val){.i64 = 0});
    declareBuiltinSymbol("true",  &TrueSymbol,  SymbolKind_Constant, BoolType, (Val){.i64 = 1});

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

    AnyType->Align = TargetTypeMetrics[TargetMetrics_Pointer].Align;
    AnyType->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width * 2;

    IntType->Align = IntType->Width = TargetTypeMetrics[TargetMetrics_Int].Width;
    UintType->Align = UintType->Width = TargetTypeMetrics[TargetMetrics_Int].Width;

    UintptrType->Align = UintptrType->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width;
    IntptrType->Align = IntptrType->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width;
    RawptrType->Align = RawptrType->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width;
}

const char *DescribeType(Type *type) {
    // FIXME(Brett): just temp output
    if (type) {
        return DescribeTypeKind(type->kind);
    }

    return DescribeTypeKind(TypeKind_Invalid);
}

#if TEST
void test_TypeIntern() {
    INIT_COMPILER();

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

    ASSERT(I8Type->Width  ==  8);
    ASSERT(I16Type->Width == 16);
    ASSERT(I32Type->Width == 32);
    ASSERT(I64Type->Width == 64);
    ASSERT(U8Type->Width  ==  8);
    ASSERT(U16Type->Width == 16);
    ASSERT(U32Type->Width == 32);
    ASSERT(U64Type->Width == 64);

    ASSERT(F32Type->Width == 32);
    ASSERT(F64Type->Width == 64);

    
}
#endif
