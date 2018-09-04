
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

Type *StringType;

// TODO: Mechanism to lookup type by their ID
u32 nextTypeId = 1;

u32 TypeRank(Type *type) {
    return type->TypeId;
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

Type *SmallestIntTypeForNegativeValue(i64 val) {
    val = MIN(val, INT8_MIN);
    if (val == INT8_MIN) return I8Type;

    val = MIN(val, INT16_MIN);
    if (val == INT16_MIN) return I16Type;

    val = MIN(val, INT32_MIN);
    if (val == INT32_MIN) return I32Type;

    return I64Type;
}

Type *SmallestIntTypeForPositiveValue(u64 val) {
    val = MAX(val, UINT8_MAX);
    if (val == UINT8_MAX) return U8Type;

    val = MAX(val, UINT16_MAX);
    if (val == UINT16_MAX) return U16Type;

    val = MAX(val, UINT32_MAX);
    if (val == UINT32_MAX) return U32Type;

    return U64Type;
}

u64 MaxValueForIntOrPointerType(Type *type) {
    if (type->Flags & TypeFlag_Signed) {
        return IntegerPower(2, type->Width) / 2 - 1;
    } else {
        // Unsigned
        return IntegerPower(2, type->Width) - 1;
    }
}

// FIXME: This doesn't sign extend to the target size currently, only 64 bits.
i64 SignExtend(Type *type, Type *target, Val val) {
    if (type->Width == 64) return val.i64;
    u64 v = val.u64 & ((1ull << type->Width) - 1);
    u64 mask = 1ull << (type->Width - 1);
    return (v ^ mask) - mask;
}

i64 SignExtendTo64Bits(Type *source, Val val) {
    if (source->Width == 64) return val.i64;
    u64 v = val.u64 & ((1ull << source->Width) - 1);
    u64 mask = 1ull << (source->Width - 1);
    return (v ^ mask) - mask;
}

b32 isAlias(Type *type);
b32 TypesIdentical(Type *type, Type *target) {
    while (isAlias(type)) {
        type = type->Symbol->type;
    }
    while (isAlias(target)) {
        target = target->Symbol->type;
    }
    return type == target;
}

StructFieldLookupResult StructFieldLookup(Type_Struct type, const char *name) {

    u32 index = 0;
    TypeField *field = NULL;
    ForEachWithIndex(type.members, i, TypeField *, it) {
        if (it->name == name) {
            index = (u32) i;
            field = it;
            break;
        }
    }
    
    return (StructFieldLookupResult){index, field};
}

#if TEST
void test_SmallestIntTypeForValue() {
    INIT_COMPILER();

    ASSERT(SmallestIntTypeForPositiveValue(0) == U8Type);
    ASSERT(SmallestIntTypeForPositiveValue(UINT8_MAX) == U8Type);
    ASSERT(SmallestIntTypeForPositiveValue(UINT16_MAX) == U16Type);
    ASSERT(SmallestIntTypeForPositiveValue(UINT32_MAX) == U32Type);
    ASSERT(SmallestIntTypeForPositiveValue(UINT64_MAX) == U64Type);
}
#endif

Type *AllocType(TypeKind kind) {
    Type *type = Calloc(DefaultAllocator, 1, sizeof(Type));
    type->kind = kind;
    type->TypeId = nextTypeId++;
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
        type->Align = RawptrType->Align;
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
        type = AllocType(TypeKind_Slice);
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
    type->Array.elementType = elementType;

    InternType *newIntern = Alloc(DefaultAllocator, sizeof(InternType));
    newIntern->type = type;
    newIntern->next = intern;
    MapSet(&internArrayTypes, (void*) key, newIntern);
    return type;
}

Map internFunctionTypes;

Type *NewTypeFunction(TypeFlag flags, DynamicArray(Type *) params, DynamicArray(Type *) results) {
    u64 hash = HashMix(HashBytes(params, ArrayLen(params) * sizeof(params)), HashBytes(results, ArrayLen(results) * sizeof(results)));
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
    MapSet(&internFunctionTypes, (void*) key, newIntern);
    return type;
}

Type *NewTypeTuple(TypeFlag flags, DynamicArray(Type *) types) {
    Type *type = AllocType(TypeKind_Tuple);
    type->Flags = flags;
    type->Tuple.types = types;
    type->Width = 0;
    type->Align = 0;
    return type;
}

Type *NewTypeStruct(u32 Align, u32 Width, TypeFlag flags, DynamicArray(TypeField *) members) {
    Type *type = AllocType(TypeKind_Struct);
    type->Align = Align;
    type->Width = Width;
    type->Flags = flags;
    type->Struct.members = members;
    return type;
}

Type *NewTypeUnion(TypeFlag flags, DynamicArray(Type *) cases)  {
    UNIMPLEMENTED();
    return NULL;
}

Scope *pushScope(Package *pkg, Scope *parent);
b32 declareSymbol(Package *pkg, Scope *scope, const char *name, Symbol **symbol, Decl *decl);

void declareBuiltinSymbol(const char *name, Symbol **symbol, SymbolKind kind, Type *type, Val val) {
    b32 dup = declareSymbol(&builtinPackage, builtinPackage.scope, StrIntern(name), symbol, NULL);
    ASSERT(!dup);
    (*symbol)->type = type;
    (*symbol)->val = val;
    (*symbol)->used = true;
    (*symbol)->state = SymbolState_Resolved;
    (*symbol)->kind = kind;
}

void declareBuiltinType(const char *name, Type *type) {
    name = StrIntern(name);
    Symbol *symbol;
    declareSymbol(&builtinPackage, builtinPackage.scope, name, &symbol, NULL);
    symbol->state = SymbolState_Resolved;
    symbol->type = type;
    symbol->kind = SymbolKind_Type;
    type->Symbol = symbol;
}

bool HaveInitializedBuiltins = false;
void InitBuiltins() {
    if (HaveInitializedBuiltins) return;
    HaveInitializedBuiltins = true;

    switch (TargetOs) {
        case Os_Linux:
            TargetTypeMetrics = Os_Linux_ArchSupport[TargetArch];
            break;
        case Os_Darwin:
            TargetTypeMetrics = Os_Darwin_ArchSupport[TargetArch];
            break;
        case Os_Windows:
            TargetTypeMetrics = Os_Windows_ArchSupport[TargetArch];
            break;

        default:
            break;
    }
    if (!TargetTypeMetrics) {
        printf("Unsupported os & arch combination: %s/%s\n", OsNames[TargetOs], ArchNames[TargetArch]);
        exit(1);
    }

    builtinPackage.scope = pushScope(&builtinPackage, NULL);

#define TYPE(_global, _name, _kind, _width, _flags) \
    _global = AllocType(TypeKind_##_kind); \
    _global->Width = _width; \
    _global->Align = _width; \
    _global->Flags = _flags; \
    declareBuiltinType(_name, _global)

#define TYPEALIAS(_global, _name, _alias) \
    _global = Alloc(DefaultAllocator, sizeof(Type)); \
    memcpy(_global, _alias, sizeof(Type)); \
    _global->Symbol = _alias->Symbol; \
    _global->Flags |= TypeFlag_Alias; \
    declareBuiltinType(_name, _alias)

    TYPE(InvalidType, "<invalid>", Invalid, 0, TypeFlag_None);
    nextTypeId--;

    // @IMPORTANT: The order is important here as it sets up the TypeId's
    TYPE(AnyType,   "any",  Any, 128, TypeFlag_None); // typeid = 1
    TYPE(VoidType, "void", Tuple, 0, TypeFlag_None);

    TYPE(BoolType, "bool", Int, 1, TypeFlag_Boolean);
    BoolType->Align = 8; // Must be byte aligned

    TYPE(F32Type, "f32", Float, 32, TypeFlag_None);
    TYPE(F64Type, "f64", Float, 64, TypeFlag_None);

    TYPE(I8Type,  "i8",  Int,  8, TypeFlag_Signed);
    TYPE(I16Type, "i16", Int, 16, TypeFlag_Signed);
    TYPE(I32Type, "i32", Int, 32, TypeFlag_Signed);
    TYPE(I64Type, "i64", Int, 64, TypeFlag_Signed);
    TYPE(U8Type,  "u8",  Int,  8, TypeFlag_None);
    TYPE(U16Type, "u16", Int, 16, TypeFlag_None);
    TYPE(U32Type, "u32", Int, 32, TypeFlag_None);
    TYPE(U64Type, "u64", Int, 64, TypeFlag_None);
    // TODO: Do we need an UntypedUintType? ... UntypedIntType cannot represent values over INT64_MAX;

    TYPE(RawptrType, "rawptr", Pointer, 64, TypeFlag_None);

    // Aliases behave in promotion just as the types they alias do.
    TYPEALIAS(IntType,   "int", I32Type);
    TYPEALIAS(UintType, "uint", U32Type);

    TYPE(StringType, "string", Struct, 128, TypeFlag_None);

    switch (TargetTypeMetrics[TargetMetrics_Pointer].Width) {
        case 32:
            TYPEALIAS(IntptrType,   "intptr", I32Type);
            TYPEALIAS(UintptrType, "uintptr", U32Type);
            break;
        case 64:
            TYPEALIAS(IntptrType,   "intptr", I64Type);
            TYPEALIAS(UintptrType, "uintptr", U64Type);
            break;
        default:
            printf("Unsupported pointer width on os & arch %s/%s\n", OsNames[TargetOs], ArchNames[TargetArch]);
            exit(1);
    }

    declareBuiltinSymbol("false", &FalseSymbol, SymbolKind_Constant, BoolType, (Val){.i64 = 0});
    declareBuiltinSymbol("true",  &TrueSymbol,  SymbolKind_Constant, BoolType, (Val){.i64 = 1});

    RawptrType->Pointer.pointeeType = U8Type;

    AnyType->Align = TargetTypeMetrics[TargetMetrics_Pointer].Align;
    AnyType->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width * 2;

    RawptrType->Align = RawptrType->Width = TargetTypeMetrics[TargetMetrics_Pointer].Width;

#undef TYPE
}

const char *DescribeType(Type *type) {
    if (!type) return DescribeTypeKind(TypeKind_Invalid);

    if (type->Symbol) {
        return type->Symbol->name;
    }

    return DescribeTypeKind(type->kind);
}

#if TEST
void test_TypeIntern() {
    TargetArch = Arch_x86_64;
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

    ASSERT(IntptrType->Symbol->type == I64Type);
    ASSERT(UintptrType->Symbol->type == U64Type);

    ASSERT(IntptrType->Width == 64);
    ASSERT(UintptrType->Width == 64);

    ASSERT(RawptrType->Width == 64);
    ASSERT(AnyType->Width == 128);
}
#endif
