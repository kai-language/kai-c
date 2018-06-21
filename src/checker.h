
typedef u8 CheckerInfoKind;
enum {
    // None is the zero value and so the default for zero initialized checker info.
    CheckerInfoKind_None,
    CheckerInfoKind_Constant,
    CheckerInfoKind_Variable,
    CheckerInfoKind_Ident,
    CheckerInfoKind_Selector,
    CheckerInfoKind_BasicExpr,
    NUM_CHECKER_INFO_KINDS,
};

STATIC_ASSERT(_StmtKind_End < UINT8_MAX, "Enumeration stored in u8 has case past 255, overflow will occur");

typedef u8 Conversion;
#define ConversionClass_Mask 0x07  // Lower 3 bits denote the class
#define ConversionClass_None 0
#define ConversionClass_Same 1
#define ConversionClass_FtoI 2
#define ConversionClass_ItoF 3
#define ConversionClass_PtoI 4
#define ConversionClass_ItoP 5
#define ConversionClass_Bool 6
#define ConversionClass_Any  7

#define ConversionFlag_Extend 0x10 // 0001
#define ConversionFlag_Signed 0x20 // 0010

typedef struct CheckerInfo_Constant CheckerInfo_Constant;
struct CheckerInfo_Constant {
    Symbol *symbol;
};

typedef struct CheckerInfo_Variable CheckerInfo_Variable;
struct CheckerInfo_Variable {
    DynamicArray(Symbol *) symbols;
};

typedef struct CheckerInfo_Ident CheckerInfo_Ident;
struct CheckerInfo_Ident {
    Conversion coerce;
    Symbol *symbol;
};

typedef struct CheckerInfo_Selector CheckerInfo_Selector;
struct CheckerInfo_Selector {
    Conversion coerce;
    u32 levelsOfIndirection;
    Val constant;
};

typedef struct CheckerInfo_BasicExpr CheckerInfo_BasicExpr;
struct CheckerInfo_BasicExpr {
    Conversion coerce;
    Type *type;
    b8 isConstant;
    Val val;
};

STATIC_ASSERT(offsetof(CheckerInfo_Ident,     coerce) == 0, "conversion must be at offset 0 for expressions");
STATIC_ASSERT(offsetof(CheckerInfo_Selector,  coerce) == 0, "conversion must be at offset 0 for expressions");
STATIC_ASSERT(offsetof(CheckerInfo_BasicExpr, coerce) == 0, "conversion must be at offset 0 for expressions");

typedef struct CheckerInfo CheckerInfo;
struct CheckerInfo {
    CheckerInfoKind kind;
    union {
        Conversion coerce; // Present when CheckerInfo is for an expression
        CheckerInfo_Constant Constant;
        CheckerInfo_Variable Variable;
        CheckerInfo_Selector Selector;
        CheckerInfo_Ident Ident;
        CheckerInfo_BasicExpr BasicExpr;
    };
};

#ifdef __cplusplus
extern "C" {
#endif
Symbol *Lookup(Scope *scope, const char *name);
b32 IsInteger(Type *type);
b32 IsSigned(Type *type);
b32 IsFloat(Type *type);
#ifdef __cplusplus
}
#endif

