
typedef u8 CheckerInfoKind;
enum CheckerInfoKindEnum {
    // None is the zero value and so the default for zero initialized checker info.
    CheckerInfoKind_None,
    CheckerInfoKind_Constant,
    CheckerInfoKind_Variable,
    CheckerInfoKind_Ident,
    CheckerInfoKind_Selector,
    CheckerInfoKind_BasicExpr,
    CheckerInfoKind_Label,
    CheckerInfoKind_Goto,
    CheckerInfoKind_For,
    CheckerInfoKind_Switch,
    CheckerinfoKind_Case,
    NUM_CHECKER_INFO_KINDS,
};

STATIC_ASSERT(_StmtKind_End <= UINT8_MAX, "enum values overflow storage type");

typedef u8 Conversion;
#define ConversionKind_Mask 0x07  // Lower 3 bits denote the class
#define ConversionKind_None 0
#define ConversionKind_Same 1
#define ConversionKind_FtoI 2
#define ConversionKind_ItoF 3
#define ConversionKind_PtoI 4
#define ConversionKind_ItoP 5
#define ConversionKind_Bool 6
#define ConversionKind_Any  7

#define ConversionFlag_Extend 0x10 // 0001
#define ConversionFlag_Signed 0x20 // 0010
#define ConversionFlag_Float  0x40 // 0100 (Source type is a Float)

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

typedef u8 SelectorKind;
#define SelectorKind_None   0x0
#define SelectorKind_Struct 0x1

typedef struct Selector_Struct Selector_Struct;
struct Selector_Struct {
    u32 offset;
};

typedef union SelectorValue SelectorValue;
union SelectorValue {
    Selector_Struct Struct;
};

typedef struct CheckerInfo_Selector CheckerInfo_Selector;
struct CheckerInfo_Selector {
    Conversion coerce;
    Type *type;
    b8 isConstant;
    Val val;
    
    SelectorKind kind;
    SelectorValue value;
};

typedef struct CheckerInfo_BasicExpr CheckerInfo_BasicExpr;
struct CheckerInfo_BasicExpr {
    Conversion coerce;
    Type *type;
    b8 isConstant;
    Val val;
};

typedef struct CheckerInfo_Label CheckerInfo_Label;
struct CheckerInfo_Label {
    Symbol *symbol;
};

typedef struct CheckerInfo_Goto CheckerInfo_Goto;
struct CheckerInfo_Goto {
    // NOTE: When the statement provides an expression this can be NULL. In this case the backend should generate the
    //  expression value and branch to the address returned
    Symbol *target;
};

typedef struct CheckerInfo_For CheckerInfo_For;
struct CheckerInfo_For {
    Symbol *continueTarget;
    Symbol *breakTarget;
};

typedef struct CheckerInfo_Switch CheckerInfo_Switch;
struct CheckerInfo_Switch {
    Symbol *breakTarget;
};

typedef struct CheckerInfo_Case CheckerInfo_Case;
struct CheckerInfo_Case {
    Symbol *fallthroughTarget;
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
        CheckerInfo_Label Label;
        CheckerInfo_Goto Goto;
        CheckerInfo_For For;
        CheckerInfo_Switch Switch;
        CheckerInfo_Case Case;
    };
};

#ifdef __cplusplus
extern "C" {
#endif
Symbol *Lookup(Scope *scope, const char *name);
Type *TypeFromCheckerInfo(CheckerInfo info);
b32 IsInteger(Type *type);
b32 IsSigned(Type *type);
b32 IsFloat(Type *type);
#ifdef __cplusplus
}
#endif

