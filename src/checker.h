#define CHECKER_INFO_KINDS \
    FOR_EACH(Decl)      \
    FOR_EACH(Selector)  \

typedef enum CheckerInfoKind {
#define FOR_EACH(kind) CheckerInfoKind_##kind,
    CHECKER_INFO_KINDS
#undef FOR_EACH
} CheckerInfoKind;

struct CheckerInfo_Decl {
    Symbol *symbol;
};

struct CheckerInfo_Selector {
    u32 levelsOfIndirection;
    Val constant;
};

#define FOR_EACH(kind) typedef struct CheckerInfo_##kind CheckerInfo_##kind;
    CHECKER_INFO_KINDS
#undef  FOR_EACH

typedef struct CheckerInfo CheckerInfo;
struct CheckerInfo {
    CheckerInfoKind kind;
    union {
#define FOR_EACH(kind) CheckerInfo_##kind kind;
        CHECKER_INFO_KINDS
#undef FOR_EACH
    };
};
