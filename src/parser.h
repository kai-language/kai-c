


enum Operator {
    OP_Invalid,
    OP_Add,
    OP_Sub,
    OP_Mul,
    OP_Quo,
    OP_Rem,
    OP_And,
    OP_Or,
    OP_Xor,
    OP_Shl,
    OP_Shr,
};


#define AST_KINDS \
    AKind( \
        Ident, \
        "identifier", \
        { \
            Token token;  \
        }) \
    \
    AKind( \
        BadExpr, \
        "bad expression", \
        { \
            Token begin; \
            Token end; \
        }) \
    \
    AKind( \
        Comment, \
        "comment", \
        { \
            Token token; \
        })\
    \
    AKind( \
        StructField, \
        "struct field", \
        { \
        }) \
    \
    AKind( \
        EnumCase, \
        "enumeration case", \
        { \
        })\
    \
    AKind( \
        LocationDirective, \
        "location directive", \
        { \
            Token  token; \
            const char *ident; \
        })\
    \
    AKind( \
        NilLiteral, \
        "nil literal", \
        { \
            Token token; \
        })\
    \
    AKind( \
        Ellipsis, \
        "ellipsis", \
        { \
            Token token; \
            Ast   *expr; \
        })\
    \
    AKind( \
        BasicLit, \
        "basic literal", \
        { \
            Token token; \
        })\
    \
    AKind( \
        PolyParameterList, \
        "polymorphic parameter list", \
        {\
        })\
    \
    AKind( \
        ResultList, \
        "result list", \
        {\
        })\
    \
    AKind( \
        IdentList, \
        "identifier list", \
        { \
            Token              token; \
            DynamicArray(Ast*) idents; \
        })\
    \
    AKind( \
        FuncLit, \
        "function literal", \
        { \
            Token token; \
            Ast   *type; \
            Ast   *body; \
            u8    flags; \
        })\
    \
    AKind( \
        CompositeLit, \
        "composite literal", \
        { \
            Token              begin; \
            Token              end; \
            DynamicArray(Ast*) elements; \
        })\
    \
    AKind( \
        Paren, \
        "parenthesis", \
        { \
            Token begin; \
            Token end; \
            Ast   *expr; \
        })\
    \
    AKind( \
        Selector, \
        "selector", \
        { \
            Token token; \
            Ast   *rec; \
            Ast   *sel; \
        })\
    \
    AKind( \
        Subscript, \
        "subscript", \
        { \
            Token begin; \
            Token end; \
            Ast   *rec; \
            Ast   *index; \
        })\
    \
    AKind( \
        Slice, \
        "slice", \
        { \
            Token begin; \
            Token end; \
            Ast   *expr; \
            Ast   *lo; \
            Ast   *hi; \
        })\
    \
    AKind( \
        Autocast, \
        "autocast", \
        { \
            Token token; \
            Ast   *expr; \
        })\
    \
    AKind( \
        Cast, \
        "cast", \
        { \
            Token kind; \
            Ast   *type; \
            Ast   *expr; \
        })\
    \
    AKind( \
        Call, \
        "call", \
        { \
            Token       begin; \
            Token       end; \
            DynamicArray(Ast*) labels; \
            DynamicArray(Ast*) args; \
        })\
    \
    AKind( \
        Unary, \
        "unary", \
        { \
            Token    op; \
            Operator kind; \
            Ast      *element; \
        })\
    \
    AKind( \
        Binary, \
        "binary", \
        { \
            Token    op; \
            Operator kind; \
            Ast      *lhs; \
            Ast      *rhs; \
        })\
    \
    AKind( \
        Ternary, \
        "ternary", \
        { \
            Token qmark; \
            Token colon; \
            Ast   *cond; \
            Ast   *then; \
            Ast   *els; \
        })\
    \
    AKind( \
        KeyValue, \
        "key value", \
        { \
            Token token; \
            Ast   *key; \
            Ast   *value; \
        })\
    \
    AKind( \
        PointerType, \
        "pointer type", \
        { \
            Token token; \
            Ast   *type; \
        })\
    \
    AKind( \
        ArrayType, \
        "array type", \
        { \
            Token begin; \
            Token end; \
            Ast   *length; \
            Ast   *type; \
        })\
    \
    AKind( \
        SliceType, \
        "slice type", \
        { \
            Token begin; \
            Token end; \
            Ast   *type; \
        })\
    \
    AKind( \
        VectorType, \
        "vector type", \
        { \
            Token begin; \
            Token end; \
            Ast   *size; \
            Ast   *type; \
        })\
    \
    AKind( \
        StructType, \
        "struct type", \
        { \
        })\
    \
    AKind( \
        PolyStructType, \
        "poly struct type", \
        { \
        })\
    \
    AKind( \
        EnumType, \
        "enum type", \
        { \
        })\
    \
    AKind( \
        UnionType, \
        "union type", \
        { \
        })\
    \
    AKind( \
        VariantType, \
        "variant type", \
        { \
        })\
    \
    AKind( \
        PolyType, \
        "polymorphic type", \
        { \
            Token token; \
            Ast   *type; \
        })\
    \
    AKind( \
        VariadicType, \
        "variadic type", \
        { \
            Token token; \
            Ast   *type; \
            b8    *isCVargs; \
        })\
    \
    AKind( \
        FuncType, \
        "function type", \
        { \
            Token              begin; \
            Token              end; \
            Ast                *result; \
            DynamicArray(Ast*) params; \
        })\
    \
    AKind( \
        BadStmt, \
        "bad statement", \
        { \
            Token begin; \
            Token end; \
        })\
    \
    AKind( \
        Empty, \
        "empty", \
        { \
            Token token; \
        })\
    \
    AKind( \
        Label, \
        "label", \
        { \
        })\
    \
    AKind( \
        ExprStmt, \
        "expression statement", \
        { \
            Token token; \
            Ast   *expr; \
        })\
    \
    AKind( \
        Assign, \
        "assignment", \
        { \
            Token              token; \
            DynamicArray(Ast*) lhs; \
            DynamicArray(Ast*) rhs; \
        })\
    \
    AKind( \
        Return, \
        "return", \
        { \
            Token              token; \
            DynamicArray(Ast*) stmts; \
        })\
    \
    AKind( \
        Defer, \
        "deference", \
        { \
            Token token; \
            Ast   *stmt; \
        })\
    \
    AKind( \
        Using, \
        "using", \
        { \
        })\
    \
    AKind( \
        Branch, \
        "branch", \
        { \
            Token token; \
            Ast   *label; \
        })\
    \
    AKind( \
        Block, \
        "block", \
        {\
            Token              begin; \
            Token              end; \
            DynamicArray(Ast*) stmts; \
        })\
    \
    AKind( \
        If, \
        "if", \
        { \
            Token token; \
            Ast   *cond; \
            Ast   *body; \
            Ast   *els; \
        })\
    \
    AKind( \
        CaseClause, \
        "case clause", \
        { \
            Token              token; \
            DynamicArray(Ast*) match; \
            Ast                *block;  /* TODO(Jonas, Brett): entity/label support */ \
        })\
    \
    AKind( \
        Switch, \
        "switch", \
        { \
            Token              token; \
            Ast                *match; \
            DynamicArray(Ast*) cases; /* TODO(Jonas, Brett): entity/label support */ \
        })\
    \
    AKind( \
        For, \
        "for", \
        { \
            Token token; \
            Ast   *init; \
            Ast   *cond; \
            Ast   *step; \
            Ast   *body; \
        })\
    \
    AKind( \
        ForIn, \
        "for in", \
        { \
        })\
    \
    AKind( \
        Import, \
        "import", \
        { \
        })\
    \
    AKind( \
        Library, \
        "library", \
        { \
            Token token; \
            Ast   *path; \
            Ast   *aliases; \
        })\
    \
    AKind( \
        Foreign, \
        "foreign", \
        { \
            Token  token; \
            Ast    *library; \
            Ast    *decl; \
            const char *linkname; \
            const char *callconv; \
        })\
    \
    AKind( \
        DeclBlock, \
        "declaration block", \
        { \
        })\
    \
    AKind( \
        Declaration, \
        "declaration", \
        { \
            Token              token; \
            DynamicArray(Ast*) names; \
            DynamicArray(Ast*) values; \
            Ast                *type; \
            b8                 isConstant; \
        })\
    \
    AKind( \
        BadDecl, \
        "bad declaration", \
        { \
            Token begin; \
            Token end; \
        })\
    \
    AKind( \
        FunctionSpecialization, \
        "function specialization", \
        { \
        })\
    \
    AKind( \
        StructSpecialization, \
        "struct specialization", \
        { \
        })


enum AstKind {
    AK_Invalid,
#define AKind(kindName, ...) CONCAT(AK_, kindName),
    AST_KINDS
#undef AKind
};

const char *AstDescriptions[] = {
#define AKind(kindName, s, ...) "" s "",
    AST_KINDS
#undef AKind
};

const char *DescribeAstKind(AstKind ak) {
    return AstDescriptions[ak];
}

struct Ast {
    AstKind kind;

    union {
#define AKind(kindName, s, definition) struct definition kindName;
        AST_KINDS
#undef AKind
    };
};



