
typedef enum Operator Operator;
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
        });


typedef enum AstKind AstKind;
enum AstKind {
    AK_Invalid,
#define AKind(kindName, ...) CONCAT(AK_, kindName),
    AST_KINDS;
#undef AKind;
};

const char *AstDescriptions[] = {
#define AKind(kindName, s, ...) "" s "",
    AST_KINDS;
#undef AKind;
};

const char *DescribeAstKind(AstKind ak) {
    return AstDescriptions[ak];
}

typedef struct Ast Ast;
struct Ast {
    AstKind kind;

    union {
#define AKind(kindName, s, definition) struct definition kindName;
        AST_KINDS;
#undef AKind;
    };
};

typedef struct SourceFile {
    Allocator allocator;
} SourceFile;

Ast *MakeNode(SourceFile *f, AstKind ak) {
    Ast *node = Alloc(f->allocator, sizeof(Ast));
    node->kind = ak;
    return node;
}

Ast *MakeIdent(SourceFile *f, Token token) {
    Ast *node = MakeNode(f, AK_Ident);
    node->Ident.token = token;
    return node;
}

Ast *MakeBadExpr(SourceFile *f, Token begin, Token end) {
    Ast *node = MakeNode(f, AK_BadExpr);
    node->BadExpr.begin = begin;
    node->BadExpr.end = end;
    return node;
}

Ast *MakeNilLiteral(SourceFile *f, Token token) {
    Ast *node = MakeNode(f, AK_NilLiteral);
    node->NilLiteral.token = token;
    return node;
}

Ast *MakeEllipsis(SourceFile *f, Ast *expr, Token token) {
    Ast *node = MakeNode(f, AK_Ellipsis);
    node->Ellipsis.expr = expr;
    node->Ellipsis.token = token;
    return node;
}

Ast *MakeBasicLit(SourceFile *f, Token token) {
    Ast *node = MakeNode(f, AK_BasicLit);
    node->BasicLit.token = token;
    return node;
}

Ast *MakeIdentList(SourceFile *f, Token token, DynamicArray(Ast *) idents) {
    Ast *node = MakeNode(f, AK_IdentList);
    node->IdentList.token = token;
    node->IdentList.idents = idents;
    return node;
}

Ast *MakeFuncLit(SourceFile *f, Token token, type, body: Ast *) {
    Ast *node = MakeNode(f, AK_FuncLit);
    node->FuncLit.token = token;
    node->FuncLit.type = type;
    node->FuncLit.body = body;
    return node;
}

Ast *MakeCompositeLit(SourceFile *f, Token begin, Token end, DynamicArray(Ast *) elements) {
    Ast *node = MakeNode(f, AK_CompositeLit);
    node->CompositeLit.begin = begin;
    node->CompositeLit.end = end;
    node->CompositeLit.elements = elements;
    return node;
}

Ast *MakeParen(SourceFile *f, Token begin, Token end, Ast *expr) {
    Ast *node = MakeNode(f, AK_Paren);
    node->Paren.begin = begin;
    node->Paren.end = end;
    node->Paren.expr = expr;
    return node;
}

Ast *MakeSelector(SourceFile *f, Token token, rec, sel: Ast *) {
    Ast *node = MakeNode(f, AK_Selector);
    node->Selector.token = token;
    node->Selector.rec = rec;
    node->Selector.sel = sel;
    return node;
}

Ast *MakeSubscript(SourceFile *f, Token begin, Token end, rec, index: Ast *) {
    Ast *node = MakeNode(f, AK_Subscript);
    node->Subscript.begin = begin;
    node->Subscript.end = end;
    node->Subscript.rec = rec;
    node->Subscript.index = index;
    return node;
}

Ast *MakeSlice(SourceFile *f, Token begin, Token end, expr, hi, lo: Ast *) {
    Ast *node = MakeNode(f, AK_Slice);
    node->Slice.begin = begin;
    node->Slice.end = end;
    node->Slice.expr = expr;
    node->Slice.hi = hi;
    node->Slice.lo = lo;
    return node;
}

Ast *MakeAutocast(SourceFile *f, Token token, Ast *expr) {
    Ast *node = MakeNode(f, AK_Autocast);
    node->Autocast.token = token;
    node->Autocast.expr = expr;
    return node;
}

Ast *MakeCast(SourceFile *f, kind: Token, Ast *type, Ast *expr) {
    Ast *node = MakeNode(f, AK_Cast);
    node->Cast.kind = kind;
    node->Cast.type = type;
    node->Cast.expr = expr;
    return node;
}

Ast *MakeCall(SourceFile *f, Token begin, Token end, labels, args: DynamicArray(Ast *)) {
    Ast *node = MakeNode(f, AK_Call);
    node->Call.begin = begin;
    node->Call.end = end;
    node->Call.labels = labels;
    node->Call.args = args;
    return node;
}

Ast *MakeUnary(SourceFile *f, Token op, element: Ast *) {
    Ast *node = MakeNode(f, AK_Unary);
    node->Unary.op = op;
    node->Unary.kind = tokenOperator(op);
    node->Unary.element = element;
    return node;
}

Ast *MakeBinary(SourceFile *f, Token op, Ast *lhs, Ast *rhs) {
    Ast *node = MakeNode(f, AK_Binary);
    node->Binary.op = op;
    node->Binary.kind = tokenOperator(op);
    node->Binary.lhs = lhs;
    node->Binary.rhs = rhs;
    return node;
}

Ast *MakeTernary(SourceFile *f, qmark, colon: Token, cond, then, els: Ast *) {
    Ast *node = MakeNode(f, AK_Ternary);
    node->Ternary.qmark = qmark;
    node->Ternary.colon = colon;
    node->Ternary.cond = cond;
    node->Ternary.then = then;
    node->Ternary.els = els;
    return node;
}

Ast *MakeKeyValue(SourceFile *f, Token token, key, value: Ast *) {
    Ast *node = MakeNode(f, AK_KeyValue);
    node->KeyValue.token = token;
    node->KeyValue.key = key;
    node->KeyValue.value = value;
    return node;
}

Ast *MakePointerType(SourceFile *f, Token token, type: Ast *) {
    Ast *node = MakeNode(f, AK_PointerType);
    node->PointerType.token = token;
    node->PointerType.type = type;
    return node;
}

Ast *MakeArrayType(SourceFile *f, Token begin, Token end, length, type: Ast *) {
    Ast *node = MakeNode(f, AK_ArrayType);
    node->ArrayType.begin = begin;
    node->ArrayType.end = end;
    node->ArrayType.length = length;
    node->ArrayType.type = type;
    return node;
}

Ast *MakeSliceType(SourceFile *f, Token begin, Token end, type: Ast *) {
    Ast *node = MakeNode(f, AK_SliceType);
    node->SliceType.begin = begin;
    node->SliceType.end = end;
    node->SliceType.type = type;
    return node;
}

Ast *MakeVectorType(SourceFile *f, Token begin, Token end, size, type: Ast *) {
    Ast *node = MakeNode(f, AK_VectorType);
    node->VectorType.begin = begin;
    node->VectorType.end = end;
    node->VectorType.size = size;
    node->VectorType.type = type;
    return node;
}

Ast *MakePolyType(SourceFile *f, Token token, type: Ast *) {
    Ast *node = MakeNode(f, AK_PolyType);
    node->PolyType.token = token;
    node->PolyType.type = type;
    return node;
}

Ast *MakeVariadicType(SourceFile *f, Token token, type: Ast *, isCVargs: bool) {
    Ast *node = MakeNode(f, AK_VariadicType);
    node->VariadicType.token = token;
    node->VariadicType.type = type;
    node->VariadicType.isCVargs = isCVargs;
    return node;
}

Ast *MakeBadStmt(SourceFile *f, Token begin, Token end) {
    Ast *node = MakeNode(f, AK_BadStmt);
    node->BadStmt.begin = begin;
    node->BadStmt.end = end;
    return node;
}

Ast *MakeEmpty(SourceFile *f, Token token) {
    Ast *node = MakeNode(f, AK_Empty);
    node->Empty = token;
    return node;
}

Ast *MakeLabel(SourceFile *f, Token token) {
    Ast *node = MakeNode(f, AK_Label);
    node->Label = token;
    return node;
}

Ast *MakeExprStmt(SourceFile *f, Token token, Ast *expr) {
    Ast *node = MakeNode(f, AK_ExprStmt);
    node->ExprStmt.token = token;
    node->ExprStmt.expr = expr;
    return node;
}

Ast *MakeAssign(SourceFile *f, Token token, lhs, rhs: DynamicArray(Ast *)) {
    Ast *node = MakeNode(f, AK_Assign);
    node->Assign.token = token;
    node->Assign.lhs = lhs;
    node->Assign.rhs = rhs;
    return node;
}

Ast *MakeReturn(SourceFile *f, Token token, stmts: DynamicArray(Ast *)) {
    Ast *node = MakeNode(f, AK_Return);
    node->Return.token = token;
    node->Return.stmts = stmts;
    return node;
}

Ast *MakeDefer(SourceFile *f, Token token, Ast *stmt) {
    Ast *node = MakeNode(f, AK_Defer);
    node->Defer.token = token;
    node->Defer.stmt = stmt;
    return node;
}

Ast *MakeUsing(SourceFile *f, Token token, Ast *expr) {
    Ast *node = MakeNode(f, AK_Using);
    node->Using.token = token;
    node->Using.expr = expr;
    return node;
}

Ast *MakeBranch(SourceFile *f, Token token, label: Ast *) {
    Ast *node = MakeNode(f, AK_Branch);
    node->Branch.token = token;
    node->Branch.label = label;
    return node;
}

Ast *MakeBlock(SourceFile *f, Token begin, Token end, stmts: DynamicArray(Ast *)) {
    Ast *node = MakeNode(f, AK_Block);
    node->Block.begin = begin;
    node->Block.end = end;
    node->Block.stmts = stmts;
    return node;
}

Ast *MakeIf(SourceFile *f, Token token, cond, body, els: Ast *) {
    Ast *node = MakeNode(f, AK_If);
    node->If.token = token;
    node->If.cond = cond;
    node->If.body = body;
    node->If.els = els;
    return node;
}

Ast *MakeCaseClause(SourceFile *f, Token token, match: DynamicArray(Ast *), block: Ast *) {
    Ast *node = MakeNode(f, AK_CaseClause);
    node->CaseClause.token = token;
    node->CaseClause.match = match;
    node->CaseClause.block = block;
    return node;
}

Ast *MakeSwitch(SourceFile *f, Token token, match: Ast *, cases: DynamicArray(Ast *)) {
    Ast *node = MakeNode(f, AK_Switch);
    node->Switch.token = token;
    node->Switch.match = match;
    node->Switch.cases = cases;
    return node;
}

Ast *MakeFor(SourceFile *f, Token token, init, cond, step, body: Ast *) {
    Ast *node = MakeNode(f, AK_For);
    node->For.token = token;
    node->For.init = init;
    node->For.cond = cond;
    node->For.step = step;
    node->For.body = body;
    return node;
}

Ast *MakeLibrary(SourceFile *f, Token token, path, alias: Ast *) {
    Ast *node = MakeNode(f, AK_Library);
    node->Library.token = token;
    node->Library.path = path;
    node->Library.alias = alias;
    return node;
}

Ast *MakeForeign(SourceFile *f, Token token, library, decl: Ast *, linkname, callconv: []u8) {
    Ast *node = MakeNode(f, AK_Foreign);
    node->Foreign.token = token;
    node->Foreign.library = library;
    node->Foreign.decl = decl;
    node->Foreign.linkname = linkname;
    node->Foreign.callconv = callconv;
    return node;
}

Ast *MakeDeclaration(SourceFile *f, Token token, names, values: DynamicArray(Ast *), type: Ast *, isConstant: bool) {
    Ast *node = MakeNode(f, AK_Declaration);
    node->Declaration.token = token;
    node->Declaration.names = names;
    node->Declaration.values = values;
    node->Declaration.type = type;
    node->Declaration.isConstant = isConstant;
    return node;
}

Ast *MakeBadDecl(SourceFile *f, Token begin, Token end) {
    Ast *node = MakeNode(f, AK_BadDecl);
    node->BadDecl.begin = begin;
    node->BadDecl.end = end;
    return node;
}
