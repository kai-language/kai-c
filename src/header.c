void cgenType(String *buffer, Type *type) {
    switch (type->kind) {
        case TypeKind_Int:
            ArrayPrintf(*buffer, "Kai%s%d", type->Flags & TypeFlag_Signed ? "I" : "U", type->Width);
            break;
        case TypeKind_Float:
            ArrayPrintf(*buffer, "KaiF%d", type->Width);
            break;
        default:
            ArrayPrintf(*buffer, "type");
    }
}

void cgenDecl(String *buffer, DynamicArray(Expr_Ident *) names, Type *type, b32 isConst) {
    String typeStr = NULL;
    cgenType(&typeStr, type);

    const char *qualifiers = isConst ? "extern const" : "extern";
    ForEach(names, Expr_Ident *) {
        ArrayPrintf(*buffer, "%s %s %s;\n", qualifiers, typeStr, it->name);
    }
}

void cgenStmt(String *buffer, CheckerInfo *info, Stmt *stmt) {
    switch (stmt->kind) {
        case StmtDeclKind_Constant: {
            Decl_Constant decl = stmt->Constant;
            cgenDecl(buffer, decl.names, info[stmt->id].Constant.symbol->type, true);
        } break;

        case StmtDeclKind_Variable: {
            Decl_Variable decl = stmt->Variable;
            cgenDecl(buffer, decl.names, info[stmt->id].Variable.symbols[0]->type, false);
        } break;

        default:
            break;
    }
}

const char *headerPreface = "#ifndef KAI_%s_H\n#define KAI_%s_H\n\n#ifndef KAI_TYPES\n#define KAI_TYPES\n#include <inttypes.h>\n\ntypedef int8_t  KaiI8;\ntypedef int16_t KaiI16;\ntypedef int32_t KaiI32;\ntypedef int64_t KaiI64;\n\ntypedef uint8_t  KaiU8;\ntypedef uint16_t KaiU16;\ntypedef uint32_t KaiU32;\ntypedef uint64_t KaiU64;\n\ntypedef float  KaiF32;\ntypedef double KaiF64;\n#endif\n\n";

void CodegenCHeader(Package *pkg) {
    String buffer = NULL;

    char buff[MAX_PATH];
    char *dir;
    char *name = KaiToObjectExtension(GetFileName(pkg->path, buff, &dir));
    name = "wip";

    ArrayPrintf(buffer, headerPreface, name, name);

    For(pkg->stmts) {
        cgenStmt(&buffer, pkg->checkerInfo, pkg->stmts[i]);
    }


    ArrayPrintf(buffer, "\n#endif // KAI_%s_H", name);
    printf("%s\n", buffer);
}
