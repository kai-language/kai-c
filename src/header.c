typedef enum HeaderPass HeaderPass;
enum HeaderPass {
    HeaderPass_Primitives,
    HeaderPass_Incomplete,
    HeaderPass_Final,

    _HEADER_PASS_COUNT
};

void cgenType(String *buffer, const char * name, Type *type) {
    switch (type->kind) {
        case TypeKind_Int:
            ArrayPrintf(*buffer, "Kai%s%d", type->Flags & TypeFlag_Signed ? "I" : "U", type->Width);
            break;

        case TypeKind_Float:
            ArrayPrintf(*buffer, "KaiF%d", type->Width);
            break;

        case TypeKind_Pointer: {
            String pointee = NULL;
            cgenType(&pointee, NULL, type->Pointer.pointeeType);
            ArrayPrintf(*buffer, "%s*", pointee);
            ArrayFree(pointee);
        } break;

        case TypeKind_Array: {
            String elementType = NULL;
            cgenType(&elementType, NULL, type->Array.elementType);
            ArrayPrintf(*buffer,"%s %s[%lu]", elementType, name, type->Array.length);
            ArrayFree(elementType);
        } break;

        case TypeKind_Struct:
            ArrayPrintf(*buffer, "struct %s", type->Symbol->name);
            break;

        default:
            if (type == VoidType) {
                ArrayPrintf(*buffer, "void");
            } else {
                ArrayPrintf(*buffer, "type");
            }
    }
}

void cgenFuncPrototype(String *buffer, const char *name, Type *type) {
    Type_Function func = type->Function;

    String returnType = NULL;
    cgenType(&returnType, NULL, func.results[0]);

    String params = NULL;
    size_t paramCount = ArrayLen(func.params);
    for (size_t i = 0; i < paramCount; i += 1) {
        cgenType(&params, NULL, func.params[i]);

        if (i+1 < paramCount) {
            ArrayPrintf(params, ", ");
        }
    }

    ArrayPrintf(*buffer, "extern %s %s(%s);\n", returnType, name, params);

    ArrayFree(params);
    ArrayFree(returnType);
}

void cgenDecl(String *buffer, DynamicArray(Expr_Ident *) names, Type *type, b32 isConst) {
    ForEach(names, Expr_Ident *) {
        switch (type->kind) {
        case TypeKind_Function:
            cgenFuncPrototype(buffer, it->name, type);
            break;

        case TypeKind_Struct: {
            ArrayPrintf(*buffer, "typedef struct %s {\n", it->name);
            ForEach(type->Struct.members, TypeField *) {
                ArrayPrintf(*buffer, "    ");
                cgenType(buffer, it->name, it->type);
                ArrayPrintf(*buffer, " %s;\n", it->name);
            }
            ArrayPrintf(*buffer, "} %s;\n", it->name);
        } break;

        default: {
            String typeStr = NULL;
            cgenType(&typeStr, it->name, type);
            const char *qualifiers = isConst ? "extern const" : "extern";
            if (type->kind != TypeKind_Array) {
                ArrayPrintf(*buffer, "%s %s %s;\n", qualifiers, typeStr, it->name);
            } else {
                ArrayPrintf(*buffer, "%s %s;\n", qualifiers, typeStr);
            }
        }
        }
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
    char *name = RemoveKaiExtension(GetFileName(pkg->path, buff, &dir));

    ArrayPrintf(buffer, headerPreface, name, name);

    For(pkg->stmts) {
        cgenStmt(&buffer, pkg->checkerInfo, pkg->stmts[i]);
    }


    ArrayPrintf(buffer, "\n#endif // KAI_%s_H", name);
    printf("%s\n", buffer);
}
