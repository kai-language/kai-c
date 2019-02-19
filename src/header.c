typedef struct HeaderContext {
    String primitiveDecls;
    String complexDecls;
    String functions;
} HeaderContext;

#define HEAD_GENERATED ((void *)0xDEADBEEF)

void cgenType(String *buffer, const char * name, Type *type) {
    b32 appendName = name != NULL;

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

    case TypeKind_Struct: {
        appendName = false;
        ArrayPrintf(*buffer, "struct %s", name ? name : type->Symbol->name);
    } break;


    case TypeKind_Array: {
        appendName = false;

        String elementType = NULL;
        cgenType(&elementType, NULL, type->Array.elementType);
        ArrayPrintf(*buffer,"%s %s[%lu]", elementType, name, type->Array.length);
        ArrayFree(elementType);
    } break;

    default:
        if (type == VoidType) {
            ArrayPrintf(*buffer, "void");
        } else {
            ArrayPrintf(*buffer, "type");
        }
    }

    if (appendName) {
        ArrayPrintf(*buffer, " %s", name);
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

    ArrayPrintf(*buffer, "extern %s %s(%s);\n", returnType, name, params ? (char *)params : "void");

    ArrayFree(params);
    ArrayFree(returnType);
}

void cgenDecl(HeaderContext *ctx, DynamicArray(Expr_Ident *) names, Type *type, b32 isConst) {
    size_t numNames = ArrayLen(names);
    for (size_t i = 0; i < numNames; i++) {
        Expr_Ident *it = names[i];
        switch (type->kind) {
        case TypeKind_Function:
            cgenFuncPrototype(&ctx->functions, it->name, type);
            break;

        case TypeKind_Struct: {
            if (type->Symbol->backendUserdata != HEAD_GENERATED) {
                ArrayPrintf(ctx->primitiveDecls, "typedef ");
                cgenType(&ctx->primitiveDecls, it->name, type);
                ArrayPrintf(ctx->primitiveDecls, " %s;\n", it->name);
                type->Symbol->backendUserdata = HEAD_GENERATED;
            }

            ArrayPrintf(ctx->complexDecls, "struct %s {\n", it->name);
            for (u32 i = 0; i < type->Struct.numMembers; i++) {
                TypeField it = type->Struct.members[i];
                ArrayPrintf(ctx->complexDecls, "    ");
                cgenType(&ctx->complexDecls, it.name, it.type);
                ArrayPrintf(ctx->complexDecls, ";\n");
            }
            ArrayPrintf(ctx->complexDecls, "};\n");
        } break;

        default: {
            String typeStr = NULL;
            cgenType(&typeStr, it->name, type);
            const char *qualifiers = isConst ? "extern const" : "extern";
            ArrayPrintf(ctx->primitiveDecls, "%s %s;\n", qualifiers, typeStr);
        }
        }
    }
}

void cgenStmt(HeaderContext *ctx, CheckerInfo *info, Stmt *stmt) {
    switch (stmt->kind) {
        case StmtDeclKind_Constant: {
            Decl_Constant decl = stmt->Constant;
            cgenDecl(ctx, decl.names, info[stmt->id].Constant.symbol->type, true);
        } break;

        case StmtDeclKind_Variable: {
            Decl_Variable decl = stmt->Variable;
            cgenDecl(ctx, decl.names, info[stmt->id].Variable.symbols[0]->type, false);
        } break;

        default:
            break;
    }
}

const char *headerPreface = "#ifndef KAI_%s_H\n#define KAI_%s_H\n\n#ifndef KAI_TYPES\n#define KAI_TYPES\n#include <inttypes.h>\n\ntypedef int8_t  KaiI8;\ntypedef int16_t KaiI16;\ntypedef int32_t KaiI32;\ntypedef int64_t KaiI64;\n\ntypedef uint8_t  KaiU8;\ntypedef uint16_t KaiU16;\ntypedef uint32_t KaiU32;\ntypedef uint64_t KaiU64;\n\ntypedef float  KaiF32;\ntypedef double KaiF64;\n#endif\n";

void CodegenCHeader(Package *pkg) {
    char buff[MAX_PATH];
    char *dir;
    char *name = RemoveKaiExtension(GetFileName(pkg->path, buff, &dir));

    String preface = NULL;
    ArrayPrintf(preface, headerPreface, name, name);

    HeaderContext ctx = {
        NULL, NULL, NULL
    };

    size_t numStmts = ArrayLen(pkg->stmts);
    for (size_t i = 0; i < numStmts; i++) {
        cgenStmt(&ctx, pkg->checkerInfo, pkg->stmts[i]);
    }

    char outputPath[MAX_PATH];
    snprintf(&outputPath[0], sizeof(outputPath), "%s.h", name);

    FILE *file = fopen(outputPath, "wb");
    if (file) {
        fprintf(file, "%s\n", preface);

        if (ctx.primitiveDecls) {
            fprintf(file, "// MARK: Declarations\n");
            fprintf(file, "%s\n", ctx.primitiveDecls);
        }

        if (ctx.complexDecls) {
            fprintf(file, "// MARK: Types\n");
            fprintf(file, "%s\n", ctx.complexDecls);
        }

        if (ctx.functions) {
            fprintf(file, "// MARK: Functions\n");
            fprintf(file, "%s\n", ctx.functions);
        }

        fprintf(file, "#endif // KAI_%s_H\n", name);
        fclose(file);
    }

    ArrayFree(ctx.primitiveDecls);
    ArrayFree(ctx.complexDecls);
    ArrayFree(ctx.functions);
}
