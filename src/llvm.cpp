#include "common.h"
#include "symbols.h"
#include "compiler.h"
#include "flags.h"
#include "lexer.h"
#include "ast.h"
#include "types.h"
#include "checker.h"
#include "llvm.h"

// Save our definition of DEBUG and undefine it to avoid conflict with LLVM's
// definition
#ifdef DEBUG
#define KAI_DEBUG DEBUG
#undef DEBUG
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#pragma clang diagnostic ignored "-Wcomma"

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/DIBuilder.h>

#include "llvm/IR/LegacyPassManager.h"

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>


#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#pragma clang diagnostic pop

// Undefine LLVM's definition of DEBUG and restore our definition (if any)
#undef DEBUG
#ifdef KAI_DEBUG
#define DEBUG KAI_DEBUG
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wswitch"

typedef struct DebugTypes {
    llvm::DIType *i8;
    llvm::DIType *i16;
    llvm::DIType *i32;
    llvm::DIType *i64;

    llvm::DIType *u8;
    llvm::DIType *u16;
    llvm::DIType *u32;
    llvm::DIType *u64;

    llvm::DIType *f32;
    llvm::DIType *f64;
} DebugTypes;

typedef struct Debug {
    llvm::DIBuilder *builder;
    llvm::DICompileUnit *unit;
    llvm::DIFile *file;
    DebugTypes types;
    llvm::DIScope *scope;
} Debug;

typedef struct Context Context;
struct Context {
    DynamicArray(CheckerInfo) checkerInfo;
    llvm::Module *m;
    llvm::TargetMachine *targetMachine;
    llvm::DataLayout dataLayout;
    llvm::IRBuilder<> b;

    llvm::Function *fn;

    llvm::BasicBlock *retBlock;
    llvm::Value *retValue;
    bool returnAddress;

    Debug d;
    Arena arena;

    std::vector<llvm::BasicBlock *> deferStack;
};

typedef struct BackendStructUserdata BackendStructUserdata;
struct BackendStructUserdata {
    llvm::StructType *type;
    llvm::DIType *debugType;
};

void clearDebugPos(Context *ctx);
void debugPos(Context *ctx, Position pos);
b32 emitObjectFile(Package *p, char *name, Context *ctx);
llvm::Value *emitExpr(Context *ctx, Expr *expr, llvm::Type *desiredType = nullptr);
llvm::Value *emitExprBinary(Context *ctx, Expr *expr);
llvm::Value *emitExprUnary(Context *ctx, Expr *expr);
llvm::Value *emitExprSubscript(Context *ctx, Expr *expr);
void emitStmt(Context *ctx, Stmt *stmt);

llvm::Type *canonicalize(Context *ctx, Type *type) {
    switch (type->kind) {
        case TypeKind_Tuple:
            // TODO: Should they?
            PANIC("Tuples are a checker thing, they should not make their way into the backend, unless they do?");
            if (!type->Tuple.numTypes) {
                return llvm::Type::getVoidTy(ctx->m->getContext());
            }
            UNIMPLEMENTED(); // Multiple returns
        case TypeKind_Int:
            return llvm::IntegerType::get(ctx->m->getContext(), type->Width);

        case TypeKind_Float: {
            if (type->Width == 32) {
                return llvm::Type::getFloatTy(ctx->m->getContext());
            } else if (type->Width == 64) {
                return llvm::Type::getDoubleTy(ctx->m->getContext());
            } else {
                ASSERT(false);
            }
        } break;

        case TypeKind_Array: {
            return llvm::ArrayType::get(canonicalize(ctx, type->Array.elementType), type->Array.length);
        } break;

        case TypeKind_Pointer: {
            return llvm::PointerType::get(canonicalize(ctx, type->Pointer.pointeeType), 0);
        } break;

        case TypeKind_Function: {
            std::vector<llvm::Type *> params;
            for (u32 i = 0; i < type->Function.numParams; i++) {
                llvm::Type *paramType = canonicalize(ctx, type->Function.params[i]);
                params.push_back(paramType);
            }

            llvm::Type *returnType;
            if (type->Function.numResults == 0) {
                returnType = llvm::Type::getVoidTy(ctx->m->getContext());
            } else if (type->Function.numResults == 1) {
                returnType = canonicalize(ctx, type->Function.results[0]);
            } else {
                std::vector<llvm::Type *> elements;
                for (u32 i = 0; i < type->Function.numResults; i++) {
                    llvm::Type *ty = canonicalize(ctx, type->Function.results[i]);
                    elements.push_back(ty);
                }

                returnType = llvm::StructType::create(ctx->m->getContext(), elements);
            }

            // TODO: Kai vargs
            return llvm::FunctionType::get(returnType, params, (type->Function.Flags & TypeFlag_CVargs) != 0);
        } break;

        case TypeKind_Struct: {
            if (type->Symbol && type->Symbol->backendUserdata) {
                BackendStructUserdata *userdata = (BackendStructUserdata *) type->Symbol->backendUserdata;
                return userdata->type;
            } else if (type->Symbol->decl) {
                // TODO: When we support importing symbols into other scopes we will need to do a context switch
                emitStmt(ctx, (Stmt *) type->Symbol->decl);
                BackendStructUserdata *userdata = (BackendStructUserdata *) type->Symbol->backendUserdata;
                return userdata->type;
            }

            ASSERT_MSG(!type->Symbol, "Only unnamed structures should get to here");
            std::vector<llvm::Type *> elements;
            for (u32 i = 0; i < type->Struct.numMembers; i++) {
                llvm::Type *ty = canonicalize(ctx, type->Struct.members[i].type);
                elements.push_back(ty);
            }

            llvm::StructType *ty = llvm::StructType::create(ctx->m->getContext(), elements);
            if (type->Symbol) {
                ty->setName(type->Symbol->name);

                // NOTE: emitExprTypeStruct will update this with debug info later if needed.

                BackendStructUserdata *userdata = (BackendStructUserdata *) ArenaAlloc(&ctx->arena, sizeof(BackendStructUserdata));
                userdata->type = ty;
                userdata->debugType = NULL;
                type->Symbol->backendUserdata = userdata;
            }

            return ty;
        } break;
    }

    ASSERT_MSG_VA(false, "Unable to canonicalize type %s", DescribeType(type));
    return NULL;
}

llvm::DIType *debugCanonicalize(Context *ctx, Type *type) {
    DebugTypes types = ctx->d.types;

    if (type->kind == TypeKind_Int) {
        switch (type->Width) {
        case 8:  return type->Flags & TypeFlag_Signed ? types.i8  : types.u8;
        case 16: return type->Flags & TypeFlag_Signed ? types.i16 : types.u16;
        case 32: return type->Flags & TypeFlag_Signed ? types.i32 : types.u32;
        case 64: return type->Flags & TypeFlag_Signed ? types.i64 : types.u64;
        }
    }

    if (type->kind == TypeKind_Float) {
        return type->Width == 32 ? types.f32 : types.f64;
    }

    if (type->kind == TypeKind_Tuple && !type->Tuple.numTypes) {
        return NULL;
    }

    if (type->kind == TypeKind_Pointer) {
        return ctx->d.builder->createPointerType(
            debugCanonicalize(ctx, type->Pointer.pointeeType),
            ctx->m->getDataLayout().getPointerSize()
        );
    }

    if (type->kind == TypeKind_Array) {
        std::vector<llvm::Metadata *> subscripts;
        Type *elementType = type;
        while (elementType->kind == TypeKind_Array) {
            subscripts.push_back(ctx->d.builder->getOrCreateSubrange(0, type->Array.length));
            elementType = elementType->Array.elementType;
        }

        llvm::DINodeArray subscriptsArray = ctx->d.builder->getOrCreateArray(subscripts);
        return ctx->d.builder->createArrayType(type->Width, type->Align, debugCanonicalize(ctx, elementType), subscriptsArray);
    }

    if (type->kind == TypeKind_Function) {
        // NOTE: Clang just uses a derived type that is a pointer
        // !44 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !9, size: 64)
        std::vector<llvm::Metadata *> parameterTypes;
        for (u32 i = 0; i < type->Function.numParams; i++) {
            llvm::DIType *ty = debugCanonicalize(ctx, type->Function.params[i]);
            parameterTypes.push_back(ty);
        }

        auto pTypes = ctx->d.builder->getOrCreateTypeArray(parameterTypes);
        return ctx->d.builder->createSubroutineType(pTypes);
    }

    if (type->kind == TypeKind_Struct) {
        if (type->Symbol && type->Symbol->backendUserdata) {
            BackendStructUserdata *userdata = (BackendStructUserdata *) type->Symbol->backendUserdata;
            if (userdata->debugType) return userdata->debugType;
        }

        std::vector<llvm::Metadata *> elementTypes;
        for (u32 i = 0; i < type->Struct.numMembers; i++) {
            TypeField it = type->Struct.members[i];
            llvm::DIType *dtype = debugCanonicalize(ctx, it.type);

            auto member = ctx->d.builder->createMemberType(
                ctx->d.scope,
                it.name,
                ctx->d.file,
                0, // TODO: LineNo for struct members
                it.type->Width,
                it.type->Align,
                it.offset,
                llvm::DINode::DIFlags::FlagZero,
                dtype
            );

            elementTypes.push_back(member);
        }

        auto elements = ctx->d.builder->getOrCreateArray(elementTypes);
        return ctx->d.builder->createStructType(
            ctx->d.scope,
            type->Symbol->name,
            ctx->d.file,
            type->Symbol->decl->start.line,
            type->Width,
            type->Align,
            llvm::DINode::DIFlags::FlagZero,
            NULL, // DerivedFrom
            elements
        );
    }

    ASSERT(false);
    return NULL;
}

void initDebugTypes(llvm::DIBuilder *b, DebugTypes *types) {
    using namespace llvm::dwarf;

    types->i8  = b->createBasicType("i8",   8, DW_ATE_signed_char);
    types->i16 = b->createBasicType("i16", 16, DW_ATE_signed);
    types->i32 = b->createBasicType("i32", 32, DW_ATE_signed);
    types->i64 = b->createBasicType("i64", 64, DW_ATE_signed);

    types->u8  = b->createBasicType("u8",   8, DW_ATE_unsigned_char);
    types->u16 = b->createBasicType("u16", 16, DW_ATE_unsigned);
    types->u32 = b->createBasicType("u32", 32, DW_ATE_unsigned);
    types->u64 = b->createBasicType("u64", 64, DW_ATE_unsigned);

    types->f32 = b->createBasicType("f32", 32, DW_ATE_float);
    types->f64 = b->createBasicType("f64", 64, DW_ATE_float);
}

llvm::DIFile *setDebugInfoForPackage(llvm::DIBuilder *b, Package *import) {
    if (FlagDebug && !import->backendUserdata) {
        char directory[MAX_PATH];
        strcpy(directory, import->fullpath);
        char *filename = strrchr(directory, '/');
        *(filename++) = '\0';

        llvm::DIFile *file = b->createFile(filename, directory);
        import->backendUserdata = file;
        return file;
    }
    return NULL;
}

void setCallingConvention(llvm::Function *fn, const char *name) {
    if (name == internCallConv_C) {
        fn->setCallingConv(llvm::CallingConv::C);
    }
}

llvm::AllocaInst *createEntryBlockAlloca(Context *ctx, Type *type, const char *name) {
    llvm::IRBuilder<> b(&ctx->fn->getEntryBlock(), ctx->fn->getEntryBlock().begin());
    llvm::AllocaInst *alloca = b.CreateAlloca(canonicalize(ctx, type), 0, name);
    alloca->setAlignment(BytesFromBits(type->Align));
    return alloca;
}

llvm::AllocaInst *createEntryBlockAlloca(Context *ctx, Symbol *sym) {
    auto type = canonicalize(ctx, sym->type);
    llvm::IRBuilder<> b(&ctx->fn->getEntryBlock(), ctx->fn->getEntryBlock().begin());
    auto alloca = b.CreateAlloca(type, 0, sym->name);
    alloca->setAlignment(BytesFromBits(sym->type->Align));
    return alloca;
}

llvm::AllocaInst *createEntryBlockAlloca(Context *ctx, llvm::Type *type, const char *name) {
    llvm::IRBuilder<> b(&ctx->fn->getEntryBlock(), ctx->fn->getEntryBlock().begin());
    llvm::AllocaInst *alloca = b.CreateAlloca(type, 0, name);
    return alloca;
}

llvm::Value *createVariable(Symbol *symbol, Position pos, Context *ctx) {
    ASSERT(symbol->kind == SymbolKind_Variable);

    if (symbol->flags & SymbolFlag_Global) {
        llvm::GlobalVariable *global = new llvm::GlobalVariable(
            *ctx->m,
            canonicalize(ctx, symbol->type),
            false, // IsConstant
            llvm::GlobalValue::ExternalLinkage,
            NULL,  // Initializer
            symbol->name
        );
        global->setAlignment(BytesFromBits(symbol->type->Align));
        symbol->backendUserdata = global;

        if (FlagDebug) {
            ctx->d.builder->createGlobalVariableExpression(
                ctx->d.scope,
                symbol->name, // Name
                symbol->name, // LinkageName
                ctx->d.file,
                pos.line,
                debugCanonicalize(ctx, symbol->type),
                false,        // isLocalToUnit
                nullptr,      // Expr
                nullptr,      // Decl
                symbol->type->Align
            );
        }
        return global;
    } else {
        llvm::AllocaInst *alloca = createEntryBlockAlloca(ctx, symbol);
        symbol->backendUserdata = alloca;

        if (FlagDebug) {
            auto d = ctx->d.builder->createAutoVariable(
                ctx->d.scope,
                symbol->name,
                ctx->d.file,
                pos.line,
                debugCanonicalize(ctx, symbol->type)
            );

            ctx->d.builder->insertDeclare(
                alloca,
                d,
                ctx->d.builder->createExpression(),
                llvm::DebugLoc::get(pos.line, pos.column, ctx->d.scope),
                ctx->b.GetInsertBlock()
            );
        }
        return alloca;
    }
}

void setVariableInitializer(Symbol *symbol, Context *ctx, llvm::Value *value) {

    if (symbol->flags & SymbolFlag_Global) {
        ASSERT(llvm::isa<llvm::GlobalVariable>((llvm::Value *) symbol->backendUserdata));
        llvm::GlobalVariable *global = (llvm::GlobalVariable *) symbol->backendUserdata;

        if (llvm::Constant *constant = llvm::dyn_cast<llvm::Constant>(value)) {
            global->setInitializer(constant);
        } else if (llvm::LoadInst *inst = llvm::dyn_cast<llvm::LoadInst>(value)) {

            // This handles a file scope variables refering to one another where the initializer is a constant

            // FIXME: Is there some cleaner way to achieve this? This is probably one of the worst ways to do this.
            //  It could be much better to have in the context a flag that says, *if* a global is encountered then
            //  return *that* do not load it. Some sort of returnAddressIfGlobal ... also gross.
            // -vdka September 2018

            llvm::Value *loaded = inst->getPointerOperand();
            if (llvm::GlobalVariable *other = llvm::dyn_cast<llvm::GlobalVariable>(loaded)) {

                inst->removeFromParent();
                inst->deleteValue();

                global->setInitializer(other->getInitializer());
            } else {
                PANIC("Attempt to set global variable from non global variable. Should not pass checking!");
            }

        } else {
            global->setExternallyInitialized(true);
            // TODO: Add initializer to some sort of premain?
            // -vdka September 2018
            UNIMPLEMENTED();
        }
    } else {
        ctx->b.CreateAlignedStore(value, (llvm::Value *) symbol->backendUserdata, BytesFromBits(symbol->type->Align));
    }
}

llvm::Value *coerceValue(Context *ctx, Conversion conversion, llvm::Value *value, llvm::Type *target) {
    ASSERT(target);
    ASSERT(value);
    switch (conversion & ConversionKind_Mask) {
        case ConversionKind_None:
            return value;

        case ConversionKind_Same: {
            bool extend = conversion & ConversionFlag_Extend;
            if (conversion & ConversionFlag_Float) {
                return extend ? ctx->b.CreateFPExt(value, target) : ctx->b.CreateFPTrunc(value, target);
            }
            if (extend) {
                bool isSigned = conversion & ConversionFlag_Signed;
                return isSigned ? ctx->b.CreateSExt(value, target) : ctx->b.CreateZExt(value, target);
            } else {
                return ctx->b.CreateTrunc(value, target);
            }
        }

        case ConversionKind_FtoI:
            if (conversion & ConversionFlag_Extend) {
                return ctx->b.CreateFPExt(value, target);
            } else {
                return ctx->b.CreateFPTrunc(value, target);
            }

        case ConversionKind_ItoF:
            if (conversion & ConversionFlag_Extend) {
                bool isSigned = conversion & ConversionFlag_Signed;
                return isSigned ? ctx->b.CreateSExt(value, target) : ctx->b.CreateZExt(value, target);
            } else {
                return ctx->b.CreateTrunc(value, target);
            }

        case ConversionKind_PtoI:
            return ctx->b.CreatePtrToInt(value, target);

        case ConversionKind_ItoP:
            return ctx->b.CreateIntToPtr(value, target);

        case ConversionKind_Bool:
            if (conversion & ConversionFlag_Float) {
                return ctx->b.CreateFCmpONE(value, llvm::ConstantFP::get(value->getType(), 0));
            } else if (value->getType()->isPointerTy()) {
                auto intptr = ctx->m->getDataLayout().getIntPtrType(ctx->m->getContext());
                value = ctx->b.CreatePtrToInt(value, intptr);
                return ctx->b.CreateICmpNE(value, llvm::ConstantInt::get(intptr, 0));
            } else {
                ASSERT(value->getType()->isIntegerTy());
                return ctx->b.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0));
            }

        case ConversionKind_Any:
            // TODO: Implement conversion to Any type
            UNIMPLEMENTED();
            return NULL;

        case ConversionKind_Tuple:
            return value;

        default:
            UNIMPLEMENTED();
            return value;
    }
}

llvm::Value *emitExprIdent(Context *ctx, Expr *expr) {
    CheckerInfo info = ctx->checkerInfo[expr->id];
    Symbol *symbol = info.Ident.symbol;
    if (!symbol->backendUserdata) {

        // TODO: Switch to the symbol's package (only relevant when a symbol can be imported) into another scope
        emitStmt(ctx, (Stmt *) symbol->decl);
        ASSERT(symbol->backendUserdata);
    }

    llvm::Value *value = (llvm::Value *) symbol->backendUserdata;
    if (symbol->kind == SymbolKind_Variable && !ctx->returnAddress) {
        value = ctx->b.CreateAlignedLoad(value, BytesFromBits(symbol->type->Align));
    }
    return value;
}

llvm::Value *emitExprCall(Context *ctx, Expr *expr) {
    ASSERT(expr->kind == ExprKind_Call);
    auto fnType = TypeFromCheckerInfo(ctx->checkerInfo[expr->Call.expr->id]);

    std::vector<llvm::Value *> args;
    ForEachWithIndex(expr->Call.args, i, Expr_KeyValue *, arg) {
        auto irArgType = canonicalize(ctx, fnType->Function.params[i]);
        auto irArg = emitExpr(ctx, expr->Call.args[i]->value, irArgType);
        args.push_back(irArg);
    }
    auto irFunc = emitExpr(ctx, expr->Call.expr);
    debugPos(ctx, expr->Call.start);
    return ctx->b.CreateCall(irFunc, args);
}

llvm::Value *emitExprLitCompound(Context *ctx, Expr *expr) {
    ASSERT(expr->kind == ExprKind_LitCompound);
    CheckerInfo_BasicExpr info = ctx->checkerInfo[expr->id].BasicExpr;

    switch (info.type->kind) {
        case TypeKind_Struct: {
            llvm::StructType *type = (llvm::StructType *) canonicalize(ctx, info.type);

            // FIXME: Determine if, like C, all uninitialized Struct members are zero'd or left undefined
            llvm::Value *agg = llvm::UndefValue::get(type);
            ForEach(expr->LitCompound.elements, Expr_KeyValue *) {
                TypeField *field = (TypeField *) it->info;
                u64 index = (field - &info.type->Struct.members[0]);
                llvm::Value *val = emitExpr(ctx, it->value);

                agg = ctx->b.CreateInsertValue(agg, val, (u32) index);
            }

            if (ctx->returnAddress) UNIMPLEMENTED();
            return agg;
        };

        case TypeKind_Array: {
            llvm::Type *elementType = canonicalize(ctx, info.type->Array.elementType);
            llvm::Type *irType = canonicalize(ctx, info.type);

            llvm::Value *value;
            if (!expr->LitCompound.elements) {
                value = llvm::Constant::getNullValue(irType);
            } else {
                // FIXME: Determine if, like C all uninitialized Array members are zero'd or left undefined
                value = llvm::UndefValue::get(irType);
                ForEach(expr->LitCompound.elements, Expr_KeyValue *) {
                    // For both Array and Slice type Compound Literals the info on KeyValue is the constant index
                    u64 index = (u64) it->info;
                    llvm::Value *el = emitExpr(ctx, it->value, elementType);
                    value = ctx->b.CreateInsertValue(value, el, (u32) index);
                }
            }

            if (ctx->returnAddress) UNIMPLEMENTED();
            return value;
        }

        case TypeKind_Union: case TypeKind_Enum: case TypeKind_Slice:
            UNIMPLEMENTED();
            break;
    }

    ASSERT(false);
    return NULL;
}

llvm::Value *emitExprSelector(Context *ctx, Expr *expr) {
    ASSERT(expr->kind == ExprKind_Selector);
    CheckerInfo_Selector info = ctx->checkerInfo[expr->id].Selector;

    switch (info.kind) {
        case SelectorKind_None: {
            UNIMPLEMENTED();
            break;
        }

        case SelectorKind_Struct: {
            bool previousReturnAddress = ctx->returnAddress;
            ctx->returnAddress = true;
            llvm::Value *value = emitExpr(ctx, expr->Selector.expr);
            ctx->returnAddress = previousReturnAddress;

            llvm::Type *indexType = llvm::IntegerType::get(ctx->m->getContext(), 32);
            llvm::Value *idxs[2] = {
                llvm::ConstantInt::get(indexType, 0),
                llvm::ConstantInt::get(indexType, info.value.Struct.index),
            };

            llvm::ArrayRef<llvm::Value *> idxList = llvm::ArrayRef<llvm::Value *>(idxs, 2);
            llvm::Value *addr = ctx->b.CreateGEP(value, idxList);

            if (ctx->returnAddress) return addr;
            return ctx->b.CreateAlignedLoad(addr, BytesFromBits(info.value.Struct.index));
        }

        case SelectorKind_Import: {
            Symbol *symbol = info.value.Import.symbol;
            if (!symbol->backendUserdata) {
                CheckerInfo *prevCheckerInfo = ctx->checkerInfo;
                ctx->checkerInfo = info.value.Import.package->checkerInfo;

                llvm::DIFile *file = (llvm::DIFile *) info.value.Import.package->backendUserdata;
                Debug debug = {ctx->d.builder, ctx->d.unit, file, ctx->d.types, file};

                llvm::IRBuilder<> b(ctx->m->getContext());
                Context declContext = {
                    .checkerInfo = info.value.Import.package->checkerInfo,
                    .m = ctx->m,
                    .targetMachine = ctx->targetMachine,
                    .dataLayout = ctx->dataLayout,
                    .b = b,
                    .fn = nullptr,
                    .d = debug,
                };

                emitStmt(&declContext, (Stmt *) symbol->decl);
                ASSERT(symbol->backendUserdata);

                ctx->checkerInfo = prevCheckerInfo;
            }

            llvm::Value *value = (llvm::Value *) symbol->backendUserdata;
            if (symbol->kind == SymbolKind_Variable && !ctx->returnAddress) {
                value = ctx->b.CreateAlignedLoad(value, BytesFromBits(symbol->type->Align));
            }
            return value;
        }
    }

    ASSERT(false);
    return NULL;
}

llvm::Value *emitExpr(Context *ctx, Expr *expr, llvm::Type *desiredType) {
    debugPos(ctx, expr->start);

    llvm::Value *value = NULL;
    switch (expr->kind) {
        case ExprKind_LitInt: {
            Expr_LitInt lit = expr->LitInt;
            CheckerInfo info = ctx->checkerInfo[expr->id];
            Type *type = info.BasicExpr.type;
            if (type->kind == TypeKind_Float) {
                value = llvm::ConstantFP::get(canonicalize(ctx, type), lit.val);
            } else {
                value = llvm::ConstantInt::get(canonicalize(ctx, type), lit.val, type->Flags & TypeFlag_Signed);
            }
            break;
        }

        case ExprKind_LitFloat: {
            Expr_LitFloat lit = expr->LitFloat;
            CheckerInfo info = ctx->checkerInfo[expr->id];
            Type *type = info.BasicExpr.type;
            value = llvm::ConstantFP::get(canonicalize(ctx, type), lit.val);
            break;
        }

        case ExprKind_LitNil: {
            CheckerInfo info = ctx->checkerInfo[expr->id];
            Type *type = info.BasicExpr.type;
            value = llvm::ConstantPointerNull::get((llvm::PointerType *) canonicalize(ctx, type));
            break;
        }

        case ExprKind_LitString:{
            value = ctx->b.CreateGlobalStringPtr(expr->LitString.val);
            // TODO: @Strings
            break;
        }

        case ExprKind_Ident: {
            value = emitExprIdent(ctx, expr);
            break;
        }

        case ExprKind_LitCompound: {
            value = emitExprLitCompound(ctx, expr);
            break;
        }

        case ExprKind_Selector:
            value = emitExprSelector(ctx, expr);
            break;

        case ExprKind_Unary:
            value = emitExprUnary(ctx, expr);
            break;

        case ExprKind_Binary:
            value = emitExprBinary(ctx, expr);
            break;

        case ExprKind_Subscript:
            value = emitExprSubscript(ctx, expr);
            break;

        case ExprKind_Call:
            value = emitExprCall(ctx, expr);
            break;
    }

    if (ctx->checkerInfo[expr->id].coerce != ConversionKind_None && desiredType) {
        // If desired type is NULL and coerce is not then it maybe because we are emitting the lhs of a expr
        value = coerceValue(ctx, ctx->checkerInfo[expr->id].coerce, value, desiredType);
    }

    return value;
}

llvm::Value *emitExprUnary(Context *ctx, Expr *expr) {
    ASSERT(expr->kind == ExprKind_Unary);
    Expr_Unary unary = expr->Unary;
    
    llvm::Value *val;
    if (unary.op == TK_And) {
        bool previousReturnAddress = ctx->returnAddress;
        ctx->returnAddress = true;
        val = emitExpr(ctx, unary.expr);
        ctx->returnAddress = previousReturnAddress;
    } else {
        val = emitExpr(ctx, unary.expr);
    }

    debugPos(ctx, expr->Unary.start);
    switch (unary.op) {
        case TK_Add:
        case TK_And:
            return val;
        case TK_Sub:
            return ctx->b.CreateNeg(val);
        case TK_Not:
        case TK_BNot:
            return ctx->b.CreateNot(val);

        case TK_Lss: {
            if (ctx->returnAddress)
                return val;

            return ctx->b.CreateAlignedLoad(val, BytesFromBits(ctx->checkerInfo[expr->id].BasicExpr.type->Align));
        };
    }

    ASSERT(false);
    return NULL;
}

llvm::Value *emitExprBinary(Context *ctx, Expr *expr) {
    Type *type = TypeFromCheckerInfo(ctx->checkerInfo[expr->Binary.lhs->id]);
    auto ty = canonicalize(ctx, type);
    b32 isInt = IsInteger(type);

    llvm::Value *lhs, *rhs;
    lhs = emitExpr(ctx, expr->Binary.lhs, /*desiredType:*/ ty);
    rhs = emitExpr(ctx, expr->Binary.rhs, /*desiredType:*/ ty);

    debugPos(ctx, expr->Binary.pos);
    switch (expr->Binary.op) {
        case TK_Add:
            return isInt ? ctx->b.CreateAdd(lhs, rhs) : ctx->b.CreateFAdd(lhs, rhs);
        case TK_Sub:
            return isInt ? ctx->b.CreateSub(lhs, rhs) : ctx->b.CreateFSub(lhs, rhs);
        case TK_Mul:
            return isInt ? ctx->b.CreateMul(lhs, rhs) : ctx->b.CreateFMul(lhs, rhs);

        case TK_And:
            return ctx->b.CreateAnd(lhs, rhs);
        case TK_Or:
            return ctx->b.CreateOr(lhs, rhs);
        case TK_Xor:
            return ctx->b.CreateXor(lhs, rhs);

        case TK_Div: {
            if (isInt) {
                return IsSigned(type) ? ctx->b.CreateSDiv(lhs, rhs) : ctx->b.CreateUDiv(lhs, rhs);
            } else {
                return ctx->b.CreateFDiv(lhs, rhs);
            }
        };

        case TK_Rem: {
            if (isInt) {
                return IsSigned(type) ? ctx->b.CreateSRem(lhs, rhs) : ctx->b.CreateSRem(lhs, rhs);
            } else {
                return ctx->b.CreateFRem(lhs, rhs);
            }
        };

        case TK_Land: {
            llvm::Value *x = ctx->b.CreateAnd(lhs, rhs);
            return ctx->b.CreateTruncOrBitCast(x, canonicalize(ctx, BoolType));
        };

        case TK_Lor: {
            llvm::Value *x = ctx->b.CreateOr(lhs, rhs);
            return ctx->b.CreateTruncOrBitCast(x, canonicalize(ctx, BoolType));
        };

        case TK_Shl: {
            return ctx->b.CreateShl(lhs, rhs);
        };

        case TK_Shr: {
            return IsSigned(type) ? ctx->b.CreateAShr(lhs, rhs) : ctx->b.CreateLShr(lhs, rhs);
        };

        case TK_Lss: {
            if (isInt) {
                return IsSigned(type) ? ctx->b.CreateICmpSLT(lhs, rhs) : ctx->b.CreateICmpULT(lhs, rhs);
            } else {
                return ctx->b.CreateFCmpOLT(lhs, rhs);
            }
        };

        case TK_Gtr: {
            if (isInt) {
                return IsSigned(type) ? ctx->b.CreateICmpSGT(lhs, rhs) : ctx->b.CreateICmpUGT(lhs, rhs);
            } else {
                return ctx->b.CreateFCmpOGT(lhs, rhs);
            }
        };

        case TK_Leq: {
            if (isInt) {
                return IsSigned(type) ? ctx->b.CreateICmpSLE(lhs, rhs) : ctx->b.CreateICmpULE(lhs, rhs);
            } else {
                return ctx->b.CreateFCmpOLE(lhs, rhs);
            }
        };

        case TK_Geq: {
            if (isInt) {
                return IsSigned(type) ? ctx->b.CreateICmpSGE(lhs, rhs) : ctx->b.CreateICmpUGE(lhs, rhs);
            } else {
                return ctx->b.CreateFCmpOGE(lhs, rhs);
            }
        };

        case TK_Eql: {
            return isInt ? ctx->b.CreateICmpEQ(lhs, rhs) : ctx->b.CreateFCmpOEQ(lhs, rhs);
        };

        case TK_Neq: {
            return isInt ? ctx->b.CreateICmpNE(lhs, rhs) : ctx->b.CreateFCmpONE(lhs, rhs);
        };
    }

    ASSERT(false);
    return NULL;
}

llvm::Value *emitExprSubscript(Context *ctx, Expr *expr) {
    CheckerInfo recvInfo = ctx->checkerInfo[expr->Subscript.expr->id];
    CheckerInfo indexInfo = ctx->checkerInfo[expr->Subscript.index->id];
    llvm::Value *aggregate;

    std::vector<llvm::Value *> indicies;

    llvm::Value *index = emitExpr(ctx, expr->Subscript.index);
    Type *indexType = TypeFromCheckerInfo(indexInfo);

    // NOTE: LLVM doesn't have unsigned integers and an index in the upper-half
    // of an unsigned integer would get wrapped and become negative. We can
    // prevent this by ZExt-ing the index
    if (indexType->Width < 64 && !IsSigned(indexType)) {
        index = ctx->b.CreateZExt(index, canonicalize(ctx, I64Type));
    }

    Type *recvType = TypeFromCheckerInfo(recvInfo);
    Type *resultType;
    switch (recvType->kind) {
    case TypeKind_Array: {
        bool previousReturnAddress = ctx->returnAddress;
        ctx->returnAddress = true;
        aggregate = emitExpr(ctx, expr->Subscript.expr);
        ctx->returnAddress = previousReturnAddress;
        indicies.push_back(llvm::ConstantInt::get(canonicalize(ctx, I64Type), 0));
        indicies.push_back(index);
        resultType = TypeFromCheckerInfo(recvInfo)->Array.elementType;
    } break;

    case TypeKind_Slice: {
        bool previousReturnAddress = ctx->returnAddress;
        ctx->returnAddress = true;
        llvm::Value *structPtr = emitExpr(ctx, expr->Subscript.expr);
        ctx->returnAddress = previousReturnAddress;
        llvm::Value *arrayPtr = ctx->b.CreateStructGEP(NULL, structPtr, 0);
        aggregate = ctx->b.CreateAlignedLoad(arrayPtr, ctx->targetMachine->getPointerSize());
        indicies.push_back(index);
        resultType = TypeFromCheckerInfo(recvInfo)->Slice.elementType;
    } break;

    case TypeKind_Pointer: {
        aggregate = emitExpr(ctx, expr->Subscript.expr);
        indicies.push_back(index);
        resultType = TypeFromCheckerInfo(recvInfo)->Pointer.pointeeType;
    } break;

    default:
        ASSERT(false);
        return NULL;
    }

    llvm::Value *val = ctx->b.CreateInBoundsGEP(aggregate, indicies);
    if (ctx->returnAddress) {
        return val;
    }
    
    return ctx->b.CreateAlignedLoad(val, BytesFromBits(resultType->Align));
}

llvm::Function *emitExprLitFunction(Context *ctx, Expr *expr, llvm::Function *fn = nullptr) {
    CheckerInfo info = ctx->checkerInfo[expr->id];
    debugPos(ctx, expr->start);

    if (!fn) {
        llvm::FunctionType *type = (llvm::FunctionType *) canonicalize(ctx, info.BasicExpr.type);
        fn = llvm::Function::Create(type, llvm::Function::LinkageTypes::ExternalLinkage, llvm::StringRef(), ctx->m);
    }

    // TODO: Set calling convention
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx->m->getContext(), "entry", fn);
    ctx->b.SetInsertPoint(entry);

    llvm::Function *prevFunction = ctx->fn;
    ctx->fn = fn;
    if (FlagDebug) {

        auto dbgType = (llvm::DISubroutineType *)debugCanonicalize(ctx, info.BasicExpr.type);

        // TODO: Set isOptimized in a logical way
        // TODO: Set isLocalToUnit (if a function is declared in a function scope)
        llvm::DISubprogram *sub = ctx->d.builder->createFunction(
            ctx->d.file,
            fn->getName(),      // Name (will be set correctly by the caller) (TODO)
            fn->getName(),      // LinkageName
            ctx->d.file,
            expr->start.line,
            dbgType,
            false,              // isLocalToUnit
            true,               // isDefinition
            expr->LitFunction.body->start.line,
            llvm::DINode::FlagPrototyped,
            false               // isOptimized (TODO)
        );
        fn->setSubprogram(sub);

        ctx->d.scope = sub;
    }

    // Unset the location for the prologue emission (leading instructions with no
    // location in a function are considered part of the prologue and the debugger
    // will run past them when breaking on a function)
    clearDebugPos(ctx);

    llvm::Function::arg_iterator args = fn->arg_begin();

    ForEachWithIndex(expr->LitFunction.type->TypeFunction.params, i, auto, it) {
        // TODO: Support unnamed parameters ($0, $1, $2)
        CheckerInfo paramInfo = ctx->checkerInfo[it->key->id];
        auto param = args++;
        param->setName(paramInfo.Ident.symbol->name);
        auto storage = createEntryBlockAlloca(ctx, paramInfo.Ident.symbol);
        paramInfo.Ident.symbol->backendUserdata = storage;
        ctx->b.CreateAlignedStore(param, storage, BytesFromBits(paramInfo.Ident.symbol->type->Align));

        if (FlagDebug) {
            auto dbg = ctx->d.builder->createParameterVariable(
                ctx->d.scope,
                paramInfo.Ident.symbol->name,
                (u32) i,
                ctx->d.file,
                it->start.line,
                debugCanonicalize(ctx, paramInfo.Ident.symbol->type),
                true
            );
            auto pos = ((Expr_KeyValue *) paramInfo.Ident.symbol->decl)->start;
            ctx->d.builder->insertDeclare(
                storage,
                dbg,
                ctx->d.builder->createExpression(),
                llvm::DebugLoc::get(pos.line, pos.column, ctx->d.scope),
                ctx->b.GetInsertBlock()
            );
        }
    }
    fn->arg_end();

    // FIXME: In order to support nested functions we must restore retBlock, deferStack, retValue ...

    // Setup return block

    ctx->retBlock = llvm::BasicBlock::Create(ctx->m->getContext(), "return", fn);

    if (info.BasicExpr.type->Function.numResults) {
        llvm::IRBuilder<> b(entry, entry->begin());
        llvm::AllocaInst *alloca = b.CreateAlloca(fn->getReturnType(), 0, "result");
        if (info.BasicExpr.type->Function.numResults == 1) {
            alloca->setAlignment(BytesFromBits(info.BasicExpr.type->Function.results[0]->Align));
        }
        ctx->retValue = alloca;
    }

    size_t len = ArrayLen(expr->LitFunction.body->stmts);
    for (size_t i = 0; i < len; i++) {
        emitStmt(ctx, expr->LitFunction.body->stmts[i]);
    }

    if (!ctx->b.GetInsertBlock()->getTerminator()) {
        ctx->b.CreateBr(ctx->retBlock);
    }

    // Set insert point to the return block
    ctx->b.SetInsertPoint(ctx->retBlock);
    if (!ctx->deferStack.empty()) {
        ctx->retBlock = llvm::BasicBlock::Create(ctx->m->getContext(), "return_post_defer", fn);
        ctx->b.CreateBr(ctx->deferStack[0]);
    }

    for (size_t i = 0; i < ctx->deferStack.size(); i++) {
        printf("%zu\n", ctx->deferStack.size());
        llvm::BasicBlock *block = ctx->deferStack[i];
        ctx->b.SetInsertPoint(block);
        if (i < ctx->deferStack.size() - 1) {
            ctx->b.CreateBr(ctx->deferStack[i + 1]);
        } else {
            ctx->b.CreateBr(ctx->retBlock);
        }
    }

    if (!ctx->deferStack.empty()) {
        ctx->b.SetInsertPoint(ctx->retBlock);
    }

    if (info.BasicExpr.type->Function.numResults == 1) {
        Type *retType = info.BasicExpr.type->Function.results[0];
        auto retValue = ctx->b.CreateAlignedLoad(ctx->retValue, BytesFromBits(retType->Align));
        ctx->b.CreateRet(retValue);
    } else if (info.BasicExpr.type->Function.numResults) {
        llvm::StructType *retType = llvm::dyn_cast<llvm::StructType>(fn->getReturnType());
        ASSERT_MSG(retType, "Expect a function with multiple returns to have a struct return type");
        auto retValue = ctx->b.CreateAlignedLoad(ctx->retValue, ctx->dataLayout.getStructLayout(retType)->getAlignment());
        ctx->b.CreateRet(retValue);
    } else {
        ctx->b.CreateRetVoid();
    }

    // FIXME: Return to previous scope
    ctx->d.scope = ctx->d.file;

    // Move the return block to the end, just for readability
    ctx->retBlock->moveAfter(ctx->b.GetInsertBlock());

    if (FlagDebug) {
        ctx->d.builder->finalizeSubprogram(fn->getSubprogram());
    }
    ctx->fn = prevFunction;

#if DEBUG
    if (llvm::verifyFunction(*fn, &llvm::errs())) {
        ctx->m->print(llvm::errs(), nullptr);
        ASSERT(false);
    }
#endif

    return fn;
}

llvm::StructType *emitExprTypeStruct(Context *ctx, Expr *expr) {
    ASSERT(expr->kind == ExprKind_TypeStruct);

    Type *type = TypeFromCheckerInfo(ctx->checkerInfo[expr->id]);

    if (type->Symbol->backendUserdata && !FlagDebug) {
        BackendStructUserdata *userdata = (BackendStructUserdata *) type->Symbol->backendUserdata;
        return userdata->type;
    }

    std::vector<llvm::Type *> elementTypes;
    std::vector<llvm::Metadata *> debugMembers;

    u32 index = 0;
    for (size_t i = 0; i < ArrayLen(expr->TypeStruct.items); i++) {
        AggregateItem item = expr->TypeStruct.items[i];
        Type *fieldType = type->Struct.members[index].type;

        llvm::Type *ty = canonicalize(ctx, fieldType);
        llvm::DIType *dty = debugCanonicalize(ctx, fieldType);
        for (size_t j = 0; j < ArrayLen(item.names); j++) {
            if (FlagDebug) {
            llvm::DIDerivedType *member = ctx->d.builder->createMemberType(
                ctx->d.scope,
                item.names[j],
                ctx->d.file,
                item.start.line,
                fieldType->Width,
                fieldType->Align,
                type->Struct.members[index].offset,
                llvm::DINode::DIFlags::FlagZero,
                dty
            );

            debugMembers.push_back(member);
            }
            elementTypes.push_back(ty);

            index += 1;
        }
    }

    llvm::StructType *ty;
    if (type->Symbol->backendUserdata) {
        ty = (llvm::StructType *) type->Symbol->backendUserdata;
    } else {
        ty = llvm::StructType::create(ctx->m->getContext(), elementTypes);
    }
    llvm::DICompositeType *debugType;
    if (FlagDebug) {
        debugType = ctx->d.builder->createStructType(
        ctx->d.scope,
        type->Symbol->name,
        ctx->d.file,
        type->Symbol->decl->start.line,
        type->Width,
        type->Align,
        llvm::DINode::DIFlags::FlagZero,
        NULL, // DerivedFrom
        ctx->d.builder->getOrCreateArray(debugMembers)
    );
    }

    if (type->Symbol) {
        ty->setName(type->Symbol->name);

        BackendStructUserdata *userdata;
        if (type->Symbol->backendUserdata) {
            userdata = (BackendStructUserdata *) type->Symbol->backendUserdata;
            ASSERT_MSG(!userdata->debugType, "debugType is only set here, which means this run twice");
        } else {
            userdata = (BackendStructUserdata *) ArenaAlloc(&ctx->arena, sizeof(BackendStructUserdata));
            userdata->type = ty;
            type->Symbol->backendUserdata = userdata;
        }
        if (FlagDebug) {
            userdata->debugType = debugType;
        }
    }

#if DEBUG
    // Checks the frontend layout matches the llvm backend
    const llvm::StructLayout *layout = ctx->dataLayout.getStructLayout(ty);
    for (u32 i = 0; i < type->Struct.numMembers; i++) {
        ASSERT(layout->getElementOffsetInBits(i) == type->Struct.members[i].offset);
        ASSERT(layout->getSizeInBits() == type->Width);
        ASSERT(layout->getAlignment() == type->Align / 8);
    }
#endif

    return ty;
}

void emitDeclConstant(Context *ctx, Decl *decl) {
    ASSERT(decl->kind == DeclKind_Constant);
    CheckerInfo info = ctx->checkerInfo[decl->id];
    Symbol *symbol = info.Constant.symbol;

    // TODO: CreateLifetimeStart for this symbol (if applicable)

    debugPos(ctx, decl->start);
    if (symbol->type->kind == TypeKind_Function && decl->Constant.values[0]->kind == ExprKind_LitFunction) {

        CheckerInfo info = ctx->checkerInfo[decl->Constant.values[0]->id];

        const char *name = symbol->externalName ? symbol->externalName : symbol->name;
        auto type = (llvm::FunctionType *) canonicalize(ctx, info.BasicExpr.type);
        auto fn = llvm::Function::Create(type, llvm::Function::LinkageTypes::ExternalLinkage, name, ctx->m);
        symbol->backendUserdata = fn;
        ctx->fn = fn;

        // FIXME: We need to start using external name correctly, it should be always set

        emitExprLitFunction(ctx, decl->Constant.values[0], fn);
        // FIXME: we need to set the debug info subprogram's actual name. Right now it will be set to the mangled name.
        // The reason we didn't do this initially is that there is no setName on DISubprogram. We will maybe need to
        //  provide the non mangled name directly to emitExprLitFunction :/

        return;
    }

    switch (decl->Constant.values[0]->kind) {
        case ExprKind_TypeStruct: {
            emitExprTypeStruct(ctx, decl->Constant.values[0]);
            break;
        }

        default: {
            llvm::Type *type = canonicalize(ctx, symbol->type);
            
            llvm::Value *value = emitExpr(ctx, decl->Constant.values[0]);

            llvm::GlobalVariable *global = new llvm::GlobalVariable(
                *ctx->m,
                type,
                true, // IsConstant
                symbol->flags & SymbolFlag_Global ?
                    llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::CommonLinkage,
                (llvm::Constant *) value, // Initializer
                symbol->name
            );

            symbol->backendUserdata = global;
            break;
        }
    }
}

void emitDeclVariable(Context *ctx, Decl *decl) {
    ASSERT(decl->kind == DeclKind_Variable);

    // TODO: CreateLifetimeStart for this symbol
    CheckerInfo info = ctx->checkerInfo[decl->id];
    Symbol **symbols = info.Variable.symbols;
    Decl_Variable var = decl->Variable;

    bool prevReturnAddress = ctx->returnAddress;
    ctx->returnAddress = false;

    size_t numLhs = ArrayLen(var.names);
    size_t rhsIndex = 0;
    for (size_t lhsIndex = 0; lhsIndex < numLhs;) {
        Expr *expr = var.values[rhsIndex++];
        llvm::Value *rhs = emitExpr(ctx, expr);

        Type *exprType = TypeFromCheckerInfo(ctx->checkerInfo[expr->id]);
        if (expr->kind == ExprKind_Call) {
            Conversion *conversions = ctx->checkerInfo[decl->id].Variable.conversions;

            size_t numValues = exprType->Tuple.numTypes;
            debugPos(ctx, var.names[lhsIndex]->start);

            if (numValues == 1) {
                Symbol *symbol = symbols[lhsIndex];
                llvm::Value *lhs = createVariable(symbol, var.names[lhsIndex]->start, ctx);

                // Conversions of tuples like this are stored on the lhs
                if (conversions[lhsIndex] != ConversionKind_None) {
                    ASSERT(lhs->getType()->isPointerTy());
                    rhs = coerceValue(ctx, conversions[lhsIndex], rhs, lhs->getType()->getPointerElementType());
                }

                setVariableInitializer(symbol, ctx, rhs);
                lhsIndex += 1;
            } else {
                // FIXME: Globals are not handled here at all
                // -vdka September 2018
                ASSERT_MSG(decl->owningScope != decl->owningPackage->scope, "Multiple Global variable declarations from calls unimplemented");
                llvm::Value *resultAddress = createEntryBlockAlloca(ctx, rhs->getType(), "");
                ctx->b.CreateStore(rhs, resultAddress);

                for (size_t resultIndex = 0; resultIndex < numValues; resultIndex++) {
                    Symbol *symbol = symbols[lhsIndex];
                    llvm::Value *lhs = createVariable(symbol, var.names[lhsIndex]->start, ctx);

                    llvm::Value *addr = ctx->b.CreateStructGEP(rhs->getType(), resultAddress, (u32) resultIndex);
                    llvm::Value *val = ctx->b.CreateLoad(addr);

                    // Conversions of tuples like this are stored on the lhs
                    if (conversions[lhsIndex] != ConversionKind_None) {
                        ASSERT(lhs->getType()->isPointerTy());
                        val = coerceValue(ctx, conversions[lhsIndex], val, lhs->getType()->getPointerElementType());
                    }

                    ctx->b.CreateAlignedStore(val, lhs, BytesFromBits(symbol->type->Align));
                    lhsIndex += 1;
                }
            }
        } else {
            Symbol *symbol = symbols[lhsIndex];
            createVariable(symbol, var.names[lhsIndex]->start, ctx);
            debugPos(ctx, decl->start);
            setVariableInitializer(symbol, ctx, rhs);
            lhsIndex += 1;
        }
    }
    ctx->returnAddress = prevReturnAddress;
}

void emitDeclForeign(Context *ctx, Decl *decl) {
    ASSERT(decl->kind == DeclKind_Foreign);
    CheckerInfo info = ctx->checkerInfo[decl->id];

    debugPos(ctx, decl->start);
    llvm::Type *type = canonicalize(ctx, info.Foreign.symbol->type);

    switch (info.Foreign.symbol->type->kind) {
        case TypeKind_Function: {
            llvm::Function *fn = llvm::Function::Create(
                (llvm::FunctionType *) type,
                llvm::Function::LinkageTypes::ExternalLinkage,
                info.Foreign.symbol->externalName,
                ctx->m
            );
            setCallingConvention(fn, decl->Foreign.callingConvention);
            info.Foreign.symbol->backendUserdata = fn;
            break;
        }

        default: {
            llvm::Constant *val = ctx->m->getOrInsertGlobal(info.Foreign.symbol->externalName, type);
            llvm::GlobalVariable *global = (llvm::GlobalVariable *) val;
            global->setExternallyInitialized(true);
            info.Foreign.symbol->backendUserdata = global;
            global->setConstant(decl->Foreign.isConstant);
        }
    }
}

void emitDeclForeignBlock(Context *ctx, Decl *decl) {
    ASSERT(decl->kind == DeclKind_ForeignBlock);

    size_t len = ArrayLen(decl->ForeignBlock.members);
    for (size_t i = 0; i < len; i++) {
        Decl_ForeignBlockMember it = decl->ForeignBlock.members[i];
        debugPos(ctx, it.start);
        llvm::Type *type = canonicalize(ctx, it.symbol->type);

        switch (it.symbol->type->kind) {
            case TypeKind_Function: {
                llvm::Function *fn = llvm::Function::Create(
                    (llvm::FunctionType *) type,
                    llvm::Function::LinkageTypes::ExternalLinkage,
                    it.symbol->externalName,
                    ctx->m
                );
                setCallingConvention(fn, decl->Foreign.callingConvention);
                it.symbol->backendUserdata = fn;
                break;
            }

            default: {
                auto global = (llvm::GlobalVariable *) ctx->m->getOrInsertGlobal(it.symbol->externalName, type);
                global->setExternallyInitialized(true);
                it.symbol->backendUserdata = global;
                global->setConstant(it.isConstant);
            }
        }
    }
}

void emitStmtLabel(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_Label);
    CheckerInfo_Label info = ctx->checkerInfo[stmt->id].Label;
    auto block = llvm::BasicBlock::Create(ctx->m->getContext(), info.symbol->name, ctx->fn);
    ctx->b.SetInsertPoint(block);
    info.symbol->backendUserdata = block;
}

void emitStmtAssign(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_Assign);
    Stmt_Assign assign = stmt->Assign;

    size_t numLhs = ArrayLen(assign.lhs);
    size_t rhsIndex = 0;
    for (size_t lhsIndex = 0; lhsIndex < numLhs;) {
        Expr *expr = assign.rhs[rhsIndex++];
        llvm::Value *rhs = emitExpr(ctx, expr);

        Type *exprType = TypeFromCheckerInfo(ctx->checkerInfo[expr->id]);
        if (expr->kind == ExprKind_Call) {
            size_t numValues = exprType->Tuple.numTypes;
            if (numValues == 1) {
                ctx->returnAddress = true;
                llvm::Value *lhs = emitExpr(ctx, assign.lhs[lhsIndex++]);
                ctx->returnAddress = false;
                debugPos(ctx, assign.start);
                ctx->b.CreateAlignedStore(rhs, lhs, BytesFromBits(exprType->Tuple.types[0]->Align));
            } else {
                // create some stack space to land the returned struct onto
                llvm::Value *resultAddress = createEntryBlockAlloca(ctx, rhs->getType(), "");
                ctx->b.CreateStore(rhs, resultAddress);

                for (size_t resultIndex = 0; resultIndex < numValues; resultIndex++) {
                    Expr *lhsExpr = assign.lhs[lhsIndex++];
                    ctx->returnAddress = true;
                    llvm::Value *lhs = emitExpr(ctx, lhsExpr);
                    ctx->returnAddress = false;
                    debugPos(ctx, assign.start);

                    llvm::Value *addr = ctx->b.CreateStructGEP(rhs->getType(), resultAddress, (u32) resultIndex);
                    llvm::Value *val = ctx->b.CreateLoad(addr);

                    CheckerInfo lhsInfo = ctx->checkerInfo[lhsExpr->id];
                    // Conversions of tuples like this are stored on the lhs
                    if (lhsInfo.coerce != ConversionKind_None) {
                        ASSERT(lhs->getType()->isPointerTy());
                        val = coerceValue(ctx, lhsInfo.coerce, val, lhs->getType()->getPointerElementType());
                    }

                    ctx->b.CreateAlignedStore(val, lhs, BytesFromBits(TypeFromCheckerInfo(lhsInfo)->Align));
                }
            }
        } else {
            Expr *lhsExpr = assign.lhs[lhsIndex++];
            ctx->returnAddress = true;
            llvm::Value *lhs = emitExpr(ctx, lhsExpr);
            ctx->returnAddress = false;
            Type *type = TypeFromCheckerInfo(ctx->checkerInfo[lhsExpr->id]);
            debugPos(ctx, assign.start);
            ctx->b.CreateAlignedStore(rhs, lhs, BytesFromBits(type->Align));
        }
    }
}

void emitStmtIf(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_If);
    auto pass = llvm::BasicBlock::Create(ctx->m->getContext(), "if.pass", ctx->fn);
    auto fail = stmt->If.fail ? llvm::BasicBlock::Create(ctx->m->getContext(), "if.fail", ctx->fn) : nullptr;
    auto post = llvm::BasicBlock::Create(ctx->m->getContext(), "if.post", ctx->fn);

    auto cond = emitExpr(ctx, stmt->If.cond, llvm::IntegerType::get(ctx->m->getContext(), 1));
    debugPos(ctx, stmt->If.start);
    ctx->b.CreateCondBr(cond, pass, fail ? fail : post);

    ctx->b.SetInsertPoint(pass);
    emitStmt(ctx, stmt->If.pass);
    if (!pass->getTerminator()) ctx->b.CreateBr(post);

    if (stmt->If.fail) {
        ctx->b.SetInsertPoint(fail);
        emitStmt(ctx, stmt->If.fail);
        if (!fail->getTerminator()) ctx->b.CreateBr(post);
    }
    ctx->b.SetInsertPoint(post);
}

void emitStmtReturn(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_Return);

    size_t numReturns = ArrayLen(stmt->Return.exprs);

    std::vector<llvm::Value *> values;
    for (size_t i = 0; i < numReturns; i++) {
        Expr *it = stmt->Return.exprs[i];
        llvm::Type *desiredType = ctx->fn->getReturnType();
        if (numReturns > 1) {
            llvm::StructType *type = llvm::dyn_cast<llvm::StructType>(ctx->fn->getReturnType());
            ASSERT_MSG(type, "Expected the context struct type for function with multiple return vals");
            desiredType = type->getElementType((u32) i);
        }
        auto value = emitExpr(ctx, it, desiredType);
        values.push_back(value);
    }
    clearDebugPos(ctx);
    if (values.size() > 1) {
        for (u32 idx = 0; idx < values.size(); idx++) {
            llvm::Value *elPtr = ctx->b.CreateStructGEP(ctx->fn->getReturnType(), ctx->retValue, idx);
            ctx->b.CreateStore(values[idx], elPtr);
        }
    } else if (values.size() == 1) {
        ctx->b.CreateStore(values[0], ctx->retValue);
    }
    debugPos(ctx, stmt->start);
    ctx->b.CreateBr(ctx->retBlock);
}

void emitStmtDefer(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_Defer);

    llvm::BasicBlock *block = llvm::BasicBlock::Create(ctx->m->getContext(), "defer", ctx->fn);
    ctx->deferStack.push_back(block);

    llvm::BasicBlock *prevBlock = ctx->b.GetInsertBlock();
    ctx->b.SetInsertPoint(block);

    emitStmt(ctx, stmt->Defer.stmt);
    ctx->b.SetInsertPoint(prevBlock);
}

void emitStmtFor(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_For);

    Stmt_For fore = stmt->For;

    llvm::Function *currentFunc = ctx->b.GetInsertBlock()->getParent();
    llvm::BasicBlock *body, *post, *cond, *step;
    body = post = cond = step = NULL;

    llvm::DIScope *oldScope = NULL;
    if (FlagDebug) {
        oldScope = ctx->d.scope;
        ctx->d.scope = ctx->d.builder->createLexicalBlock(oldScope, ctx->d.file, stmt->start.line, stmt->start.column);
    }

    if (fore.init) {
        debugPos(ctx, fore.init->start);
        emitStmt(ctx, fore.init);
    }

    if (fore.cond) {
        cond = llvm::BasicBlock::Create(ctx->m->getContext(), "for.cond", currentFunc);
        if (fore.step) {
            step = llvm::BasicBlock::Create(ctx->m->getContext(), "for.step", currentFunc);
        }

        body = llvm::BasicBlock::Create(ctx->m->getContext(), "for.body", currentFunc);
        post = llvm::BasicBlock::Create(ctx->m->getContext(), "for.post", currentFunc);

        ctx->b.CreateBr(cond);
        ctx->b.SetInsertPoint(cond);

        llvm::Value *condVal = emitExpr(ctx, fore.cond);
        condVal = ctx->b.CreateTruncOrBitCast(condVal, llvm::IntegerType::get(ctx->m->getContext(), 1));
        ctx->b.CreateCondBr(condVal, body, post);
    } else {
        if (fore.step) {
            step = llvm::BasicBlock::Create(ctx->m->getContext(), "for.step", currentFunc);
        }

        body = llvm::BasicBlock::Create(ctx->m->getContext(), "for.body", currentFunc);
        post = llvm::BasicBlock::Create(ctx->m->getContext(), "for.post", currentFunc);

        ctx->b.CreateBr(body);
    }

    { // Body
        ctx->b.SetInsertPoint(body);

        llvm::DIScope *oldScope = NULL;
        if (FlagDebug) {
            oldScope = ctx->d.scope;
            ctx->d.scope = ctx->d.builder->createLexicalBlock(oldScope, ctx->d.file, fore.body->start.line, fore.body->start.column);
        }

        ForEachWithIndex(fore.body->stmts, i, Stmt *, stmt) {
            emitStmt(ctx, stmt);
        }

        if (FlagDebug) {
            ctx->d.scope = oldScope;
        }
    }

    b32 hasJump = ctx->b.GetInsertBlock()->getTerminator() != NULL;
    if (fore.step) {
        if (!hasJump) {
            ctx->b.CreateBr(step);
        }

        ctx->b.SetInsertPoint(step);
        emitStmt(ctx, fore.step);
        ctx->b.CreateBr(cond);
    } else if (cond) {
        // `for x < 5 { /* ... */ }` || `for i := 1; x < 5; { /* ... */ }`
        if (!hasJump) {
            ctx->b.CreateBr(cond);
        }
    } else {
         // `for { /* ... */ }`
        if (!hasJump) {
            ctx->b.CreateBr(body);
        }
    }

    ctx->b.SetInsertPoint(post);

    if (FlagDebug) {
        ctx->d.scope = oldScope;
    }
}

void emitStmtBlock(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_Block);

    llvm::BasicBlock *block = llvm::BasicBlock::Create(ctx->m->getContext(), "", ctx->fn);
    ctx->b.CreateBr(block);
    ctx->b.SetInsertPoint(block);

    ForEach(stmt->Block.stmts, Stmt *) {
        emitStmt(ctx, it);
    }
}

void emitStmtSwitch(Context *ctx, Stmt *stmt) {
    ASSERT(stmt->kind == StmtKind_Switch);
    Stmt_Switch swt = stmt->Switch;

    CheckerInfo_Switch info = ctx->checkerInfo[stmt->id].Switch;

    if (!swt.match) {
        UNIMPLEMENTED(); // TODO: Boolean-esque switch
    }

    llvm::BasicBlock *currentBlock = ctx->b.GetInsertBlock();
    llvm::Function   *currentFunc = currentBlock->getParent();

    llvm::BasicBlock *post = llvm::BasicBlock::Create(ctx->m->getContext(), "switch.post", currentFunc);
    info.breakTarget->backendUserdata = post;
    llvm::BasicBlock *defaultBlock = NULL;

    std::vector<llvm::BasicBlock *> thenBlocks;
    ForEachWithIndex(swt.cases, i, Stmt *, c) {
        if (ArrayLen(c->SwitchCase.matches)) {
            llvm::BasicBlock *then = llvm::BasicBlock::Create(ctx->m->getContext(), "switch.then.case", currentFunc); // TODO: number cases
            thenBlocks.push_back(then);
        } else {
            llvm::BasicBlock *then = llvm::BasicBlock::Create(ctx->m->getContext(), "switch.default", currentFunc);
            defaultBlock = then;
            thenBlocks.push_back(then);
        }
    }

    llvm::Value *value;
    llvm::Value *tag = NULL;

    if (swt.match) {
        value = emitExpr(ctx, swt.match);
    } else {
        value = llvm::ConstantInt::get(canonicalize(ctx, BoolType), 1);
    }

    size_t caseCount = ArrayLen(swt.cases);
    std::vector<std::vector<llvm::Value *>> matches;
    ForEachWithIndex(swt.cases, j, Stmt *, c) {
        if (j+1 < caseCount) {
            CheckerInfo_Case nextCase = ctx->checkerInfo[swt.cases[j+1]->id].Case;
            nextCase.fallthroughTarget->backendUserdata = thenBlocks[j+1];;
        }

        Stmt_SwitchCase caseStmt = c->SwitchCase;

        llvm::BasicBlock *thenBlock = thenBlocks[j];
        ctx->b.SetInsertPoint(thenBlock);

        // TODO: unions and any support
        if (ArrayLen(caseStmt.matches)) {
            std::vector<llvm::Value *> vals;
            For(caseStmt.matches) {
                vals.push_back(emitExpr(ctx, caseStmt.matches[i]));
            }

            matches.push_back(vals);
        }

        ForEach(caseStmt.block->stmts, Stmt *) {
            emitStmt(ctx, it);
        }

        b32 hasTerm = ctx->b.GetInsertBlock()->getTerminator() != NULL;
        if (!hasTerm) {
            ctx->b.CreateBr(post);
        }
        ctx->b.SetInsertPoint(currentBlock);
    }

    llvm::SwitchInst *swtch = ctx->b.CreateSwitch(
        tag ? tag : value,
        defaultBlock ? defaultBlock : post,
        (u32)thenBlocks.size()
    );

    size_t count = MIN(matches.size(), thenBlocks.size());
    for (size_t i = 0; i < count; i += 1) {
        llvm::BasicBlock *block = thenBlocks[i];

        for (size_t j = 0; j < matches[i].size(); j += 1) {
            swtch->addCase((llvm::ConstantInt *)matches[i][j], block);
        }
    }

    ctx->b.SetInsertPoint(post);
}

void emitStmt(Context *ctx, Stmt *stmt) {
    if (stmt->kind > _StmtExprKind_Start && stmt->kind < _StmtExprKind_End) {
        emitExpr(ctx, (Expr *) stmt);
        return;
    }

    switch (stmt->kind) {
        case StmtDeclKind_Constant:
            emitDeclConstant(ctx, (Decl *) stmt);
            break;

        case StmtDeclKind_Variable:
            emitDeclVariable(ctx, (Decl *) stmt);
            break;

        case StmtDeclKind_Foreign:
            emitDeclForeign(ctx, (Decl *) stmt);
            break;

        case StmtDeclKind_ForeignBlock:
            emitDeclForeignBlock(ctx, (Decl *) stmt);
            break;

        case StmtDeclKind_Import:
            setDebugInfoForPackage(ctx->d.builder, (Package *) stmt->Import.symbol->backendUserdata);
            break;

        case StmtKind_Label:
            emitStmtLabel(ctx, stmt);
            break;

        case StmtKind_Assign:
            emitStmtAssign(ctx, stmt);
            break;

        case StmtKind_If:
            emitStmtIf(ctx, stmt);
            break;

        case StmtKind_Return:
            emitStmtReturn(ctx, stmt);
            break;

        case StmtKind_Defer:
            emitStmtDefer(ctx, stmt);
            break;
        
        case StmtKind_Block:
            emitStmtBlock(ctx, stmt);
            break;

        case StmtKind_For:
            emitStmtFor(ctx, stmt);
            break;
        
        case StmtKind_Switch:
            emitStmtSwitch(ctx, stmt);
            break;

        case StmtKind_Goto: {
            CheckerInfo_Goto info = ctx->checkerInfo[stmt->id].Goto;
            if (!info.target) {
                UNIMPLEMENTED();
            }

            ASSERT(info.target->backendUserdata);
            ctx->b.CreateBr((llvm::BasicBlock *)info.target->backendUserdata);
        } break;

        default:
            ASSERT(false);
    }
}

void setupTargetInfo() {
    static b32 init = false;
    if (init) return;

    // TODO: Initialize only for the targets we are outputting.
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86AsmPrinter();

    init = true;
}

b32 CodegenLLVM(Package *p) {
    llvm::LLVMContext context;
    llvm::Module *module = new llvm::Module(p->path, context);

    setupTargetInfo();

    std::string error;
    llvm::Triple triple = llvm::Triple(llvm::sys::getDefaultTargetTriple());
    if (triple.getOS() == llvm::Triple::Darwin) {
        triple.setOS(llvm::Triple::MacOSX);
    }

    auto target = llvm::TargetRegistry::lookupTarget(triple.str(), error);
    if (!target) {
        llvm::errs() << error;
        return 1;
    }

    if (FlagVerbose) printf("Target: %s\n", triple.str().c_str());

    const char *cpu = "generic";
    const char *features = "";

    llvm::TargetOptions opt;
    llvm::TargetMachine *targetMachine = target->createTargetMachine(triple.str(), cpu, features, opt, llvm::None);

    // TODO: Only on unoptimized builds
    targetMachine->setO0WantsFastISel(true);

    llvm::DataLayout dataLayout = targetMachine->createDataLayout();

    // TODO: Handle targets correctly by using TargetOs & TargetArch globals
    module->setTargetTriple(triple.str());
    module->setDataLayout(dataLayout);
    module->getOrInsertModuleFlagsMetadata();
    module->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
    module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);
    // See https://llvm.org/docs/LangRef.html#c-type-width-module-flags-metadata
    module->addModuleFlag(llvm::Module::Warning, "short_enum", 1);
    // TODO: Include details about the kai compiler version in the module metadata. clang has the following:
    /*
     !llvm.ident = !{!1}
     !1 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
     */

    Debug debug;
    if (FlagDebug) {
        llvm::DIBuilder *builder = new llvm::DIBuilder(*module);

        llvm::DIFile *file = setDebugInfoForPackage(builder, p);

        // TODO: Set isOptimized in a logical way
        // TODO: Set RuntimeVersion in some logical way
        llvm::DICompileUnit *unit = builder->createCompileUnit(
            llvm::dwarf::DW_LANG_C,
            file,
            "Kai programming language",
            false,  // isOptimized
            "",
            0       // RuntimeVersion
        );

        DebugTypes types;
        initDebugTypes(builder, &types);

        debug = {builder, unit, file, types, unit};
    }

    llvm::IRBuilder<> b(context);
    Context ctx = {
        .checkerInfo = p->checkerInfo,
        .m = module,
        .targetMachine = targetMachine,
        .dataLayout = dataLayout,
        .b = b,
        .fn = nullptr,
        .d = debug,
    };

    // TODO: Figure out where and how we want to handle these
    TrueSymbol->backendUserdata = llvm::ConstantInt::get(canonicalize(&ctx, BoolType), 1);
    FalseSymbol->backendUserdata = llvm::ConstantInt::get(canonicalize(&ctx, BoolType), 0);

    // NOTE: Unset the location for the prologue emission (leading instructions
    // with nolocation in a function are considered part of the prologue and the
    // debugger will run past them when breaking on a function)
    Position pos;
    pos.line = 0;
    pos.column = 0;
    debugPos(&ctx, pos);

    For (p->stmts) {
        emitStmt(&ctx, p->stmts[i]);
    }

    if (FlagDebug) ctx.d.builder->finalize();

#if DEBUG
    if (llvm::verifyModule(*module, &llvm::errs())) {
        module->print(llvm::errs(), nullptr);
        ASSERT(false);
    }
#endif

    if (FlagDumpIR) {
        module->print(llvm::outs(), nullptr);
        return 0;
    } else {
        char path[MAX_PATH];
        strcpy(path, p->path);
        char *filename = strrchr(path, '/');
        *(filename++) = '\0';
        return emitObjectFile(p, filename, &ctx);
    }
}


b32 emitObjectFile(Package *p, char *name, Context *ctx) {
    char *objectName = KaiToObjectExtension(name);

    std::error_code ec;
    llvm::raw_fd_ostream dest(objectName, ec, llvm::sys::fs::F_None);

    if (ec) {
        llvm::errs() << "Could not open object file: " << ec.message();
        return 1;
    }

    llvm::legacy::PassManager pass;
    llvm::TargetMachine::CodeGenFileType fileType = llvm::TargetMachine::CGFT_ObjectFile;
    if (ctx->targetMachine->addPassesToEmitFile(pass, dest, fileType)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*ctx->m);
    dest.flush();

    if (!FlagLink)
        return 0;

    // Linking and debug symbols
#ifdef SYSTEM_OSX
    bool isStatic = OutputType == OutputType_Static;

    DynamicArray(u8) linkerFlags = NULL;
    if (isStatic) {
        ArrayPrintf(linkerFlags, "libtool -static -o %s %s", OutputName, objectName);
    } else {
        const char *outputType = OutputType == OutputType_Exec ? "execute" : "dynamic -dylib";
        ArrayPrintf(linkerFlags, "ld %s -o %s -lSystem -%s -macosx_version_min 10.13", objectName, OutputName, outputType);
    }

    if (FlagVerbose) {
        printf("%s\n", linkerFlags);
    }
    system((char *)linkerFlags);

    if (FlagDebug && !isStatic) {
        DynamicArray(u8) symutilFlags = NULL;
        ArrayPrintf(symutilFlags, "dsymutil %s", OutputName);

        if (FlagVerbose) {
            printf("%s\n", symutilFlags);
        }
        system((char *)symutilFlags);
    }
#else
    // TODO: linking on Windows and Linux
    UNIMPLEMENTED();
#endif

    return 0;
}

void clearDebugPos(Context *ctx) {
    if (!FlagDebug) return;
    ctx->b.SetCurrentDebugLocation(llvm::DebugLoc());
}

void debugPos(Context *ctx, Position pos) {
    if (!FlagDebug) return;
    ctx->b.SetCurrentDebugLocation(llvm::DebugLoc::get(pos.line, pos.column, ctx->d.scope));
}

void printIR(llvm::Module *value) {
    value->print(llvm::outs(), nullptr, true, true);
    puts("\n");
}

void printIR(llvm::Value *value) {
    value->print(llvm::outs());
    puts("\n");
}

void printIR(llvm::Type *value) {
    value->print(llvm::outs());
    puts("\n");
}

#pragma clang diagnostic pop
