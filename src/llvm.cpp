
#include "common.h"
#include "symbols.h"
#include "compiler.h"
#include "flags.h"
#include "lexer.h"
#include "ast.h"
#include "types.h"
#include "checker.h"
#include "llvm.h"

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
    llvm::IRBuilder<> b;
    llvm::Function *fn;
    llvm::BasicBlock *retBlock;
    llvm::Value *retValue;
    bool returnAddress;
    Debug d;

    std::vector<llvm::BasicBlock *> deferStack;
};

void clearDebugPos(Context *ctx);
void debugPos(Context *ctx, Position pos);
b32 emitObjectFile(Package *p, char *name, Context *ctx);
llvm::Value *emitExpr(Context *ctx, Expr *expr, llvm::Type *desiredType = nullptr);
llvm::Value *emitExprBinary(Context *ctx, Expr *expr);
llvm::Value *emitExprUnary(Context *ctx, Expr *expr);
void emitStmt(Context *ctx, Stmt *stmt);

llvm::Type *canonicalize(Context *ctx, Type *type) {
    switch (type->kind) {
        case TypeKind_Tuple:
            if (!type->Tuple.types) {
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

        case TypeKind_Pointer: {
            return llvm::PointerType::get(canonicalize(ctx, type->Pointer.pointeeType), 0);
        } break;

        case TypeKind_Function: {
            ASSERT_MSG(ArrayLen(type->Function.results) == 1, "Currently we don't support multi-return");
            std::vector<llvm::Type *> params;
            For(type->Function.params) {
                llvm::Type *paramType = canonicalize(ctx, type->Function.params[i]);
                params.push_back(paramType);
            }

            // TODO(Brett): canonicalize multi-return and Kai vargs
            llvm::Type *returnType = canonicalize(ctx, type->Function.results[0]);
            return llvm::FunctionType::get(returnType, params, (type->Function.Flags & TypeFlag_CVargs) != 0);
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

    if (type->kind == TypeKind_Tuple && !type->Tuple.types) {
        return NULL;
    }

    if (type->kind == TypeKind_Pointer) {
        return ctx->d.builder->createPointerType(
            debugCanonicalize(ctx, type->Pointer.pointeeType),
            ctx->m->getDataLayout().getPointerSize()
        );
    }

    if (type->kind == TypeKind_Function) {
        // NOTE: Clang just uses a derived type that is a pointer
        // !44 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !9, size: 64)
        std::vector<llvm::Metadata *> parameterTypes;
        ForEach(type->Function.params, Type *) {
            auto type = debugCanonicalize(ctx, it);
            parameterTypes.push_back(type);
        }

        auto pTypes = ctx->d.builder->getOrCreateTypeArray(parameterTypes);
        return ctx->d.builder->createSubroutineType(pTypes);
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

llvm::Value *coerceValue(Context *ctx, Conversion conversion, llvm::Value *value, llvm::Type *target) {
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

        default:
            return NULL;
    }
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

llvm::Value *emitExpr(Context *ctx, Expr *expr, llvm::Type *desiredType) {
    debugPos(ctx, expr->start);

    llvm::Value *value = NULL;
    switch (expr->kind) {
        case ExprKind_LitInt: {
            Expr_LitInt lit = expr->LitInt;
            CheckerInfo info = ctx->checkerInfo[expr->id];
            Type *type = info.BasicExpr.type;
            value = llvm::ConstantInt::get(canonicalize(ctx, type), lit.val, type->Flags & TypeFlag_Signed);
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

        case ExprKind_Ident: {
            CheckerInfo info = ctx->checkerInfo[expr->id];
            Symbol *symbol = info.Ident.symbol;
            value = (llvm::Value *) symbol->backendUserdata;
            if (symbol->kind == SymbolKind_Variable) {
                if (!ctx->returnAddress) {
                    value = ctx->b.CreateAlignedLoad(value, BytesFromBits(symbol->type->Align));
                }
            }
            break;
        }

        case ExprKind_Unary:
            value = emitExprUnary(ctx, expr);
            break;

        case ExprKind_Binary:
            value = emitExprBinary(ctx, expr);
            break;

        case ExprKind_Call:
            value = emitExprCall(ctx, expr);
            break;
    }

    if (ctx->checkerInfo[expr->id].coerce != ConversionKind_None) {
        value = coerceValue(ctx, ctx->checkerInfo[expr->id].coerce, value, desiredType);
    }

    return value;
}

llvm::Value *emitExprUnary(Context *ctx, Expr *expr) {
    ASSERT(expr->kind == ExprKind_Unary);
    Expr_Unary unary = expr->Unary;
    llvm::Value *val = emitExpr(ctx, unary.expr);

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
            // TODO: check for return address
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

llvm::Function *emitExprLitFunction(Context *ctx, Expr *expr, llvm::Function *fn = nullptr) {
    CheckerInfo info = ctx->checkerInfo[expr->id];
    debugPos(ctx, expr->start);

    if (!fn) {
        llvm::FunctionType *type = (llvm::FunctionType *) canonicalize(ctx, info.BasicExpr.type);
        fn = llvm::Function::Create(type, llvm::Function::LinkageTypes::ExternalLinkage, llvm::StringRef(), ctx->m);
    }

    fn->setCallingConv(llvm::CallingConv::C);
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx->m->getContext(), "entry", fn);
    ctx->b.SetInsertPoint(entry);
    ctx->fn = fn;
    if (FlagDebug) {

        auto dbgType = (llvm::DISubroutineType *)debugCanonicalize(ctx, info.BasicExpr.type);
        llvm::DISubprogram *sub = ctx->d.builder->createFunction(
            ctx->d.file,
            fn->getName(), // Name (will be set correctly by the caller) (TODO)
            fn->getName(), // LinkageName
            ctx->d.file,
            expr->start.line,
            dbgType,
            false,
            true,
            expr->LitFunction.body->start.line,
            llvm::DINode::FlagPrototyped,
            false
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
    fn->arg_end();

    // FIXME: In order to support nested functions we must restore retBlock, deferStack, retValue ...

    // Setup return block

    // TODO: multiple returns (we should change TypeFunction to have a single possibly tuple return type)
    Type *retType = info.BasicExpr.type->Function.results[0];
    ctx->retBlock = llvm::BasicBlock::Create(ctx->m->getContext(), "return", fn);
    if (!fn->getReturnType()->isVoidTy()) {
        ctx->retValue = createEntryBlockAlloca(ctx, retType, "result");
    }
    ForEach(expr->LitFunction.body->stmts, Stmt *) {
        emitStmt(ctx, it);
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

    if (fn->getReturnType()->isVoidTy()) {
        ctx->b.CreateRetVoid();
    } else {
        auto retValue = ctx->b.CreateAlignedLoad(ctx->retValue, BytesFromBits(retType->Align));
        ctx->b.CreateRet(retValue);
    }

    // FIXME: Return to previous scope
    ctx->d.scope = ctx->d.file;

    // Move the return block to the end, just for readability
    ctx->retBlock->moveAfter(ctx->b.GetInsertBlock());

    if (FlagDebug) {
        ctx->d.builder->finalizeSubprogram(fn->getSubprogram());
    }

#if defined(SLOW) || defined(DIAGNOSTICS) || defined(DEBUG)
    if (llvm::verifyFunction(*fn, &llvm::errs())) {
        ctx->m->print(llvm::errs(), nullptr);
        ASSERT(false);
    }
#endif

    return fn;
}

void emitDeclConstant(Context *ctx, Decl *decl) {
    ASSERT(decl->kind == DeclKind_Constant);
    CheckerInfo info = ctx->checkerInfo[decl->id];
    Symbol *symbol = info.Constant.symbol;

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

    } else {
        llvm::Type *type = canonicalize(ctx, symbol->type);

        bool isGlobal = ctx->fn == NULL;
        llvm::GlobalVariable *global = new llvm::GlobalVariable(
            *ctx->m,
            type,
            /*isConstant:*/true,
            isGlobal ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::CommonLinkage,
            0,
            symbol->name
        );

        symbol->backendUserdata = global;

        llvm::Value *value = emitExpr(ctx, decl->Constant.values[0]);
        global->setInitializer((llvm::Constant *) value);
    }
}

void emitDeclVariable(Context *ctx, Decl *decl) {
    ASSERT(decl->kind == DeclKind_Variable);
    CheckerInfo info = ctx->checkerInfo[decl->id];
        DynamicArray(Symbol *) symbols = info.Variable.symbols;
        Decl_Variable var = decl->Variable;

        For (symbols) {
            Symbol *symbol = symbols[i];
            // FIXME: Global variables
            debugPos(ctx, var.names[i]->start);

            llvm::AllocaInst *alloca = createEntryBlockAlloca(ctx, symbol);
            symbol->backendUserdata = alloca;

            if (FlagDebug) {
                auto d = ctx->d.builder->createAutoVariable(
                    ctx->d.scope,
                    symbol->name,
                    ctx->d.file,
                    decl->start.line,
                    debugCanonicalize(ctx, symbol->type)
                );
                ctx->d.builder->insertDeclare(
                    alloca,
                    d,
                    ctx->d.builder->createExpression(),
                    llvm::DebugLoc::get(decl->start.line, decl->start.column, ctx->d.scope),
                    ctx->b.GetInsertBlock()
                );
            }

            if (ArrayLen(var.values) >= i) {
                Type *type = TypeFromCheckerInfo(ctx->checkerInfo[var.values[i]->id]);
                llvm::Value *value = emitExpr(ctx, var.values[i]);
                ctx->b.CreateAlignedStore(value, alloca, BytesFromBits(type->Align));
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

    if (ArrayLen(assign.lhs) > 1 && ArrayLen(assign.rhs) == 1 && assign.rhs[0]->kind == ExprKind_Call) {
//        llvm::Value *value = emitExpr(ctx, assign.rhs[0]);
        UNIMPLEMENTED();
    }
    ForEachWithIndex(assign.lhs, i, Expr *, it) {
        ctx->returnAddress = true;
        llvm::Value *lhs = emitExpr(ctx, it);
        ctx->returnAddress = false;
        llvm::Value *rhs = emitExpr(ctx, assign.rhs[i]);
        Type *type = TypeFromCheckerInfo(ctx->checkerInfo[assign.lhs[i]->id]);
        debugPos(ctx, assign.start);
        ctx->b.CreateAlignedStore(rhs, lhs, BytesFromBits(type->Align));
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
    if (ArrayLen(stmt->Return.exprs) != 1) UNIMPLEMENTED();
    std::vector<llvm::Value *> values;
    ForEach(stmt->Return.exprs, Expr *) {
        // FIXME: getReturnType will no suffice when we support multireturn
        auto value = emitExpr(ctx, it, ctx->fn->getReturnType());
        values.push_back(value);
    }
    clearDebugPos(ctx);
    if (values.size() > 1) {
        for (u32 idx = 0; idx < values.size(); idx++) {
            std::vector<u32> idxs = {0, idx};
            ctx->retValue = ctx->b.CreateInsertValue(ctx->retValue, values[idx], idxs);
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

void emitStmt(Context *ctx, Stmt *stmt) {
    switch (stmt->kind) {
        case StmtDeclKind_Constant:
            emitDeclConstant(ctx, (Decl *) stmt);
            break;

        case StmtDeclKind_Variable:
            emitDeclVariable(ctx, (Decl *) stmt);
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
    }
}

void setupTargetInfo() {
    static b32 init = false;
    if (init) return;

    // TODO: If no target is specified we can get away with initializing just the native target.
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    init = true;
}

b32 CodegenLLVM(Package *p) {
    llvm::LLVMContext context;
    llvm::Module *module = new llvm::Module(p->path, context);

    setupTargetInfo();

    std::string error;
    auto triple = llvm::sys::getDefaultTargetTriple();
    auto target = llvm::TargetRegistry::lookupTarget(triple, error);

    if (!target) {
        llvm::errs() << error;
        return 1;
    }

    if (FlagVerbose) printf("Target: %s\n", triple.c_str());

    const char *cpu = "generic";
    const char *features = "";

    llvm::TargetOptions opt;
    llvm::Optional<llvm::Reloc::Model> rm = llvm::Optional<llvm::Reloc::Model>();
    llvm::TargetMachine *targetMachine = target->createTargetMachine(triple, cpu, features, opt, rm);

    // TODO: Only on unoptimized builds
    targetMachine->setO0WantsFastISel(true);

    // TODO: Handle targets correctly by using TargetOs & TargetArch globals
    module->setTargetTriple(triple);
    module->setDataLayout(targetMachine->createDataLayout());
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

    char *path = AbsolutePath(p->path, NULL);
    char buff[MAX_PATH];
    char *dir;
    char *name = GetFileName(path, buff, &dir);

    Debug debug;
    if (FlagDebug) {
        llvm::DIBuilder *builder = new llvm::DIBuilder(*module);

        // TODO: Absolute path for dir
        llvm::DIFile *file = builder->createFile(name, dir);

        llvm::DICompileUnit *unit = builder->createCompileUnit(
            llvm::dwarf::DW_LANG_C,
            file,
            "Kai programming language",
            /* isOptimized:*/ false,
            "",
            /* RuntimeVersion:*/ 0
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
        .b = b,
        .fn = nullptr,
        .d = debug,
    };

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

    ctx.d.builder->finalize();

#if defined(SLOW) || defined(DIAGNOSTICS) || defined(DEBUG)
    if (llvm::verifyModule(*module, &llvm::errs())) {
        module->print(llvm::errs(), nullptr);
        ASSERT(false);
    }
#endif

    if (FlagDumpIR) {
        module->print(llvm::outs(), nullptr);
        return 0;
    } else {
        return emitObjectFile(p, name, &ctx);
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

    // Linking and debug symbols
#ifdef SYSTEM_OSX
    DynamicArray(u8) linkerFlags = NULL;
    ArrayPrintf(linkerFlags, "ld %s -o %s -lSystem -macosx_version_min 10.13", objectName, OutputName);

    if (FlagVerbose) {
        printf("%s\n", linkerFlags);
    }
    system((char *)linkerFlags);

    if (FlagDebug) {
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

void printIR(llvm::Value *value) {
    value->print(llvm::outs());
}

void printIR(llvm::Type *value) {
    value->print(llvm::outs());
}

#pragma clang diagnostic pop
