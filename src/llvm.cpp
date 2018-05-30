#include "common.h"
#include "symbols.h"
#include "compiler.h"
#include "flags.h"
#include "lexer.h"
#include "ast.h"
#include "types.h"
#include "checker.h"
#include "llvm.h"

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

typedef struct LLVMGen {
    llvm::Module *m;
    llvm::Function *currentFunc;
    Debug *d;
} LLVMGen;

llvm::Type *canonicalize(LLVMGen *gen, Type *type) {
    switch (type->kind) {
    case TypeKind_UntypedInt:
        return llvm::IntegerType::get(gen->m->getContext(), 64);
    case TypeKind_Int:
        return llvm::IntegerType::get(gen->m->getContext(), type->width);
    case TypeKind_UntypedFloat:
        return llvm::Type::getDoubleTy(gen->m->getContext());
    case TypeKind_Float: {
        if (type->width == 32) {
            return llvm::Type::getFloatTy(gen->m->getContext());
        } else if (type->width == 64) {
            return llvm::Type::getDoubleTy(gen->m->getContext());
        } else {
            ASSERT(false);
        }
    }
    }

    return NULL;
}

llvm::DIType *debugCanonicalize(LLVMGen *gen, Type *type) {
    DebugTypes types = gen->d->types;

    if (type->kind == TypeKind_Int) {
        switch (type->width) {
        case 8:  return type->Int.isSigned ? types.i8  : types.u8;
        case 16: return type->Int.isSigned ? types.i16 : types.u16;
        case 32: return type->Int.isSigned ? types.i32 : types.u32;
        case 64: return type->Int.isSigned ? types.i64 : types.u64;
        }
    }

    if (type->kind == TypeKind_Float) {
        return type->width == 32 ? types.f32 : types.f64;
    }

    if (type->kind == TypeKind_Void) {
        return NULL;
    }

    ASSERT(false);
    return NULL;
}

void initDebugTypes(llvm::DIBuilder *b, DebugTypes *types) {
    types->i8  = b->createBasicType("i8",   8, llvm::dwarf::DW_ATE_signed_char);
    types->i16 = b->createBasicType("i16", 16, llvm::dwarf::DW_ATE_signed);
    types->i32 = b->createBasicType("i32", 32, llvm::dwarf::DW_ATE_signed);
    types->i64 = b->createBasicType("i64", 64, llvm::dwarf::DW_ATE_signed);

    types->u8  = b->createBasicType("u8",   8, llvm::dwarf::DW_ATE_unsigned_char);
    types->u16 = b->createBasicType("u16", 16, llvm::dwarf::DW_ATE_unsigned);
    types->u32 = b->createBasicType("u32", 32, llvm::dwarf::DW_ATE_unsigned);
    types->u64 = b->createBasicType("u64", 64, llvm::dwarf::DW_ATE_unsigned);

    types->f32 = b->createBasicType("f32", 32, llvm::dwarf::DW_ATE_float);
    types->f64 = b->createBasicType("f64", 64, llvm::dwarf::DW_ATE_float);
}

llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func, llvm::Type *type, const char *name) {
    llvm::IRBuilder<> b(&func->getEntryBlock(), func->getEntryBlock().begin());
    return b.CreateAlloca(type, 0, name);
}

llvm::Value *emitExpr(LLVMGen *gen, DynamicArray(CheckerInfo) checkerInfo, Expr *expr) {
    switch (expr->kind) {
    case ExprKind_LitInt: {
        Expr_LitInt lit = expr->LitInt;
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.LitInt.type;
        return llvm::ConstantInt::get(
            canonicalize(gen, type), 
            lit.val, 
            type->Int.isSigned
        );
    };

    case ExprKind_LitFloat: {
        Expr_LitFloat lit = expr->LitFloat;
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.LitFloat.type;
        return llvm::ConstantFP::get(canonicalize(gen, type), lit.val);
    };

    }

    ASSERT(false);
    return NULL;
}

void emitStmt(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Stmt *stmt) {
    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        CheckerInfo info = checkerInfo[stmt->id];
        Decl_Constant decl = stmt->Constant;
        Symbol *symbol = info.Decl.symbol;
        llvm::Type *type = canonicalize(gen, symbol->type);
        llvm::AllocaInst *alloca = createEntryBlockAlloca(gen->currentFunc, type, symbol->name);

        for (size_t i = 0; i < ArrayLen(decl.values); i++) {
            llvm::Value *value = emitExpr(gen, checkerInfo, decl.values[i]);
            b->CreateStore(value, alloca);
        }
    } break;
    }
} 

void debugPos(Package *p, Position pos);

void CodegenLLVM(Package *p) {
    llvm::LLVMContext context;

    llvm::Module *module = new llvm::Module(p->path, context);

    Debug _debug;
    Debug *debug = NULL;
    if (FlagDebug) {
        llvm::DIBuilder *builder = new llvm::DIBuilder(*module);

        char buff[MAX_PATH];
        char *dir;
        char *name = GetFileName(p->path, &buff[0], &dir);
        llvm::DIFile *file = builder->createFile(name, dir);

        llvm::DICompileUnit *unit = builder->createCompileUnit(
            llvm::dwarf::DW_LANG_C,
            file,
            "Kai programming language",
            0, "", 0
        );

        DebugTypes types;
        initDebugTypes(builder, &types);

        _debug = {builder, unit, file, types, unit};
        debug = &_debug;
    }

    LLVMGen _gen = {
        module,
        nullptr,
        debug
    };
    LLVMGen *gen = &_gen;

    llvm::IRBuilder<> b(context);

    std::vector<llvm::Type *> mainArgs;
    llvm::FunctionType *mainType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context),
        mainArgs,
        false
    );

    llvm::Function *main = (llvm::Function *)module->getOrInsertFunction("main", mainType);
    main->setCallingConv(llvm::CallingConv::C);
    
    if (FlagDebug) {
        llvm::DISubprogram *sub = debug->builder->createFunction(
            debug->unit,
            "main", 
            llvm::StringRef() /* link name */,
            debug->file,
            0,
            debug->builder->createSubroutineType(
                debug->builder->getOrCreateTypeArray(std::vector<llvm::Metadata *>())
            ),
            false, true, 0,
            llvm::DINode::FlagPrototyped,
            false
        );
        main->setSubprogram(sub);
    }

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(module->getContext(), "entry", main, 0);
    b.SetInsertPoint(entry);

    gen->currentFunc = main;

    for (size_t i = 0; i < ArrayLen(p->stmts); i++) {
        emitStmt(gen, &b, p->checkerInfo, p->stmts[i]);
    }

    b.CreateRetVoid();
    
    if (FlagDebug) {
        debug->builder->finalize();
    }

    if (FlagDumpIR) {
        gen->m->print(llvm::errs(), nullptr);
    }

    delete module;
}

void debugPos(LLVMGen *gen, llvm::IRBuilder<> *b, Position pos) {
    if (!FlagDebug) { return; }
    b->SetCurrentDebugLocation(llvm::DebugLoc::get(pos.line, pos.column, gen->d->scope));
}
