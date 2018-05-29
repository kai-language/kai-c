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

typedef struct LLVMGen {
    llvm::Module *module;
} LLVMGen;

llvm::Type *canonicalize(LLVMGen *gen, Type *type) {
    switch (type->kind) {
    case TypeKind_Int:
        return llvm::IntegerType::get(gen->module->getContext(), type->width);
    case TypeKind_Float: {
        if (type->width == 32) {
            return llvm::Type::getFloatTy(gen->module->getContext());
        } else {
            return llvm::Type::getDoubleTy(gen->module->getContext());
        }
    }
    }

    return NULL;
}

void CodegenLLVM(Package *p) {
    llvm::LLVMContext context;
    llvm::Module *module = new llvm::Module(p->path, context);

    LLVMGen _gen = {
        module 
    };
    LLVMGen *gen = &_gen;

    std::vector<llvm::Type *> mainArgs;
    llvm::FunctionType *mainType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(module->getContext()),
        mainArgs,
        false
    );

    llvm::Function *main = module->getFunction("main");
    if (!main) {
        main = llvm::Function::Create(
            mainType,
            llvm::GlobalValue::ExternalLinkage,
            "main",
            module
        );

        main->setCallingConv(llvm::CallingConv::C);
    }

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(module->getContext(), "entry", main, 0);
    llvm::IRBuilder<> b(entry);
    b.CreateRetVoid();

    if (FlagDumpIR) {
        gen->module->print(llvm::errs(), nullptr);
    }

    delete module;
}


