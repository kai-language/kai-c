#include "common.h"
#include "symbols.h"
#include "compiler.h"
#include "flags.h"
#include "lexer.h"
#include "ast.h"
#include "types.h"
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

void CodegenLLVM(Package *p) {
    llvm::LLVMContext context;
    llvm::Module *module = new llvm::Module(p->path, context);

    LLVMGen _gen = {
        module 
    };
    LLVMGen *gen = &_gen;

    if (FlagDumpIR) {
        module->print(llvm::errs(), nullptr);
    }

    delete module;
}

LLVMTypeRef canonicalize(LLVMGen *gen, Type *type) {
    return NULL;
}

