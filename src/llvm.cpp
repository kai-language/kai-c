#include "common.h"
#include "array.h"
#include "map.h"
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

void CodegenLLVM(Package *p) {
    using namespace llvm;

    LLVMContext context;
    Module *module = new Module(p->path, context);

    if (FlagDumpIR) {
        module->print(errs(), nullptr);
    }

    delete module;
}



