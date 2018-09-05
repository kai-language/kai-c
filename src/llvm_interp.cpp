
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#pragma clang diagnostic ignored "-Wcomma"

#include <llvm/ADT/STLExtras.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Mangler.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>

#pragma clang diagnostic pop

using CompileLayer = llvm::orc::IRCompileLayer<llvm::orc::RTDyldObjectLinkingLayer, llvm::orc::SimpleCompiler>;
using ModuleHandle = llvm::orc::IRCompileLayer<llvm::orc::RTDyldObjectLinkingLayer, llvm::orc::SimpleCompiler>::ModuleHandleT;

ModuleHandle addModule(CompileLayer compileLayer, std::shared_ptr<llvm::Module> module) {
    auto Resolver = llvm::orc::createLambdaResolver(
        [&compileLayer](const std::string &Name) {
            auto Sym = compileLayer.findSymbol(Name, false);
            if (Sym) return Sym;

            return llvm::JITSymbol(nullptr);
        },
        [](const std::string &Name) {
            auto SymAddr = llvm::RTDyldMemoryManager::getSymbolAddressInProcess(Name);
            if (SymAddr) return llvm::JITSymbol(SymAddr, llvm::JITSymbolFlags::Exported);
            return llvm::JITSymbol(nullptr);
        });

    return llvm::cantFail(compileLayer.addModule(module, std::move(Resolver)));
}

llvm::JITSymbol findSymbol(CompileLayer compileLayer, const std::string Name) {
    return compileLayer.findSymbol(Name, true);
}

llvm::JITTargetAddress getSymbolAddress(CompileLayer compileLayer, const std::string Name) {
    return llvm::cantFail(findSymbol(compileLayer, Name).getAddress());
}

void removeModule(CompileLayer compileLayer, ModuleHandle handle) {
    cantFail(compileLayer.removeModule(handle));
}

extern "C"
void printModuleIR(llvm::Module *value) {
    value->print(llvm::outs(), nullptr, true, true);
    puts("\n");
}

extern "C"
void RunInterpLLVM(Package *package) {
    llvm::TargetMachine *tm = llvm::EngineBuilder().selectTarget();
    llvm::DataLayout dl = tm->createDataLayout();
    llvm::orc::RTDyldObjectLinkingLayer objectLayer = llvm::orc::RTDyldObjectLinkingLayer([]() {
        return std::make_shared<llvm::SectionMemoryManager>();
    });
    CompileLayer compileLayer = CompileLayer(objectLayer, llvm::orc::SimpleCompiler(*tm));

    // when passed a null pointer it will ‘load’ the host process itself, making its exported symbols available for execution.
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

    std::shared_ptr<llvm::Module> module = std::shared_ptr<llvm::Module>((llvm::Module *) package->backendUserdata);
    addModule(compileLayer, module);

    llvm::JITTargetAddress callbackToCompilerAddr = getSymbolAddress(compileLayer, "_callbackToCompiler");
    i32 (*callbackToCompiler)(void *) = (i32(*)(void *))(void *) callbackToCompilerAddr;

    printf("I am the compiler running!\n");
    callbackToCompiler(package->backendUserdata);
}
