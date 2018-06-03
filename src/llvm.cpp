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

#include "llvm/IR/LegacyPassManager.h"

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>


#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

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
    llvm::Value **decls;
} LLVMGen;


/*
 * @Forward declarations
 */

void debugPos(LLVMGen *gen, llvm::IRBuilder<> *b, Position pos);
b32 emitObjectFile(Package *p, char *name, LLVMGen *gen);

llvm::Type *canonicalize(LLVMGen *gen, Type *type) {
repeat:
    switch (type->kind) {
    case TypeKind_Metatype: {
        type = type->Metatype.instanceType;
        goto repeat;
    }
        
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
    } break;

    case TypeKind_Pointer: {
        return llvm::PointerType::get(canonicalize(gen, type->Pointer.pointeeType), 0);
    } break;
    }

    ASSERT_MSG_VA(false, "Unable to canonicalize type %s", DescribeType(type));
    return NULL;
}

llvm::DIType *debugCanonicalize(LLVMGen *gen, Type *type) {
    DebugTypes types = gen->d->types;

    if (type->kind == TypeKind_Metatype) {
        return debugCanonicalize(gen, type->Metatype.instanceType);
    }

    if (type->kind == TypeKind_Int) {
        switch (type->width) {
        case 8:  return type->Int.isSigned ? types.i8  : types.u8;
        case 16: return type->Int.isSigned ? types.i16 : types.u16;
        case 32: return type->Int.isSigned ? types.i32 : types.u32;
        case 64: return type->Int.isSigned ? types.i64 : types.u64;
        }
    }

    if (type == UntypedIntType)
        return types.i64;

    if (type->kind == TypeKind_Float) {
        return type->width == 32 ? types.f32 : types.f64;
    }

    if (type == UntypedFloatType)
        return types.f64;

    if (type->kind == TypeKind_Void) {
        return NULL;
    }

    if (type->kind == TypeKind_Pointer) {
        return gen->d->builder->createPointerType(
            debugCanonicalize(gen, type->Pointer.pointeeType),
            64
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

llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func, llvm::Type *type, const char *name) {
    llvm::IRBuilder<> b(&func->getEntryBlock(), func->getEntryBlock().begin());
    return b.CreateAlloca(type, 0, name);
}

llvm::Value *emitExpr(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Expr *expr) {
    switch (expr->kind) {
    case ExprKind_LitInt: {
        Expr_LitInt lit = expr->LitInt;
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.BasicLit.type;
        debugPos(gen, b, expr->start);
        return llvm::ConstantInt::get(
            canonicalize(gen, type), 
            lit.val, 
            type->Int.isSigned
        );
    };

    case ExprKind_LitFloat: {
        Expr_LitFloat lit = expr->LitFloat;
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.BasicLit.type;
        debugPos(gen, b, expr->start);
        return llvm::ConstantFP::get(canonicalize(gen, type), lit.val);
    };

    case ExprKind_LitNil: {
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.NilLit.type;
        debugPos(gen, b, expr->start);
        return llvm::ConstantPointerNull::get((llvm::PointerType *)canonicalize(gen, type));
    } break;

    case ExprKind_Ident: {
        CheckerInfo info = checkerInfo[expr->id];
        Symbol *symbol = info.Ident.symbol;
        // TODO(Brett): check for return address
        return b->CreateLoad(gen->decls[symbol->decl->declId]);
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
        gen->decls[((Decl *)stmt)->declId] = (llvm::Value *) alloca;

        debugPos(gen, b, stmt->start);

        llvm::Value *value = emitExpr(gen, b, checkerInfo, decl.values[0]);
        b->CreateStore(value, alloca);
    } break;

    case StmtDeclKind_Variable: {
        CheckerInfo info = checkerInfo[stmt->id];
        DynamicArray(Symbol *) symbols = info.DeclList.symbols;
        Decl_Variable var = stmt->Variable;

        For (symbols) {
            Symbol *symbol = symbols[i];
            llvm::Type *type = canonicalize(gen, symbol->type);
            llvm::AllocaInst *alloca = createEntryBlockAlloca(gen->currentFunc, type, symbol->name);

            gen->decls[((Decl *)stmt)->declId] = (llvm::Value *) alloca;

            debugPos(gen, b, var.names[i]->start);

            if (FlagDebug) {
                llvm::DILocalVariable *d = gen->d->builder->createParameterVariable(
                    gen->d->scope,
                    symbol->name,
                    0,
                    gen->d->file,
                    stmt->start.line,
                    debugCanonicalize(gen, symbol->type),
                    true
                );

                gen->d->builder->insertDeclare(
                    alloca,
                    d,
                    gen->d->builder->createExpression(),
                    llvm::DebugLoc::get(stmt->start.line, stmt->start.column, gen->d->scope),
                    b->GetInsertBlock()
                );
            }

            if (ArrayLen(var.values)) {
                llvm::Value *value = emitExpr(gen, b, checkerInfo, var.values[i]);
                b->CreateStore(value, alloca);
            }
        }
    } break;
    }
} 


b32 CodegenLLVM(Package *p) {
    llvm::LLVMContext context;
    llvm::Module *module = new llvm::Module(p->path, context);

    char buff[MAX_PATH];
    char *dir;
    char *name = GetFileName(p->path, &buff[0], &dir);

    Debug _debug;
    Debug *debug = NULL;
    if (FlagDebug) {
        llvm::DIBuilder *builder = new llvm::DIBuilder(*module);
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

    llvm::Value **decls = (llvm::Value **) ArenaAlloc(&p->arena, sizeof(llvm::Value *)*p->declCount+1);

    LLVMGen _gen = {
        module,
        nullptr,
        debug,
        decls
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
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(module->getContext(), "entry", main, 0);
    b.SetInsertPoint(entry);
    gen->currentFunc = main;

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
        debug->scope = sub;
    }

    // NOTE: Unset the location for the prologue emission (leading instructions
    // with nolocation in a function are considered part of the prologue and the
    // debugger will run past them when breaking on a function)
    Position pos;
    pos.line = 0;
    pos.column = 0;
    debugPos(gen, &b, pos);

    For (p->stmts) {
        emitStmt(gen, &b, p->checkerInfo, p->stmts[i]);
    }

    b.CreateRetVoid();
    
    debug->scope = debug->unit;

    if (FlagDebug) {
        debug->builder->finalize();
    }

#if defined(SLOW) || defined(DIAGNOSTICS) || defined(DEBUG)
    if (llvm::verifyFunction(*main, &llvm::errs()) || llvm::verifyModule(*module, &llvm::errs())) {
        ASSERT(false);
    }
#endif

    if (FlagDumpIR) {
        gen->m->print(llvm::errs(), nullptr);
        return 0;
    } else {
        return emitObjectFile(p, name, gen);
    }
}

void setupTargetInfo() {
    static b32 init = false;
    if (init) return;

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    init = true;
}

b32 emitObjectFile(Package *p, char *name, LLVMGen *gen) {
    setupTargetInfo();

    char *objectName = KaiToObjectExtension(name);

    std::string targetTriple, error;
    targetTriple = llvm::sys::getDefaultTargetTriple();
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

    if (!target) {
        llvm::errs() << error;
        return 1;
    }

    const char *cpu = "generic";
    const char *features = "";

    llvm::TargetOptions opt;
    llvm::Optional<llvm::Reloc::Model> rm = llvm::Optional<llvm::Reloc::Model>();
    llvm::TargetMachine *targetMachine = target->createTargetMachine(
        targetTriple,
        cpu,
        features,
        opt,
        rm
    );

    gen->m->setDataLayout(targetMachine->createDataLayout());
    gen->m->setTargetTriple(targetTriple);

    std::error_code ec;
    llvm::raw_fd_ostream dest(objectName, ec, llvm::sys::fs::F_None);

    if (ec) {
        llvm::errs() << "Could not open object file: " << ec.message();
        return 1;
    }

    llvm::legacy::PassManager pass;
    llvm::TargetMachine::CodeGenFileType fileType = llvm::TargetMachine::CGFT_ObjectFile;
    if (targetMachine->addPassesToEmitFile(pass, dest, fileType)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*gen->m);
    dest.flush();

    // Linking and debug symbols
#ifdef SYSTEM_OSX
    DynamicArray(u8) linkerFlags = NULL;
    ArrayPrintf(linkerFlags, "ld %s -o %s -lSystem -macosx_version_min 10.13", objectName, OutputName);
    system((char *)linkerFlags);

    if (FlagDebug) {
        DynamicArray(u8) symutilFlags = NULL;
        ArrayPrintf(symutilFlags, "symutil %s", OutputName);
    }
#else
    // TODO: linking on mac and Linux
    UNIMPLEMENTED();
#endif

    return 0;
}

void debugPos(LLVMGen *gen, llvm::IRBuilder<> *b, Position pos) {
    if (!FlagDebug) { return; }
    b->SetCurrentDebugLocation(llvm::DebugLoc::get(pos.line, pos.column, gen->d->scope));
}
