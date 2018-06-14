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

typedef struct LLVMGen {
    llvm::Module *m;
    llvm::Function *currentFunc;
    Debug *d;
} LLVMGen;


/*
 * @Forward declarations
 */

void debugPos(LLVMGen *gen, llvm::IRBuilder<> *b, Position pos);
b32 emitObjectFile(Package *p, char *name, LLVMGen *gen);
llvm::Value *emitBinaryExpr(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Expr *expr);
llvm::Value *emitUnaryExpr(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Expr *expr);

llvm::Type *canonicalize(LLVMGen *gen, Type *type) {
    switch (type->kind) {
    case TypeKind_Void:
        return llvm::Type::getVoidTy(gen->m->getContext());
    case TypeKind_Int:
        return llvm::IntegerType::get(gen->m->getContext(), type->Width);

    case TypeKind_Float: {
        if (type->Width == 32) {
            return llvm::Type::getFloatTy(gen->m->getContext());
        } else if (type->Width == 64) {
            return llvm::Type::getDoubleTy(gen->m->getContext());
        } else {
            ASSERT(false);
        }
    } break;

    case TypeKind_Pointer: {
        return llvm::PointerType::get(canonicalize(gen, type->Pointer.pointeeType), 0);
    } break;

    case TypeKind_Function: {
        ASSERT_MSG(ArrayLen(type->Function.results) == 1, "Currently we don't support multi-return");
        std::vector<llvm::Type *> params;
        For(type->Function.params) {
            llvm::Type *paramType = canonicalize(gen, type);
            params.push_back(paramType);
        }

        // TODO(Brett): canonicalize multi-return and Kai vargs
        llvm::Type *returnType = canonicalize(gen, type->Function.results[0]);
        return llvm::FunctionType::get(returnType, params, (type->Function.Flags & TypeFlag_CVargs) != 0);
    } break;
    }

    ASSERT_MSG_VA(false, "Unable to canonicalize type %s", DescribeType(type));
    return NULL;
}

llvm::DIType *debugCanonicalize(LLVMGen *gen, Type *type) {
    DebugTypes types = gen->d->types;

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

    debugPos(gen, b, expr->start);

    switch (expr->kind) {
    case ExprKind_LitInt: {
        Expr_LitInt lit = expr->LitInt;
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.BasicExpr.type;
        return llvm::ConstantInt::get(
            canonicalize(gen, type), 
            lit.val, 
            type->Flags & TypeFlag_Signed
        );
    };

    case ExprKind_LitFloat: {
        Expr_LitFloat lit = expr->LitFloat;
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.BasicExpr.type;
        return llvm::ConstantFP::get(canonicalize(gen, type), lit.val);
    };

    case ExprKind_LitNil: {
        CheckerInfo info = checkerInfo[expr->id];
        Type *type = info.BasicExpr.type;
        return llvm::ConstantPointerNull::get((llvm::PointerType *)canonicalize(gen, type));
    } break;

    case ExprKind_Ident: {
        CheckerInfo info = checkerInfo[expr->id];
        Symbol *symbol = info.Ident.symbol;
        // TODO(Brett): check for return address
        return b->CreateLoad((llvm::Value *)symbol->backendUserdata);
    };

    case ExprKind_Unary: {
        return emitUnaryExpr(gen, b, checkerInfo, expr);
    };

    case ExprKind_Binary: {
        return emitBinaryExpr(gen, b, checkerInfo, expr);
    };
    }

    ASSERT(false);
    return NULL;
}

llvm::Value *emitUnaryExpr(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Expr *expr) {
     Expr_Unary unary = expr->Unary;
     llvm::Value *val = emitExpr(gen, b, checkerInfo, unary.expr);

     switch (unary.op) {
        case TK_Add:
        case TK_And:
            return val;
        case TK_Sub:
            return b->CreateNeg(val);
        case TK_Not:
        case TK_BNot:
            return b->CreateNot(val);

        case TK_Lss: {
            // TODO: check for return address
            return b->CreateLoad(val);
        };
     }

     ASSERT(false);
     return NULL;
}
llvm::Value *emitBinaryExpr(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Expr *expr) {
    Expr_Binary binary = expr->Binary;
    Type *type = checkerInfo[expr->id].BasicExpr.type;
    b32 isInt = IsInteger(type);

    llvm::Value *lhs, *rhs;
    lhs = emitExpr(gen, b, checkerInfo, binary.lhs);
    rhs = emitExpr(gen, b, checkerInfo, binary.rhs);

    switch (binary.op) {
        case TK_Add:
            return isInt ? b->CreateAdd(lhs, rhs) : b->CreateFAdd(lhs, rhs);
        case TK_Sub:
            return isInt ? b->CreateSub(lhs, rhs) : b->CreateFSub(lhs, rhs);
        case TK_Mul:
            return isInt ? b->CreateMul(lhs, rhs) : b->CreateFMul(lhs, rhs);

        case TK_And:
            return b->CreateAnd(lhs, rhs);
        case TK_Or:
            return b->CreateOr(lhs, rhs);
        case TK_Xor:
            return b->CreateXor(lhs, rhs);
        
        case TK_Div: {
            if (isInt) {
                return IsSigned(type) ? b->CreateSDiv(lhs, rhs) : b->CreateUDiv(lhs, rhs);
            } else {
                return b->CreateFDiv(lhs, rhs);
            }
        };

        case TK_Rem: {
            if (isInt) {
                return IsSigned(type) ? b->CreateSRem(lhs, rhs) : b->CreateSRem(lhs, rhs);
            } else {
                return b->CreateFRem(lhs, rhs);
            }
        };

        case TK_Land: {
            llvm::Value *x = b->CreateAnd(lhs, rhs);
            return b->CreateTruncOrBitCast(x, canonicalize(gen, BoolType));
        };

        case TK_Lor: {
            llvm::Value *x = b->CreateOr(lhs, rhs);
            return b->CreateTruncOrBitCast(x, canonicalize(gen, BoolType));
        };

        case TK_Shl: {
            return b->CreateShl(lhs, rhs);
        };

        case TK_Shr: {
            return IsSigned(type) ? b->CreateAShr(lhs, rhs) : b->CreateLShr(lhs, rhs);
        };

        case TK_Lss: {
            if (isInt) {
                return IsSigned(type) ? b->CreateICmpSLT(lhs, rhs) : b->CreateICmpULT(lhs, rhs);
            } else {
                return b->CreateFCmpOLT(lhs, rhs);
            }
        };

        case TK_Gtr: {
            if (isInt) {
                return IsSigned(type) ? b->CreateICmpSGT(lhs, rhs) : b->CreateICmpUGT(lhs, rhs);
            } else {
                return b->CreateFCmpOGT(lhs, rhs);
            }
        };

        case TK_Leq: {
            if (isInt) {
                return IsSigned(type) ? b->CreateICmpSLE(lhs, rhs) : b->CreateICmpULE(lhs, rhs);
            } else {
                return b->CreateFCmpOLE(lhs, rhs);
            }
        };

        case TK_Geq: {
            if (isInt) {
                return IsSigned(type) ? b->CreateICmpSGE(lhs, rhs) : b->CreateICmpUGE(lhs, rhs);
            } else {
                return b->CreateFCmpOGE(lhs, rhs);
            }
        };

        case TK_Eql: {
            return isInt ? b->CreateICmpEQ(lhs, rhs) : b->CreateFCmpOEQ(lhs, rhs);
        };

        case TK_Neq: {
            return isInt ? b->CreateICmpNE(lhs, rhs) : b->CreateFCmpONE(lhs, rhs);
        };
    }

    ASSERT(false);
    return NULL;
}

void emitStmt(LLVMGen *gen, llvm::IRBuilder<> *b, DynamicArray(CheckerInfo) checkerInfo, Stmt *stmt, bool isGlobal) {
    switch (stmt->kind) {
    case StmtDeclKind_Constant: {
        CheckerInfo info = checkerInfo[stmt->id];
        Decl_Constant decl = stmt->Constant;
        Symbol *symbol = info.Constant.symbol;

        if (symbol->type->kind == TypeKind_Function) {
            UNIMPLEMENTED();
        } else {
            llvm::Type *type = canonicalize(gen, symbol->type);

            llvm::GlobalVariable *global = new llvm::GlobalVariable(
                *gen->m,
                type,
                /*isConstant:*/true,
                isGlobal ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::CommonLinkage,
                0,
                symbol->name
            );

            symbol->backendUserdata = global;

            debugPos(gen, b, stmt->start);

            llvm::Value *value = emitExpr(gen, b, checkerInfo, decl.values[0]);
            global->setInitializer((llvm::Constant *)value);
        }
    } break;

    case StmtDeclKind_Variable: {
        CheckerInfo info = checkerInfo[stmt->id];
        DynamicArray(Symbol *) symbols = info.Variable.symbols;
        Decl_Variable var = stmt->Variable;

        For (symbols) {
            Symbol *symbol = symbols[i];
            llvm::Type *type = canonicalize(gen, symbol->type);
            llvm::AllocaInst *alloca = createEntryBlockAlloca(gen->currentFunc, type, symbol->name);

            symbol->backendUserdata = alloca;

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
        emitStmt(gen, &b, p->checkerInfo, p->stmts[i], true);
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

    if (FlagVerbose) {
        printf("Target: %s\n", targetTriple.c_str());
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

void debugPos(LLVMGen *gen, llvm::IRBuilder<> *b, Position pos) {
    if (!FlagDebug) { return; }
    b->SetCurrentDebugLocation(llvm::DebugLoc::get(pos.line, pos.column, gen->d->scope));
}

#pragma clang diagnostic pop
