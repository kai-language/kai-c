
#include "all.h"
#include "arena.h"
#include "package.h"
#include "ast.h"
#include "queue.h"
#include "package.h"
#include "compiler.h"
#include "llvm_wrap.hpp"

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

#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#pragma clang diagnostic pop

using namespace llvm;

typedef struct DebugContext DebugContext;
struct DebugContext {
    DIType *i8;
    DIType *i16;
    DIType *i32;
    DIType *i64;
    DIType *u8;
    DIType *u16;
    DIType *u32;
    DIType *u64;
    DIType *f32;
    DIType *f64;
    DIFile *file;
    DIScope *scope;
    DIBuilder *builder;
    DICompileUnit *unit;
};

struct IRFunctionArgs {
    Function *fn;
    Function::arg_iterator iter;
};

struct IRContext {
    LLVMContext context;
    Module *module;
    IRBuilder<> builder;

    DebugContext dbg;

    Type *ty_i1;
    Type *ty_i8;
    Type *ty_i16;
    Type *ty_i32;
    Type *ty_i64;
    Type *ty_u8;
    Type *ty_u16;
    Type *ty_u32;
    Type *ty_u64;
    Type *ty_f32;
    Type *ty_f64;

    Value *sym_true;
    Value *sym_false;

    IRFunctionArgs current_args;

    // constructor?
    IRContext(const char *path) : builder(context) {
        module = new Module(path, context);

        dbg.builder = new DIBuilder(*module);
        {
            using namespace dwarf;
            dbg.i8  = dbg.builder->createBasicType("i8",   8, DW_ATE_signed);
            dbg.i16 = dbg.builder->createBasicType("i16", 16, DW_ATE_signed);
            dbg.i32 = dbg.builder->createBasicType("i32", 32, DW_ATE_signed);
            dbg.i64 = dbg.builder->createBasicType("i64", 64, DW_ATE_signed);
            dbg.u8  = dbg.builder->createBasicType("u8",   8, DW_ATE_unsigned);
            dbg.u16 = dbg.builder->createBasicType("u16", 16, DW_ATE_unsigned);
            dbg.u32 = dbg.builder->createBasicType("u32", 32, DW_ATE_unsigned);
            dbg.u64 = dbg.builder->createBasicType("u64", 64, DW_ATE_unsigned);
            dbg.f32 = dbg.builder->createBasicType("f32", 32, DW_ATE_float);
            dbg.f64 = dbg.builder->createBasicType("f64", 64, DW_ATE_float);

            ty_i1  = IntegerType::getInt1Ty(context);
            ty_i8  = IntegerType::get(context, 8);
            ty_u8  = IntegerType::get(context, 8);
            ty_i16 = IntegerType::get(context, 16);
            ty_u16 = IntegerType::get(context, 16);
            ty_i32 = IntegerType::get(context, 32);
            ty_u32 = IntegerType::get(context, 32);
            ty_i64 = IntegerType::get(context, 64);
            ty_u64 = IntegerType::get(context, 64);
        }

        sym_true  = ConstantInt::getTrue(context);
        sym_false = ConstantInt::getFalse(context);
    }
};

void setupTarget() {
    static b32 initialized = false;
    if (initialized) return;

    // TODO: Initialize only for the targets we are outputting.
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86AsmPrinter();
    initialized = true;
}

IRContext *llvm_backend_init(Package *pkg) {
    IRContext *ctx = new IRContext(pkg->path);

    setupTarget();

    std::string error;
    Triple triple = Triple(sys::getDefaultTargetTriple());
    if (triple.getOS() == Triple::Darwin) triple.setOS(Triple::MacOSX);

    auto target = TargetRegistry::lookupTarget(triple.str(), error);
    if (!target) {
        errs() << error;
        return nullptr;
    }

    verbose("Target: %s\n", triple.str().c_str());

    const char *cpu = "generic";
    const char *features = "";

    TargetOptions opt;
    TargetMachine *tm = target->createTargetMachine(triple.str(), cpu, features, opt, None);

    // TODO: Only on unoptimized builds
    tm->setO0WantsFastISel(true);

    DataLayout dl = tm->createDataLayout();

    ctx->module->setTargetTriple(triple.str());
    ctx->module->setDataLayout(dl);
    ctx->module->getOrInsertModuleFlagsMetadata();
    ctx->module->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    ctx->module->addModuleFlag(Module::Warning, "Dwarf Version", 2);
    // See https://llvm.org/docs/LangRef.html#c-type-width-module-flags-metadata
    ctx->module->addModuleFlag(Module::Warning, "short_enum", 1);
    // TODO: Include details about the kai compiler version in the module metadata. eg:
    /*  !llvm.ident = !{!1}
     *  !1 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
     */

    return ctx;
}

IRType *llvm_i1(IRContext *c)  { return (IRType *) c->ty_i1; }
IRType *llvm_i8(IRContext *c)  { return (IRType *) c->ty_i8; }
IRType *llvm_i16(IRContext *c) { return (IRType *) c->ty_i16; }
IRType *llvm_i32(IRContext *c) { return (IRType *) c->ty_i32; }
IRType *llvm_i64(IRContext *c) { return (IRType *) c->ty_i64; }
IRType *llvm_u8(IRContext *c)  { return (IRType *) c->ty_u8; }
IRType *llvm_u16(IRContext *c) { return (IRType *) c->ty_u16; }
IRType *llvm_u32(IRContext *c) { return (IRType *) c->ty_u32; }
IRType *llvm_u64(IRContext *c) { return (IRType *) c->ty_u64; }
IRType *llvm_f32(IRContext *c) { return (IRType *) c->ty_f32; }
IRType *llvm_f64(IRContext *c) { return (IRType *) c->ty_f64; }

IRType *llvm_void(IRContext *c) {
    return (IRType *) Type::getVoidTy(c->context);
}

IRType *llvm_integer(IRContext *c, u64 bits) {
    return (IRType *) IntegerType::get(c->context, (u32) bits);
}

IRType *llvm_float(IRContext *c, u64 bits) {
    switch (bits) {
        case 16:  return (IRType *) Type::getHalfTy(c->context);
        case 32:  return (IRType *) Type::getFloatTy(c->context);
        case 64:  return (IRType *) Type::getDoubleTy(c->context);
        case 80:  return (IRType *) Type::getX86_FP80Ty(c->context);
        case 128: return (IRType *) Type::getFP128Ty(c->context);
        default:  fatal("Unsupported float size in backend");
    }
}

IRType *llvm_pointer(IRContext *c, IRType *base) {
    return (IRType *) PointerType::get((Type *) base, 0);
}

IRType *llvm_array(IRContext *c, IRType *base, u64 length) {
    return (IRType *) ArrayType::get((Type *) base, length);
}

IRType *llvm_function(IRContext *c, IRType **params, IRType *result, bool vargs) {
    auto ref = ArrayRef<Type *>((Type **)params, arrlen(params));
    return (IRType *) FunctionType::get((Type *) result, ref, vargs);
}

IRFunctionArgs *llvm_function_args_start(IRContext *c, IRFunction *fn) {
    Function *func = (Function *) fn;
    c->current_args = {func, func->arg_begin()};
    return &c->current_args;
}

void llvm_function_args_end(IRContext *c, IRFunctionArgs *args) {
    args->fn->arg_end();
}

void llvm_param_set_name(IRContext *c, IRFunctionArgs *args) {
     
}

IRType *llvm_struct(IRContext *c, IRType **elements) {
    auto ref = ArrayRef<Type *>((Type **) elements, arrlen(elements));
    return (IRType *) StructType::get(c->context, ref);
}

void llvm_push_lexical_block(IRContext *c, u32 line, u32 column) {

}

IRValue *llvm_constant_null(IRContext *c, IRType *type) {
    return (IRValue *) ConstantPointerNull::get((PointerType *) type);
}

IRValue *llvm_constant_float(IRContext *c, IRType *type, f64 val) {
    return (IRValue *) ConstantFP::get((Type *) type, val);
}

IRValue *llvm_constant_integer(IRContext *c, IRType *type, u64 val, bool is_signed) {
    return (IRValue *) ConstantInt::get((Type *) type, val, is_signed);
}

IRValue *llvm_global_string_pointer(IRContext *c, const char *string, u32 len) {
    auto ref = StringRef(string, len);
    return (IRValue *) c->builder.CreateGlobalStringPtr(ref);
}

IRValue *llvm_create_load(IRContext *c, IRValue *p, u32 align) {
    return (IRValue *) c->builder.CreateAlignedLoad((Value *) p, align);
}

IRValue *llvm_create_trunc_or_bitcast(IRContext *c, IRValue *val, IRType *type) {
    return (IRValue *) c->builder.CreateTruncOrBitCast((Value *) val, (Type *) type);
}

IRValue *llvm_create_ptr_to_int(IRContext *c, IRValue *v, IRType *ty) {
    return (IRValue *) c->builder.CreatePtrToInt((Value *) v, (Type *) ty);
}

IRValue *llvm_create_select(IRContext *c, IRValue *cond, IRValue *pass, IRValue *fail) {
    return (IRValue *) c->builder.CreateSelect((Value *) cond, (Value *) pass, (Value *) fail);
}

IRFunction *llvm_create_function(IRContext *c, IRType *ty, const char *name) {
    FunctionType *type = (FunctionType *) ty;
    Function::LinkageTypes linkage = Function::LinkageTypes::ExternalLinkage;
    return (IRFunction *) Function::Create(type, linkage, name ?: "", c->module);
}

IRBlock *llvm_create_block(IRContext *c, const char *name, IRFunction *fn) {
    return (IRBlock *) BasicBlock::Create(c->context, name, (Function *) fn);
}

void llvm_set_insert_block(IRContext *c, IRBlock *block) {
    c->builder.SetInsertPoint((BasicBlock *) block);
}

void llvm_set_insert_instr(IRContext *c, IRValue *instr) {
    c->builder.SetInsertPoint((Instruction *) instr);
}

IRValue *llvm_create_neg(IRContext *c, IRValue *v) {
    return (IRValue *) c->builder.CreateNeg((Value *) v);
}

IRValue *llvm_create_fneg(IRContext *c, IRValue *v) {
    return (IRValue *) c->builder.CreateFNeg((Value *) v);
}

IRValue *llvm_create_not(IRContext *c, IRValue *v) {
    return (IRValue *) c->builder.CreateNot((Value *) v);
}

IRValue *llvm_create_add(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateAdd((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fadd(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFAdd((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_sub(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateSub((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fsub(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFSub((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_mul(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateMul((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fmul(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFMul((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_sdiv(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateSDiv((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_udiv(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateUDiv((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fdiv(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFDiv((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_srem(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateSRem((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_urem(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateURem((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_frem(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFRem((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_and(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateAnd((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_or (IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateOr((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_xor(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateXor((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_ashr(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateAShr((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_lshr(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateLShr((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_shl(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateShl((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_slt(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpSLT((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_ult(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpULT((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_sgt(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpSGT((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_ugt(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpUGT((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_sle(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpSLE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_ule(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpULE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_sge(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpSGE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_uge(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpUGE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fcmp_olt(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFCmpOLT((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fcmp_ogt(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFCmpOGT((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fcmp_ole(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFCmpOLE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fcmp_oge(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFCmpOGE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_eq(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpEQ((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_icmp_ne(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateICmpNE((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fcmp_eq(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFCmpOEQ((Value *) lhs, (Value *) rhs);
}

IRValue *llvm_create_fcmp_ne(IRContext *c, IRValue *lhs, IRValue *rhs) {
    return (IRValue *) c->builder.CreateFCmpONE((Value *) lhs, (Value *) rhs);
}

// MARK: - debug

void llvm_debug_set_pos(IRContext *c, u32 line, u32 col) {
    if (!compiler.flags.debug) return;
    DebugLoc loc = DebugLoc::get(line, col, c->dbg.scope);
    c->builder.SetCurrentDebugLocation(loc);
}

void llvm_debug_unset_pos(IRContext *c) {
    if (!compiler.flags.debug) return;
    DebugLoc loc = DebugLoc();
    c->builder.SetCurrentDebugLocation(loc);
}

DBGType *llvm_debug_void(IRContext *c) {
    return nullptr;
}

DBGType *llvm_debug_type_function(IRContext *c, DBGType **params, DBGType *result,
                                  u32 num_params)
{
    std::vector<Metadata *> param_types;
    for (int i = 0; i < num_params; i++) param_types.push_back((DIType *) params[i]);
    DITypeRefArray p_types = c->dbg.builder->getOrCreateTypeArray(param_types);
    return (DBGType *) c->dbg.builder->createSubroutineType(p_types);
}

DBGType *llvm_debug_type_integer(IRContext *c, bool is_signed, u32 size) {
    using namespace dwarf;
    switch (size) {
        case 8:  return (DBGType *) (is_signed ? c->dbg.i8  : c->dbg.u8);
        case 16: return (DBGType *) (is_signed ? c->dbg.i16 : c->dbg.u16);
        case 32: return (DBGType *) (is_signed ? c->dbg.i32 : c->dbg.u32);
        case 64: return (DBGType *) (is_signed ? c->dbg.i64 : c->dbg.u64);
        default:
            char name[1024] = {0};
            snprintf(name, sizeof name, "%c%u", is_signed ? 'i' : 'u', size);
            return (DBGType *) c->dbg.builder->createBasicType(name, size, is_signed ? DW_ATE_signed : DW_ATE_unsigned);
    }
}

void llvm_debug_function(IRContext *c, IRFunction *fn, DBGType *ty, const char *name, u32 line) {
    if (!compiler.flags.debug) return;
    Function *func = (Function *) fn;
    DISubprogram *sp =
        c->dbg.builder->createFunction(c->dbg.scope, name, func->getName(), c->dbg.file, line,
                                       (DISubroutineType *) ty, line);

    func->setSubprogram(sp);
    // TODO: Investigate DISPFlags for improved debugging symbols
}

bool llvm_validate(IRContext *context) {
    return !verifyModule(*context->module);
}
