
#include "all.h" // stb_ds needs to be imported as C++
extern "C" {
#include "arena.h"
#include "package.h"
#include "checker.h"
#include "ast.h"
#include "types.h"
#include "queue.h"
#include "package.h"
#include "compiler.h"
}
#include "llvm.hpp"

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
#include <llvm/IR/LegacyPassManager.h>

#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/Timer.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>


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
    DIScope **scopes; // arr
    DIBuilder *builder;
    DICompileUnit *unit;
    Source *source_file;
};

typedef struct BuiltinTypes BuiltinTypes;
struct BuiltinTypes {
    Type *i1;
    union {
        Type *i8;
        Type *u8;
    };
    union {
        Type *i16;
        Type *u16;
    };
    union {
        Type *i32;
        Type *u32;
    };
    union {
        Type *i64;
        Type *u64;
    };
    Type *f32;
    Type *f64;
    Type *intptr;
    PointerType *rawptr;
};

typedef struct BuiltinSymbols BuiltinSymbols;
struct BuiltinSymbols {
    Value *True;
    Value *False;
};

struct IRFunction {
    Function *function;
    BasicBlock *entry_block;
    BasicBlock *return_block;
    AllocaInst *last_entry_alloca;
    AllocaInst *result_value;

    BasicBlock **defer_blocks;
    BasicBlock **loop_cond_blocks; // for, continue
    BasicBlock **post_blocks; // for & switch, breaks
    BasicBlock **next_cases; // switch, fallthroughs
};

struct IRContext {
    Package *package;
    LLVMContext &context;
    Module *module;
    TargetMachine *target;
    DataLayout data_layout;
    IRBuilder<> builder;

    DebugContext dbg;

    IRFunction *fn; // arr
    Sym **symbols; // arr
    bool return_address;

    BuiltinTypes ty;
    BuiltinSymbols sym;

    IRContext(IRContext *prev, Package *new_package) :
        context(prev->context),
        data_layout(prev->data_layout),
        builder(prev->context)
    {
        package = new_package;
        module = prev->module;
        target = prev->target;
        ty = prev->ty;
        sym = prev->sym;
        dbg = prev->dbg;
        dbg.scopes = NULL;
        fn = NULL;
        symbols = NULL;
        return_address = false;
        arrpush(dbg.scopes, prev->dbg.unit); // Everything in 1 CU
    }

    IRContext(Package *package, TargetMachine *tm, DataLayout dl, LLVMContext &context) :
        context(context), data_layout(dl), builder(context)
    {
        this->package = package;
        module = new Module(package->path, context);
        target = tm;
        data_layout = dl;
        dbg.builder = new DIBuilder(*module);
        symbols = NULL;
        fn = NULL;
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
            dbg.scopes = NULL;

            DIFile *package_file = dbg.builder->createFile(package->path, "");

            dbg.unit = dbg.builder->createCompileUnit(
                DW_LANG_C, package_file, "Kai",
                /*isOptimized*/ false, /*flags*/ "", /*RuntimeVersion*/ 0);
            arrpush(dbg.scopes, dbg.unit);

            ty.i1  = IntegerType::getInt1Ty(context);
            ty.i8  = IntegerType::get(context, 8);
            ty.u8  = IntegerType::get(context, 8);
            ty.i16 = IntegerType::get(context, 16);
            ty.u16 = IntegerType::get(context, 16);
            ty.i32 = IntegerType::get(context, 32);
            ty.u32 = IntegerType::get(context, 32);
            ty.i64 = IntegerType::get(context, 64);
            ty.u64 = IntegerType::get(context, 64);
            ty.intptr = IntegerType::get(context, target->getPointerSize(0));
            ty.rawptr = PointerType::get(ty.u8, 0);
        }

        sym.True  = ConstantInt::getTrue(context);
        sym.False = ConstantInt::getFalse(context);
    }
};

#define bitstobytes(bits) ((bits) + 7) / 8

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

IRContext *llvm_create_context(Package *pkg) {
    LLVMContext *context = new LLVMContext();

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

    IRContext *self = new IRContext(pkg, tm, dl, *context);

    self->module->setTargetTriple(triple.str());
    self->module->setDataLayout(dl);
    self->module->getOrInsertModuleFlagsMetadata();
    self->module->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    self->module->addModuleFlag(Module::Warning, "Dwarf Version", 2);
    // See https://llvm.org/docs/LangRef.html#c-type-width-module-flags-metadata
    self->module->addModuleFlag(Module::Warning, "short_enum", 1);
    // TODO: Include details about the kai compiler version in the module metadata. eg:
    /*  !llvm.ident = !{!1}
     *  !1 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
     */

    return self;
}

Type *llvm_void(IRContext *c) {
    return Type::getVoidTy(c->context);
}

Type *llvm_integer(IRContext *c, u64 bits) {
    return IntegerType::get(c->context, (u32) bits);
}

Type *llvm_float(IRContext *c, u64 bits) {
    switch (bits) {
        case 16:  return Type::getHalfTy(c->context);
        case 32:  return Type::getFloatTy(c->context);
        case 64:  return Type::getDoubleTy(c->context);
        case 80:  return Type::getX86_FP80Ty(c->context);
        case 128: return Type::getFP128Ty(c->context);
        default:  fatal("Unsupported float size in backend");
    }
}

Type *llvm_function(IRContext *c, Type **params, Type *result, bool vargs) {
    auto ref = ArrayRef<Type *>((Type **)params, arrlen(params));
    return FunctionType::get(result, ref, vargs);
}

Type *llvm_struct(IRContext *c, Type **elements) {
    auto ref = ArrayRef<Type *>((Type **) elements, arrlen(elements));
    return StructType::get(c->context, ref);
}

Value *llvm_constant_null(IRContext *c, Type *type) {
    return ConstantPointerNull::get((PointerType *) type);
}

// MARK: - debug

void set_debug_pos(IRContext *self, Range range) {
    if (!compiler.flags.debug) return;
    PosInfo pos = package_posinfo(self->package, range.start);

    if (pos.source != self->dbg.source_file) {
        DIFile *file = self->dbg.builder->createFile(self->package->path, pos.source->filename);
        arrsetlen(self->dbg.scopes, 1); // TODO: Do we need to preserve the ordering?
        arrpush(self->dbg.scopes, file);
    }

    DebugLoc loc = DebugLoc::get(pos.line, pos.column, arrlast(self->dbg.scopes));
    self->builder.SetCurrentDebugLocation(loc);
}

void llvm_debug_unset_pos(IRContext *c) {
    if (!compiler.flags.debug) return;
    DebugLoc loc = DebugLoc();
    c->builder.SetCurrentDebugLocation(loc);
}

DIType *llvm_debug_type_function(IRContext *c, DIType **params, DIType *result,
                                  u32 num_params)
{
    std::vector<Metadata *> param_types;
    for (int i = 0; i < num_params; i++) param_types.push_back((DIType *) params[i]);
    DITypeRefArray p_types = c->dbg.builder->getOrCreateTypeArray(param_types);
    return c->dbg.builder->createSubroutineType(p_types);
}

DIType *llvm_debug_type_integer(IRContext *c, bool is_signed, u32 size) {
    using namespace dwarf;
    switch (size) {
        case 1:  return is_signed ? c->dbg.i8  : c->dbg.u8;
        case 2: return is_signed ? c->dbg.i16 : c->dbg.u16;
        case 4: return is_signed ? c->dbg.i32 : c->dbg.u32;
        case 8: return is_signed ? c->dbg.i64 : c->dbg.u64;
        default:
            char name[1024] = {0};
            snprintf(name, sizeof name, "%c%u", is_signed ? 'i' : 'u', size * 8);
            return c->dbg.builder->createBasicType(
                name, size, is_signed ? DW_ATE_signed : DW_ATE_unsigned);
    }
}

bool llvm_validate(IRContext *context) {
    bool broken = verifyModule(*context->module);
    if (broken) {
        context->module->print(errs(), nullptr);
        ASSERT(false);
    }
    return !broken;
}

Type *llvm_type(IRContext *c, Ty *type, Sym *sym = nullptr) {
    switch (type->kind) {
        case TYPE_INVALID:
        case TYPE_COMPLETING: fatal("Invalid type in backend");
        case TYPE_VOID:       return llvm_void(c);
        case TYPE_BOOL:       return llvm_integer(c, 1);
        case TYPE_ENUM:    // fallthrough
        case TYPE_INT:        return llvm_integer(c, type->size * 8);
        case TYPE_FLOAT:      return llvm_float(c, type->size * 8);
        case TYPE_PTR:        return PointerType::get(llvm_type(c, type->tptr.base), 0);
        case TYPE_FUNC: {
            Type **params = NULL;
            i64 num_params = arrlen(type->tfunc.params);
            if (type->flags&FUNC_CVARGS) num_params -= 1;
            arrsetcap(params, num_params);
            for (i64 i = 0; i < num_params; i++) {
                Type *param = llvm_type(c, type->tfunc.params[i]);
                arrput(params, param);
            }
            Type *result;
            if (type->tfunc.result->kind == TYPE_VOID) {
                result = llvm_void(c);
            } else if (arrlen(type->tfunc.result->taggregate.fields) == 1) {
                result = llvm_type(c, type->tfunc.result->taggregate.fields->type);
            } else {
                result = llvm_type(c, type->tfunc.result);
            }
            Type *fn = llvm_function(c, params, result, (type->flags&FUNC_CVARGS) != 0);
            arrfree(params);
            return fn;
        }
        case TYPE_ARRAY: {
            Type *base = llvm_type(c, type->tarray.eltype);
            return ArrayType::get(base, type->tarray.length);
        }
        case TYPE_SLICE:      return NULL;
        case TYPE_STRUCT: {
            Type **elements = NULL;
            arrsetcap(elements, arrlen(type->taggregate.fields));
            for (int i = 0; i < arrlen(type->taggregate.fields); i++) {
                Type *element = llvm_type(c, type->taggregate.fields[i].type);
                arrput(elements, element);
            }
            ArrayRef<Type *> ref = ArrayRef<Type *>((Type **) elements, arrlen(elements));
            StructType *type = StructType::get(c->context, ref);
            arrfree(elements);
            if (sym) {
                type->setName(sym->external_name ?: sym->name);
                sym->userdata = type;
            }
            return type;
        }
        case TYPE_UNION: {
            Type *data = llvm_integer(c, type->size);
            Type **elements = NULL;
            arrput(elements, data);
            ArrayRef<Type *> ref = ArrayRef<Type *>((Type **) elements, arrlen(elements));
            StructType *type = StructType::get(c->context, ref);
            arrfree(elements);
            return type;
        }
        case TYPE_ANY: fatal("Unimplemented any type");
        default:
            fatal("Unhandled type");
    }
}

DIType *llvm_debug_type(IRContext *c, Ty *type) {
    switch (type->kind) {
        case TYPE_INVALID:
        case TYPE_COMPLETING:
        case TYPE_VOID:
            return NULL;
        case TYPE_BOOL:
        case TYPE_ENUM:
        case TYPE_INT:
            return llvm_debug_type_integer(c, (type->flags&SIGNED) != 0, type->size);
        case TYPE_FLOAT:
        case TYPE_PTR:
        case TYPE_FUNC: {
            DIType **params = NULL;
            for (i64 i = 0; i < arrlen(type->tfunc.params); i++) {
                DIType *param = llvm_debug_type(c, type->tfunc.params[i]);
                arrput(params, param);
            }
            DIType *result;
            if (type->tfunc.result->kind == TYPE_VOID) {
                result = NULL; // Void type?
            } else if (arrlen(type->tfunc.result->taggregate.fields) == 1) {
                result = llvm_debug_type(c, type->tfunc.result->taggregate.fields->type);
            } else {
                result = llvm_debug_type(c, type->tfunc.result);
            }
            return llvm_debug_type_function(c, params, result, (u32) arrlen(params));
        }
        case TYPE_ARRAY:
        case TYPE_SLICE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ANY:
        default:
            fatal("Unhandled type");
    }
}

AllocaInst *emit_entry_alloca(IRContext *self, Type *type, const char *name, u32 alignment_bytes) {
    IRFunction *fn = &arrlast(self->fn);
    IRBuilder<> b(fn->entry_block);
    if (fn->last_entry_alloca)
        if (Instruction *next = fn->last_entry_alloca->getNextNode())
            b.SetInsertPoint(next);
    AllocaInst *alloca = b.CreateAlloca(type, 0, name ?: "");
    fn->last_entry_alloca = alloca;
    if (alignment_bytes) alloca->setAlignment(alignment_bytes);
    return alloca;
}

Value *enter_struct_pointer_for_coerced_access(
    IRContext *self, Value *src_ptr, StructType *src_struct_type, u64 dst_size)
{
    // We can't dive into a zero-element struct.
    if (src_struct_type->getNumElements() == 0) return src_ptr;

    Type *first_el = src_struct_type->getElementType(0);

    // If the first elt is at least as large as what we're looking for, or if the
    // first element is the same size as the whole struct, we can enter it. The
    // comparison must be made on the store size and not the alloca size. Using
    // the alloca size may overstate the size of the load.
    uint64_t first_el_size = self->data_layout.getTypeStoreSize(first_el);
    if (first_el_size < dst_size &&
        first_el_size < self->data_layout.getTypeStoreSize(src_struct_type))
        return src_ptr;

    // GEP into the first element.
    src_ptr = self->builder.CreateStructGEP(src_ptr, 0, "coerce.dive");

    // If the first element is a struct, recurse.
    Type *src_ty = src_ptr->getType()->getPointerElementType();
    if (StructType *src_struct_type = dyn_cast<StructType>(src_ty))
        return enter_struct_pointer_for_coerced_access(self, src_ptr, src_struct_type, dst_size);

    return src_ptr;
}

// MARK: - conversions

Value *coerce_int_or_ptr(IRContext *self, Value *val, Type *ty) {
    if (val->getType() == ty) return val;

    if (isa<PointerType>(val->getType())) {
        if (isa<PointerType>(ty)) return self->builder.CreateBitCast(val, ty, "coerce.val");
        val = self->builder.CreatePtrToInt(val, self->ty.intptr, "coerce.val.pi");
    }
    Type *dest_int_ty = ty;
    if (isa<PointerType>(dest_int_ty))
        dest_int_ty = self->ty.intptr;

    if (val->getType() != dest_int_ty) {
        DataLayout dl = self->data_layout;
        if (dl.isBigEndian()) {
            // Preserve the high bits on big-endian targets.
            // That is what memory coercion does.
            uint64_t src_size = dl.getTypeSizeInBits(val->getType());
            uint64_t dst_size = dl.getTypeSizeInBits(dest_int_ty);

            if (src_size > dst_size) {
                val = self->builder.CreateLShr(val, src_size - dst_size, "coerce.highbits");
                val = self->builder.CreateTrunc(val, dest_int_ty, "coerce.val.ii");
            } else {
                val = self->builder.CreateZExt(val, dest_int_ty, "coerce.val.ii");
                val = self->builder.CreateShl(val, dst_size - src_size, "coerce.highbits");
            }
        } else {
            // Little-endian targets preserve the low bits. No shifts required.
            val = self->builder.CreateIntCast(val, dest_int_ty, false, "coerce.val.ii");
        }
    }

    if (isa<PointerType>(ty)) val = self->builder.CreateIntToPtr(val, ty, "coerce.val.ip");
    return val;
}

Value *emit_coerced_load(IRContext *self, Value *src, Ty *ty) {
    Type *src_ty = src->getType()->getPointerElementType();
    Type *dst_ty = llvm_type(self, ty);

    // If SrcTy and Ty are the same, just do a load.
    if (src_ty == dst_ty) return self->builder.CreateAlignedLoad(src, ty->align);
    u64 dst_size = self->data_layout.getTypeAllocSize(dst_ty);
    if (StructType *src_struct_ty = dyn_cast<StructType>(src_ty)) {
        src = enter_struct_pointer_for_coerced_access(self, src, src_struct_ty, dst_size);
        src_ty = src->getType()->getPointerElementType();
    }
    u64 src_size = self->data_layout.getTypeAllocSize(src_ty);

    // If the source and destination are integer or pointer types, just do an
    // extension or truncation to the desired type.
    if ((isa<IntegerType>(dst_ty) || isa<PointerType>(dst_ty)) &&
        (isa<IntegerType>(src_ty) || isa<PointerType>(src_ty))) {
        Value *load = self->builder.CreateLoad(src);
        return coerce_int_or_ptr(self, load, dst_ty);
    }

    // If load is legal, just bitcast the src pointer.
    if (src_size >= dst_size) {
        // Generally SrcSize is never greater than DstSize, since this means we are
        // losing bits. However, this can happen in cases where the structure has
        // additional padding, for example due to a user specified alignment.
        //
        // FIXME: Assert that we aren't truncating non-padding bits when have access
        // to that information.
        src = self->builder.CreateBitCast(
                                          src, dst_ty->getPointerTo(src_ty->getPointerAddressSpace()));
        return self->builder.CreateLoad(src);
    }

    // Otherwise do coercion through memory. This is stupid, but simple.
    u32 align_bytes = src->getPointerAlignment(self->data_layout);
    AllocaInst *tmp = emit_entry_alloca(self, src_ty, "coerce.alloca", align_bytes);
    Value *casted = self->builder.CreateBitCast(tmp, self->ty.rawptr);
    Value *src_casted = self->builder.CreateBitCast(src, self->ty.rawptr);
    self->builder.CreateMemCpy(casted, 0, src_casted, 0, src_size);
    return self->builder.CreateLoad(tmp);
}

Value *emit_coerced_load(IRContext *self, Value *src, Expr *expr) {
    Ty *dst = hmget(self->package->operands, expr).type;
    return emit_coerced_load(self, src, dst);
}

Value *emit_coercion_and_or_load(IRContext *self, Value *src, Expr *expr) {
    Ty *dst = hmget(self->package->operands, expr).type;
    Type *src_ty = src->getType();
    Type *dst_ty = llvm_type(self, dst);
    if (expr->kind == EXPR_CALL && isa<StructType>(dst_ty) &&
        dst_ty->getStructNumElements() == 1 && dst_ty->getStructElementType(0) == src_ty) {
        return src; // ensure calls return single elements correctly
    }
    if (!isa<PointerType>(src_ty)) { // coerce, but no load

        switch (src_ty->getTypeID()) {
            case Type::VoidTyID:
                return src;
            case Type::HalfTyID:
            case Type::FloatTyID:
            case Type::DoubleTyID:
            case Type::X86_FP80TyID:
            case Type::FP128TyID:
            case Type::PPC_FP128TyID:
                switch (dst_ty->getTypeID()) {
                    case Type::HalfTyID:
                    case Type::FloatTyID:
                    case Type::DoubleTyID:
                    case Type::X86_FP80TyID:
                    case Type::FP128TyID:
                    case Type::PPC_FP128TyID: {
                        u64 src_size = self->data_layout.getTypeStoreSize(src_ty);
                        u64 dst_size = self->data_layout.getTypeStoreSize(dst_ty);
                        if (src_size < dst_size)
                            src = self->builder.CreateFPExt(src, dst_ty);
                        else if (src_size > dst_size)
                            src = self->builder.CreateFPTrunc(src, dst_ty);
                        return src;
                    }
                    case Type::IntegerTyID: {
                        if (is_signed(dst)) src = self->builder.CreateFPToSI(src, dst_ty);
                        else                src = self->builder.CreateFPToUI(src, dst_ty);
                        return src;
                    }
                    default:
                        break;
                }

            case Type::IntegerTyID: { // TODO: SExt vs ZExt for signed integers
                return self->builder.CreateZExtOrTrunc(src, dst_ty);
            }

            case Type::FunctionTyID: {
                if (isa<PointerType>(dst_ty)) {
                    return self->builder.CreateBitOrPointerCast(src, dst_ty);
                }
                return src;
            }
            case Type::StructTyID:
                return src;
            case Type::ArrayTyID:
                return src;
            case Type::PointerTyID:
                return src;
            case Type::VectorTyID:
                fatal("Unsupported type in IR");

            case Type::LabelTyID:
            case Type::MetadataTyID:
            case Type::X86_MMXTyID:
            case Type::TokenTyID:
                fatal("Unhandled coercion case");
        }
        fatal("Unhandled coercion case");
    } else if (isa<FunctionType>(src_ty->getPointerElementType())) return src;

    if (self->return_address) return src;
    return emit_coerced_load(self, src, dst);
}

Value *emit_expr(IRContext *self, Expr *expr);
void emit_decl(IRContext *self, Decl *decl);
void emit_stmt(IRContext *self, Stmt *stmt);

Value *emit_expr_nil(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    Type *type = llvm_type(self, operand.type);
    return llvm_constant_null(self, type);
}

Value *emit_expr_int(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    Type *type = llvm_type(self, operand.type);
    if (operand.type->kind == TYPE_FLOAT) {
        return ConstantFP::get(type, (f64) operand.val.u);
    }
    return ConstantInt::get(type, operand.val.u, is_signed(operand.type));
}

Value *emit_expr_float(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    Type *type = llvm_type(self, operand.type);
    return ConstantFP::get(type, operand.val.f);
}

Value *emit_expr_str(IRContext *self, Expr *expr) {
    auto ref = StringRef(expr->estr.str, expr->estr.len);
    return self->builder.CreateGlobalStringPtr(ref);
}

Value *emit_sym(IRContext *self, Package *package, Sym *sym) {
    IRContext new_context(self, package);
    new_context.dbg.source_file = package_posinfo(package, sym->decl->range.start).source;
    emit_decl(&new_context, sym->decl);
    ASSERT(sym->userdata);
    return (Value *) sym->userdata;
}

Value *emit_expr_name(IRContext *self, Expr *expr) {
    Sym *sym = hmget(self->package->symbols, expr);
    if (!sym->userdata) {
        emit_sym(self, self->package, sym);
        ASSERT(sym->userdata);
    }
    if (self->return_address) return (Value *) sym->userdata;
    return emit_coerced_load(self, (Value *) sym->userdata, expr);
}

Value *emit_expr_field(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr->efield.expr);
    if (operand.flags&PACKAGE) { // operand.val is a pointer to the package Symbol
        Sym *package_sym = (Sym *) operand.val.p;
        Sym *sym = hmget(self->package->symbols, expr->efield.name);
        if (!sym->userdata) { // TODO: Switch some of the context stuff (DIFile) etc.
            emit_sym(self, package_sym->package, sym);
        }
        if (self->return_address) return (Value *) sym->userdata;
        return emit_coerced_load(self, (Value *) sym->userdata, expr);
    }
    switch (operand.type->kind) {
        case TYPE_STRUCT:
            break;
    }
    fatal("Unhandled field base kind");
}

Value *emit_expr_compound(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_cast(IRContext *self, Expr *expr) { return NULL; }

Value *emit_expr_paren(IRContext *self, Expr *expr) {
    return emit_expr(self, expr->eparen);
}

Value *emit_expr_unary(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    bool prev_return_address = self->return_address;
    self->return_address = true;
    Value *value = emit_expr(self, expr->eunary);
    self->return_address = prev_return_address;
    set_debug_pos(self, expr->range);
    switch ((Op) expr->flags) {
        case OP_ADD:
        case OP_AND:
            return value;
        case OP_SUB:
            if (operand.type->kind == TYPE_FLOAT) {
                return self->builder.CreateFNeg(value);
            } else {
                return self->builder.CreateNeg(value);
            }
        case OP_NOT:
        case OP_BNOT:
            return self->builder.CreateNot(value);
        case OP_LSS:
            if (self->return_address) return value;
            return emit_coerced_load(self, value, operand.type);
        default:
            fatal("Unhandled unary op case %d", expr->flags);
    }
}

Value *emit_expr_binary(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    bool is_int = is_integer(operand.type);
    Type *type = llvm_type(self, operand.type);
    bool prev_return_address = self->return_address;
    self->return_address = false;
    Value *lhs = emit_expr(self, expr->ebinary.elhs);
    Value *rhs = emit_expr(self, expr->ebinary.erhs);
    self->return_address = prev_return_address;
    set_debug_pos(self, expr->range);
    IRBuilder<> b = self->builder;
    switch (expr->flags) {
        case OP_ADD: return is_int ? b.CreateAdd(lhs, rhs) : b.CreateFAdd(lhs, rhs);
        case OP_SUB: return is_int ? b.CreateSub(lhs, rhs) : b.CreateFSub(lhs, rhs);
        case OP_MUL: return is_int ? b.CreateMul(lhs, rhs) : b.CreateFMul(lhs, rhs);
        case OP_DIV:
            return is_int ? is_signed(operand.type) ?
                b.CreateSDiv(lhs, rhs) : b.CreateUDiv(lhs, rhs) : b.CreateFDiv(lhs, rhs);
        case OP_REM:
            return is_int ? is_signed(operand.type) ?
                b.CreateSDiv(lhs, rhs) : b.CreateUDiv(lhs, rhs) : b.CreateFDiv(lhs, rhs);
        case OP_AND: return b.CreateAnd(lhs, rhs);
        case OP_OR:  return b.CreateOr (lhs, rhs);
        case OP_XOR: return b.CreateXor(lhs, rhs);
        case OP_LOR: return b.CreateTruncOrBitCast(b.CreateOr(lhs, rhs), type);
        case OP_LAND: return b.CreateTruncOrBitCast(b.CreateAnd(lhs, rhs), type);
        case OP_SHR:
            return is_signed(operand.type) ? b.CreateAShr(lhs, rhs) : b.CreateLShr(lhs, rhs);
        case OP_SHL: return b.CreateShl(lhs, rhs);
        case OP_LSS:
            return is_int ? is_signed(operand.type) ?
                b.CreateICmpSLT(lhs, rhs) : b.CreateICmpULT(lhs, rhs) : b.CreateFCmpOLT(lhs, rhs);
        case OP_LEQ:
            return is_int ? is_signed(operand.type) ?
                b.CreateICmpSLE(lhs, rhs) : b.CreateICmpULE(lhs, rhs) : b.CreateFCmpOLE(lhs, rhs);
        case OP_GTR:
            return is_int ? is_signed(operand.type) ?
                b.CreateICmpSGT(lhs, rhs) : b.CreateICmpUGT(lhs, rhs) : b.CreateFCmpOGT(lhs, rhs);
        case OP_GEQ:
            return is_int ? is_signed(operand.type) ?
                b.CreateICmpSGE(lhs, rhs) : b.CreateICmpUGE(lhs, rhs) : b.CreateFCmpOGE(lhs, rhs);
        case OP_EQL: return is_int ? b.CreateICmpEQ(lhs, rhs) : b.CreateFCmpOEQ(lhs, rhs);
        case OP_NEQ: return is_int ? b.CreateICmpNE(lhs, rhs) : b.CreateFCmpONE(lhs, rhs);
        default:
            fatal("Unhandled binary op case %d", expr->flags);
    }
}

Value *emit_expr_ternary(IRContext *self, Expr *expr) {
    bool is_pointer = is_ptr(hmget(self->package->operands, expr->eternary.econd).type);
    bool prev_return_address = self->return_address;
    self->return_address = true;
    Value *cond = emit_expr(self, expr->eternary.econd);
    Value *pass = cond;
    if (expr->eternary.epass)
        pass = emit_expr(self, expr->eternary.epass);
    Value *fail = emit_expr(self, expr->eternary.efail);
    self->return_address = prev_return_address;
    if (is_pointer) {
        cond = self->builder.CreatePtrToInt(cond, self->ty.i1);
    } else {
        cond = self->builder.CreateTruncOrBitCast(cond, self->ty.i1);
    }
    return self->builder.CreateSelect(cond, pass, fail);
}

Value *emit_expr_call(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr->ecall.expr);
    Value **args = NULL;
    bool is_cvargs = (operand.type->flags&FUNC_CVARGS) != 0;
    for (i64 i = 0; i < arrlen(expr->ecall.args); i++) {
        CallArg arg = expr->ecall.args[i];
        Operand arg_operand = hmget(self->package->operands, arg.expr);
        Value *val = emit_expr(self, arg.expr);
        bool last_arg = i - 1 == arrlen(operand.type->tfunc.params);
        if (is_cvargs && last_arg) { // C ABI rules (TODO: Apply to all parameters for c calls)
            if (is_integer(arg_operand.type) && arg_operand.type->size < 4) {
                val = is_signed(arg_operand.type) ?
                    self->builder.CreateSExt(val, self->ty.i32) : self->builder.CreateZExt(val, self->ty.u32);
            } else if (is_float(arg_operand.type) && arg_operand.type->size < 8) {
                self->builder.CreateFPExt(val, self->ty.f64);
            }
        }
        arrpush(args, val);
    }
    bool prev_return_address = self->return_address;
    self->return_address = true;
    Value *fn = emit_expr(self, expr->ecall.expr);
    self->return_address = prev_return_address;
    set_debug_pos(self, expr->range);
    ArrayRef<Value *> ref = ArrayRef<Value *>(args, arrlen(args));
    return self->builder.CreateCall(fn, ref);
}

Value *emit_expr_index(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_slice(IRContext *self, Expr *expr) { return NULL; }

Function *emit_expr_func(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);

    FunctionType *type = (FunctionType *) llvm_type(self, operand.type);
    const char *name = NULL;
    if (arrlen(self->symbols)) {
        name = arrlast(self->symbols)->external_name ?: arrlast(self->symbols)->name;
    }

    Function::LinkageTypes linkage = Function::LinkageTypes::ExternalLinkage;
    Function *fn = Function::Create(type, linkage, name ?: "", self->module);
    if (compiler.flags.debug) {
        DIType *dbg_type = llvm_debug_type(self, operand.type);
        PosInfo pos = package_posinfo(self->package, expr->range.start);
        DINode::DIFlags flags = strcmp(name, "main") == 0 ?
            DISubprogram::DIFlags::FlagMainSubprogram : DINode::DIFlags::FlagZero;
        DISubprogram *sp = self->dbg.builder->createFunction(
            arrlast(self->dbg.scopes), name, fn->getName(), self->dbg.file, pos.line,
            (DISubroutineType *) dbg_type, pos.line, flags,
            DISubprogram::DISPFlags::SPFlagDefinition);
        fn->setSubprogram(sp);
        arrpush(self->dbg.scopes, sp);
    }

    BasicBlock *entry_block = BasicBlock::Create(self->context, "entry", fn);
    BasicBlock *return_block = BasicBlock::Create(self->context, "return", fn);
    {
        IRFunction function = {fn, entry_block, return_block};
        arrpush(self->fn, function);
    }
    IRFunction *function = &arrlast(self->fn);

    // Unset the location for the prologue emission (leading instructions with no
    // location in a function are considered part of the prologue and the debugger
    // will run past them when breaking on a function)
    llvm_debug_unset_pos(self);

    self->builder.SetInsertPoint(entry_block);

    if (operand.type->tfunc.result != type_void) {
        function->result_value = emit_entry_alloca(
            self, type->getReturnType(), "result", operand.type->tfunc.result->align);
    }

    Function::arg_iterator args = fn->arg_begin();
    for (i64 i = 0; i < arrlen(expr->efunc.type->efunctype.params); i++) {
        FuncParam param = expr->efunc.type->efunctype.params[i];
        Argument *arg = args++;
        arg->setName(param.name->ename);
        Sym *sym = hmget(self->package->symbols, param.name);
        Type *type = llvm_type(self, sym->type);
        AllocaInst *alloca = emit_entry_alloca(self, type, sym->name, sym->type->align);
        sym->userdata = alloca;
        self->builder.CreateAlignedStore(arg, alloca, sym->type->align);

        if (compiler.flags.debug) {
            PosInfo pos = package_posinfo(self->package, param.name->range.start);
            DILocalVariable *var = self->dbg.builder->createParameterVariable(
                arrlast(self->dbg.scopes), sym->name, (u32) i, self->dbg.file, pos.line,
                llvm_debug_type(self, sym->type));
            self->dbg.builder->insertDeclare(
                alloca, var, self->dbg.builder->createExpression(),
                DebugLoc::get(pos.line, pos.column, arrlast(self->dbg.scopes)), entry_block);
        }

    }
    fn->arg_end();

    emit_stmt(self, expr->efunc.body);
    arrpop(self->fn);

    if (!self->builder.GetInsertBlock()->getTerminator()) {
        self->builder.CreateBr(function->return_block);
    }

    // Set insert point to the return block
    self->builder.SetInsertPoint(function->return_block);
    if (function->defer_blocks) {
        function->return_block = BasicBlock::Create(self->context, "return_post_defer", fn);
        self->builder.CreateBr(function->defer_blocks[0]);
    }

    for (i64 i = 0; i < arrlen(function->defer_blocks); i++) {
        BasicBlock *block = function->defer_blocks[i];
        self->builder.SetInsertPoint(block);
        if (i < arrlen(function->defer_blocks) - 1) {
            self->builder.CreateBr(function->defer_blocks[i + 1]);
        } else {
            self->builder.CreateBr(function->return_block);
        }
    }
    if (function->defer_blocks) self->builder.SetInsertPoint(function->return_block);

    if (arrlen(operand.type->tfunc.result->taggregate.fields) == 1) {
        Ty *type = operand.type->tfunc.result->taggregate.fields->type;
        Value *return_value = self->builder.CreateAlignedLoad(function->result_value, type->align);
        self->builder.CreateRet(return_value);
    } else if (operand.type->tfunc.result->kind == TYPE_STRUCT) {
        StructType *type = dyn_cast<StructType>(fn->getReturnType());
        ASSERT(type);
        DataLayout dl = self->module->getDataLayout();
        Value *return_value = self->builder.CreateAlignedLoad(
            function->result_value, dl.getStructLayout(type)->getAlignment());
        self->builder.CreateRet(return_value);
    } else {
        ASSERT(operand.type->tfunc.result->kind == TYPE_VOID);
        self->builder.CreateRetVoid();
    }

    // Move the return block to the end, just for readability
    function->return_block->moveAfter(self->builder.GetInsertBlock());

    if (compiler.flags.debug) {
        self->dbg.builder->finalizeSubprogram(fn->getSubprogram());
        arrpop(self->dbg.scopes);
    }

    if (verifyFunction(*fn, &errs())) {
        self->module->print(errs(), nullptr);
        ASSERT(false);
    }

    return fn;
}

Value *emit_expr_functype(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_slicetype(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_array(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_pointer(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_struct(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_union(IRContext *self, Expr *expr) { return NULL; }
Value *emit_expr_enum(IRContext *self, Expr *expr) { return NULL; }

Value *emit_expr(IRContext *self, Expr *expr) {
    Value *val;
    switch (expr->kind) {
        case EXPR_NIL:       val = emit_expr_nil(self, expr); break;
        case EXPR_INT:       val = emit_expr_int(self, expr); break;
        case EXPR_FLOAT:     val = emit_expr_float(self, expr); break;
        case EXPR_STR:       val = emit_expr_str(self, expr); break;
        case EXPR_NAME:      val = emit_expr_name(self, expr); break;
        case EXPR_COMPOUND:  val = emit_expr_compound(self, expr); break;
        case EXPR_CAST:      val = emit_expr_cast(self, expr); break;
        case EXPR_PAREN:     val = emit_expr_paren(self, expr); break;
        case EXPR_UNARY:     val = emit_expr_unary(self, expr); break;
        case EXPR_BINARY:    val = emit_expr_binary(self, expr); break;
        case EXPR_TERNARY:   val = emit_expr_ternary(self, expr); break;
        case EXPR_CALL:      val = emit_expr_call(self, expr); break;
        case EXPR_FIELD:     val = emit_expr_field(self, expr); break;
        case EXPR_INDEX:     val = emit_expr_index(self, expr); break;
        case EXPR_SLICE:     val = emit_expr_slice(self, expr); break;
        case EXPR_FUNC:      val = (Value *) emit_expr_func(self, expr); break;
        case EXPR_FUNCTYPE:  val = emit_expr_functype(self, expr); break;
        case EXPR_SLICETYPE: val = emit_expr_slicetype(self, expr); break;
        case EXPR_ARRAY:     val = emit_expr_array(self, expr); break;
        case EXPR_POINTER:   val = emit_expr_pointer(self, expr); break;
        case EXPR_STRUCT:    val = emit_expr_struct(self, expr); break;
        case EXPR_UNION:     val = emit_expr_union(self, expr); break;
        case EXPR_ENUM:      val = emit_expr_enum(self, expr); break;
        default:
            fatal("Unrecognized ExprKind %s", describe_ast_kind(expr->kind));
    }
    return emit_coercion_and_or_load(self, val, expr);
}

void emit_stmt_label(IRContext *self, Stmt *stmt) {
    Sym *sym = hmget(self->package->symbols, stmt->slabel);
    if (sym->userdata) { // Label has been previously emitted
        BasicBlock *bb = (BasicBlock *) sym->userdata;
        bb->moveAfter(self->builder.GetInsertBlock());
        self->builder.SetInsertPoint(bb);
    } else { // Label has not yet been emitted
        BasicBlock *bb = BasicBlock::Create(self->context, sym->name, arrlast(self->fn).function);
        self->builder.SetInsertPoint(bb);
        sym->userdata = bb;
    }
}

void emit_stmt_assign(IRContext *self, Stmt *stmt) {
    i64 num_lhs = arrlen(stmt->sassign.lhs);
    i64 rhs_index = 0;
    for (i64 lhs_index = 0; lhs_index < num_lhs;) {
        Expr *expr = stmt->sassign.rhs[rhs_index++];
        Value *rhs = emit_expr(self, expr);
        if (expr->kind == EXPR_CALL) {
            Operand operand = hmget(self->package->operands, expr);
            i64 num_values = arrlen(operand.type->taggregate.fields);
            if (num_values == 1) {
                bool prev_return_address = self->return_address;
                self->return_address = true;
                Value *lhs = emit_expr(self, stmt->sassign.lhs[lhs_index++]);
                self->return_address = prev_return_address;
                set_debug_pos(self, stmt->range);
                self->builder.CreateAlignedStore(rhs, lhs, operand.type->align);
            } else {
                // create some stack space to land the returned struct onto
                AllocaInst *result_address = emit_entry_alloca(self, rhs->getType(), nullptr, 0);
                self->builder.CreateStore(rhs, result_address); // TODO: Align?

                for (i64 result_index = 0; result_index < num_lhs; result_index++) {
                    Expr *lhs_expr = stmt->sassign.lhs[lhs_index++];
                    Operand lhs_operand = hmget(self->package->operands, lhs_expr);
                    bool prev_return_address = self->return_address;
                    self->return_address = true;
                    Value *lhs = emit_expr(self, lhs_expr);
                    self->return_address = prev_return_address;
                    set_debug_pos(self, stmt->range);

                    Value *addr = self->builder.CreateStructGEP(
                        rhs->getType(), result_address, (u32) result_index);
                    Value *val = self->builder.CreateLoad(addr); // TODO: coerced load

                    // TODO: Handle coercion
                    self->builder.CreateAlignedStore(val, lhs, lhs_operand.type->align);
                }
            }
        } else {
            Expr *lhs_expr = stmt->sassign.lhs[lhs_index++];
            Operand lhs_operand = hmget(self->package->operands, lhs_expr);
            bool prev_return_address = self->return_address;
            self->return_address = true;
            Value *lhs = emit_expr(self, lhs_expr);
            self->return_address = prev_return_address;
            set_debug_pos(self, stmt->range);
            self->builder.CreateAlignedStore(rhs, lhs, lhs_operand.type->align);
        }
    }
}

void emit_stmt_return(IRContext *self, Stmt *stmt) {
    i64 num_returns = arrlen(stmt->sreturn);
    Value **values = NULL;
    IRFunction fn = arrlast(self->fn);
    Type *ret_type = fn.function->getReturnType();
    for (i64 i = 0; i < num_returns; i++) {
        Expr *expr = stmt->sreturn[i];
        Type *desired_type = fn.function->getReturnType();
        if (num_returns > 1) {
            StructType *type = dyn_cast<StructType>(ret_type);
            ASSERT(type);
            desired_type = type->getElementType((u32) i);
        }
        bool prev_return_address = false;
        self->return_address = false;
        Value *val = emit_expr(self, expr);
        self->return_address = prev_return_address;
        arrpush(values, val);
    }
    llvm_debug_unset_pos(self);
    if (num_returns > 1) {
        for (u32 i = 0; i < num_returns; i++) {
            Value *el_ptr = self->builder.CreateStructGEP(ret_type, fn.result_value, i);
            Value *el = self->builder.CreateLoad(el_ptr); // TODO: coerced load?
            self->builder.CreateStore(values[i], el);
        }
    } else if (num_returns == 1) {
        self->builder.CreateStore(values[0], fn.result_value);
    }
    arrfree(values);
    set_debug_pos(self, stmt->range);
    self->builder.CreateBr(fn.return_block);
}

void emit_stmt_defer(IRContext *self, Stmt *stmt) {
    IRFunction *fn = &arrlast(self->fn);
    BasicBlock *block = BasicBlock::Create(self->context, "defer", arrlast(self->fn).function);
    arrpush(fn->defer_blocks, block);
    BasicBlock *prev = self->builder.GetInsertBlock();
    self->builder.SetInsertPoint(block);
    emit_stmt(self, stmt->sdefer);
    self->builder.SetInsertPoint(prev);
}

void emit_stmt_using(IRContext *self, Stmt *stmt) {}
void emit_stmt_goto(IRContext *self, Stmt *stmt) {
    IRFunction *fn = &arrlast(self->fn);
    BasicBlock *target;
    if (stmt->sgoto) {
        Sym *label = hmget(self->package->symbols, stmt->slabel);
        if (label->userdata) {
            target = (BasicBlock *) label->userdata;
        } else {
            target = BasicBlock::Create(self->context, label->name, fn->function);
            label->userdata = target;
        }
    } else {
        switch ((GotoKind) stmt->flags) {
            case GOTO_GOTO:
                fatal("Should always have target, and be handled above");
            case GOTO_BREAK:
                target = arrlast(fn->post_blocks);
            case GOTO_CONTINUE:
                target = arrlast(fn->loop_cond_blocks);
            case GOTO_FALLTHROUGH:
                target = arrlast(fn->next_cases);
                break;
            default:
                fatal("Unrecognized goto kind %d", stmt->flags);
        }
    }
    self->builder.CreateBr(target);
}

void emit_stmt_block(IRContext *self, Stmt *stmt) {
    if (compiler.flags.debug) {
        PosInfo pos = package_posinfo(self->package, stmt->range.start);
        DILexicalBlock *block = self->dbg.builder->createLexicalBlock(
            arrlast(self->dbg.scopes), self->dbg.file, pos.line, pos.column);
        arrpush(self->dbg.scopes, block);
    }
    for (i64 i = 0; i < arrlen(stmt->sblock); i++) {
        emit_stmt(self, stmt->sblock[i]);
    }
    if (compiler.flags.debug) {
        arrpop(self->dbg.scopes);
    }
}

void emit_stmt_if(IRContext *self, Stmt *stmt) {
    Function *fn = arrlast(self->fn).function;
    BasicBlock *pass = BasicBlock::Create(self->context, "if.pass", fn);
    BasicBlock *fail = stmt->sif.fail ? BasicBlock::Create(self->context, "if.fail", fn) : nullptr;
    BasicBlock *post = BasicBlock::Create(self->context, "if.post", fn);

    Value *cond = emit_expr(self, stmt->sif.cond);
    set_debug_pos(self, stmt->range);
    self->builder.CreateCondBr(cond, pass, fail ?: post);

    self->builder.SetInsertPoint(pass);
    emit_stmt(self, stmt->sif.pass);
    if (!pass->getTerminator()) self->builder.CreateBr(post);

    if (stmt->sif.fail) {
        self->builder.SetInsertPoint(fail);
        emit_stmt(self, stmt->sif.fail);
        if (!fail->getTerminator()) self->builder.CreateBr(post);
    }
    self->builder.SetInsertPoint(post);
}

void emit_stmt_for(IRContext *self, Stmt *stmt) {
    IRFunction *fn = &arrlast(self->fn);
    BasicBlock *body, *post, *cond, *step;
    body = post = cond = step = NULL;
    if (compiler.flags.debug) {
        PosInfo pos = package_posinfo(self->package, stmt->range.start);
        DILexicalBlock *block = self->dbg.builder->createLexicalBlock(
            arrlast(self->dbg.scopes), self->dbg.file, pos.line, pos.column);
        arrpush(self->dbg.scopes, block);
    }
    if (stmt->sfor.init) {
        set_debug_pos(self, stmt->sfor.init->range);
        emit_stmt(self, stmt->sfor.init);
    }
    if (stmt->sfor.cond) {
        cond = BasicBlock::Create(self->context, "for.cond", fn->function);
        if (stmt->sfor.step) {
            step = BasicBlock::Create(self->context, "for.step", fn->function);
        }
        body = BasicBlock::Create(self->context, "for.body", fn->function);
        post = BasicBlock::Create(self->context, "for.post", fn->function);
        self->builder.CreateBr(cond ?: body);
    }
    arrpush(fn->loop_cond_blocks, cond);
    arrpush(fn->post_blocks, post);
    self->builder.SetInsertPoint(body);
    emit_stmt(self, stmt->sfor.body);
    b32 has_jump = self->builder.GetInsertBlock()->getTerminator() != NULL;
    if (stmt->sfor.step) { // for init; cond; step { ... }
        if (!has_jump) self->builder.CreateBr(step);
        self->builder.SetInsertPoint(step);
        emit_stmt(self, stmt->sfor.step);
        self->builder.CreateBr(cond);
    } else if (cond) { // for cond { ... }
        if (!has_jump) self->builder.CreateBr(cond);
    } else if (!has_jump) { // for { ... }
        self->builder.CreateBr(body);
    }
    arrpop(fn->loop_cond_blocks);
    arrpop(fn->post_blocks);
    self->builder.CreateBr(post);
    if (compiler.flags.debug) arrpop(self->dbg.scopes);
}

void emit_stmt_switch(IRContext *self, Stmt *stmt) {}

void emit_decl_var_global(IRContext *self, Decl *decl) {
    Expr *name = *decl->dvar.names;
    Expr *expr = *decl->dvar.vals;
    Sym *sym = hmget(self->package->symbols, name);
    Value *val = emit_expr(self, expr);
    Constant *init = dyn_cast<Constant>(val);
    if (!init) fatal("unimplemented non constant global variables");
    Type *type = llvm_type(self, sym->type);
    GlobalVariable *global = new GlobalVariable(
        *self->module, type, /* IsConstant */ false, GlobalValue::ExternalLinkage,
        init, sym->external_name ?: sym->name);
    global->setAlignment(sym->type->align);
    sym->userdata = global;
    if (compiler.flags.debug) {
        PosInfo pos = package_posinfo(self->package, sym->decl->range.start);
        self->dbg.builder->createGlobalVariableExpression(
            arrlast(self->dbg.scopes), sym->name, sym->external_name ?: sym->name,
            self->dbg.file, pos.line, llvm_debug_type(self, sym->type),
            /* LocalToUnit */ false, /* Expr */ NULL, /* Decl */ NULL,
            /* TemplateParams */ NULL, sym->type->align * 8);
    }
    // TODO: Complete handling of global variable intialization using function calls
}

void emit_decl_var(IRContext *self, Decl *decl) {
    bool prev_return_address = self->return_address;
    self->return_address = true;
    i64 rhs_index = 0;
    for (i64 lhs_index = 0; lhs_index < arrlen(decl->dvar.names);) {
        Expr *name = decl->dvar.names[lhs_index++];
        Expr *expr = decl->dvar.vals[rhs_index++];
        Value *rhs = emit_expr(self, expr);
        Operand operand = hmget(self->package->operands, expr);
        Sym *sym = hmget(self->package->symbols, name);
        if (expr->kind == EXPR_CALL) {
            i64 num_values = arrlen(operand.type->taggregate.fields);
            if (num_values == 1) {
                Type *type = llvm_type(self, sym->type);
                AllocaInst *alloca = emit_entry_alloca(self, type, sym->name, sym->type->align);
                sym->userdata = alloca;
                if (compiler.flags.debug) {
                    PosInfo pos = package_posinfo(self->package, sym->decl->range.start);
                    DIScope *scope = arrlast(self->dbg.scopes);
                    DILocalVariable *d = self->dbg.builder->createAutoVariable(
                        scope, sym->name, self->dbg.file, pos.line,
                        llvm_debug_type(self, sym->type));
                    DILocation *loc = DebugLoc::get(pos.line, pos.column, scope);
                    self->dbg.builder->insertDeclare(
                        alloca, d, self->dbg.builder->createExpression(), loc,
                        self->builder.GetInsertBlock());
                }
                // TODO: Handle coersion
                lhs_index++;
            }
        } else {
            Type *type = llvm_type(self, sym->type);
            AllocaInst *alloca = emit_entry_alloca(self, type, sym->name, sym->type->align);
            sym->userdata = alloca;
            set_debug_pos(self, name->range);
            self->builder.CreateAlignedStore(rhs, alloca, sym->type->align);
            lhs_index++;
        }
    }
    self->return_address = prev_return_address;
}

void emit_decl_val(IRContext *self, Decl *decl) {
    Sym *sym = hmget(self->package->symbols, decl->dval.name);
    set_debug_pos(self, decl->dval.name->range);
    Type *type = llvm_type(self, sym->type, sym);
    if (sym->type->kind & SYM_TYPE) return;
    arrpush(self->symbols, sym);
    bool prev_return_address = self->return_address;
    self->return_address = true;
    Value *value = emit_expr(self, decl->dval.val);
    self->return_address = prev_return_address;
    arrpop(self->symbols);
    if (isa<Function>(value)) {
        sym->userdata = value;
        return;
    }

    GlobalValue::LinkageTypes linkage = self->fn ?
    GlobalValue::CommonLinkage : GlobalValue::ExternalLinkage;
    GlobalVariable *global = new GlobalVariable(
        *self->module, type, true, linkage, (Constant *) value, sym->name);
    sym->userdata = global;
}

void emit_decl_import(IRContext *self, Decl *decl) {}
void emit_decl_library(IRContext *self, Decl *decl) {}

void emit_decl_foreign(IRContext *self, Decl *decl) {
    Operand operand = hmget(self->package->operands, decl->dforeign.type);
    Sym *sym = hmget(self->package->symbols, decl->dforeign.name);

    Type *type = llvm_type(self, operand.type);
    if (FunctionType *fn_ty = dyn_cast<FunctionType>(type)) {
        Function *fn = Function::Create(
            fn_ty, Function::ExternalLinkage, sym->external_name, self->module);
        // FIXME: Calling convention
        sym->userdata = fn;
        return;
    }

    Constant *val = self->module->getOrInsertGlobal(sym->external_name ?: sym->name, type);
    GlobalVariable *var = (GlobalVariable *) val;
    var->setExternallyInitialized(true);
    var->setConstant(false);
    sym->userdata = var;
}

void emit_decl_foreignblock(IRContext *self, Decl *decl) {}

void emit_stmt(IRContext *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_VAR:
            if (!arrlen(self->fn)) return emit_decl_var_global(self, (Decl *) stmt);
            else return emit_decl_var(self, (Decl *) stmt);
        case (StmtKind) DECL_VAL: return emit_decl_val(self, (Decl *) stmt);
        case (StmtKind) DECL_IMPORT: return emit_decl_import(self, (Decl *) stmt);
        case (StmtKind) DECL_LIBRARY: return emit_decl_library(self, (Decl *) stmt);
        case (StmtKind) DECL_FOREIGN: return emit_decl_foreign(self, (Decl *) stmt);
        case (StmtKind) DECL_FOREIGNBLOCK: return emit_decl_foreignblock(self, (Decl *) stmt);
        case STMT_LABEL:
            emit_stmt_label(self, stmt);
            break;
        case STMT_ASSIGN:
            emit_stmt_assign(self, stmt);
            break;
        case STMT_RETURN:
            emit_stmt_return(self, stmt);
            break;
        case STMT_DEFER:
            emit_stmt_defer(self, stmt);
            break;
        case STMT_USING:
            emit_stmt_using(self, stmt);
            break;
        case STMT_GOTO:
            emit_stmt_goto(self, stmt);
            break;
        case STMT_BLOCK:
            emit_stmt_block(self, stmt);
            break;
        case STMT_IF:
            emit_stmt_if(self, stmt);
            break;
        case STMT_FOR:
            emit_stmt_for(self, stmt);
            break;
        case STMT_SWITCH:
            emit_stmt_switch(self, stmt);
            break;
        case STMT_NAMES:
            fatal("Shouldn't see this here");
            break;
    }
    if (ISEXPR(stmt)) emit_expr(self, (Expr *) stmt);
}

void emit_decl(IRContext *self, Decl *decl) {
    switch (decl->kind) {
        case DECL_VAR:
            if (!arrlen(self->fn)) return emit_decl_var_global(self, decl);
            else return emit_decl_var(self, decl);
        case DECL_VAL: return emit_decl_val(self, decl);
        case DECL_IMPORT: return emit_decl_import(self, decl);
        case DECL_LIBRARY: return emit_decl_library(self, decl);
        case DECL_FOREIGN: return emit_decl_foreign(self, decl);
        case DECL_FOREIGNBLOCK: return emit_decl_foreignblock(self, decl);
        default:
            fatal("Unrecognized DeclKind %s", describe_ast_kind(decl->kind));
    }
}

void llvm_emit_stmt(IRContext *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_FILE: {
            Source *file = ((Decl *) stmt)->dfile;
            self->dbg.source_file = file;
            self->dbg.file = self->dbg.builder->createFile(file->filename, ".");
            arrsetlen(self->dbg.scopes, 1); // 1 so we keep the compile unit at the top.
            arrpush(self->dbg.scopes, self->dbg.file);
            break;
        }
        case (StmtKind) DECL_VAL:
            emit_decl_val(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_VAR:
            emit_decl_var(self, (Decl *) stmt);
            break;
        case (StmtKind) DECL_IMPORT:
            break;
        case STMT_LABEL: 
            emit_stmt_label(self, stmt);
            break;
        case STMT_ASSIGN: 
            emit_stmt_assign(self, stmt);
            break;
        case STMT_RETURN: 
            emit_stmt_return(self, stmt);
            break;
        case STMT_DEFER: 
            emit_stmt_defer(self, stmt);
            break;
        case STMT_USING: 
            emit_stmt_using(self, stmt);
            break;
        case STMT_GOTO: 
            emit_stmt_goto(self, stmt);
            break;
        case STMT_BLOCK: 
            emit_stmt_block(self, stmt);
            break;
        case STMT_IF: 
            emit_stmt_if(self, stmt);
            break;
        case STMT_FOR: 
            emit_stmt_for(self, stmt);
            break;
        case STMT_SWITCH: 
            emit_stmt_switch(self, stmt);
            break;
        default:
            if (stmt->kind < STMT_KIND_BASE)
                emit_expr(self, (Expr *) stmt);
            fatal("Unrecognized StmtKind %s", describe_ast_kind(stmt->kind));
    }
}

static void addDiscriminatorsPass(const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {
    PM.add(createAddDiscriminatorsPass());
}

bool llvm_emit_object(Package *package) {
    IRContext *self = (IRContext *) package->userdata;

    if (compiler.flags.dump_ir) {
        std::string buf;
        raw_string_ostream os(buf);
        self->module->print(os, nullptr);
        os.flush();
        puts(buf.c_str());
    }

    char object_name[MAX_PATH];
    strncpy(object_name, package->path, MAX_PATH);
    object_name[MAX_PATH - 1] = 0;
    char *ext = strrchr(object_name, '.');
    if (!ext) {
        char *end = object_name + strlen(object_name);
        *(end) = '.';
        *(end+1) = 'o';
        *(end+2) = '\0';
    } else {
        *(ext+1) = 'o';
        *(ext+2) = '\0';
    }

    std::error_code ec;
    raw_fd_ostream dest(object_name, ec);

    if (ec) {
        fatal("Could not open object file: %s\n", ec.message().c_str());
        return false;
    }

    PassManagerBuilder *pm_builder = new(std::nothrow) PassManagerBuilder();
    if (pm_builder == nullptr) {
        fatal("memory allocation failure\n");
        return true;
    }

    pm_builder->OptLevel = self->target->getOptLevel();
    pm_builder->SizeLevel = compiler.flags.small ? 2 : 0;

    pm_builder->DisableTailCalls = compiler.flags.debug;
    pm_builder->DisableUnitAtATime = compiler.flags.debug;
    pm_builder->DisableUnrollLoops = compiler.flags.debug;
    pm_builder->SLPVectorize = !compiler.flags.debug;
    pm_builder->LoopVectorize = !compiler.flags.debug;
    pm_builder->RerollLoops = !compiler.flags.debug;
    pm_builder->DisableGVNLoadPRE = compiler.flags.debug;
    pm_builder->VerifyInput = compiler.flags.assertions;
    pm_builder->VerifyOutput = compiler.flags.assertions;
    pm_builder->MergeFunctions = !compiler.flags.debug;
    pm_builder->PrepareForLTO = false;
    pm_builder->PrepareForThinLTO = false;
    pm_builder->PerformThinLTO = false;

    Triple triple = Triple(self->module->getTargetTriple());
    TargetLibraryInfoImpl tlii(triple);
    pm_builder->LibraryInfo = &tlii;

    if (compiler.flags.debug) {
        pm_builder->Inliner = createAlwaysInlinerLegacyPass(false);
    } else {
        self->target->adjustPassManager(*pm_builder);
        pm_builder->addExtension(PassManagerBuilder::EP_EarlyAsPossible, addDiscriminatorsPass);
        pm_builder->Inliner = createFunctionInliningPass(
            pm_builder->OptLevel, pm_builder->SizeLevel, false);
    }

    legacy::FunctionPassManager function_pm = legacy::FunctionPassManager(self->module);
    pm_builder->populateFunctionPassManager(function_pm);

    legacy::PassManager module_pm;
    pm_builder->populateModulePassManager(module_pm);

    if (!compiler.flags.disable_all_passes) {
        TargetMachine::CodeGenFileType file_type = TargetMachine::CGFT_ObjectFile;
        if (self->target->addPassesToEmitFile(module_pm, dest, nullptr, file_type)) {
            errs() << "TargetMachine cannot emit a file of this type";
            return false;
        }

        // run per function optimization passes
        function_pm.doInitialization();
        for (Function &function : *self->module)
            if (!function.isDeclaration())
                function_pm.run(function);
        function_pm.doFinalization();

        module_pm.run(*self->module);
    }
    dest.flush();

//    if (compiler.flags.dump_ir) {
//        std::string buf;
//        raw_string_ostream os(buf);
//        self->module->print(os, nullptr);
//        os.flush();
//        puts(buf.c_str());
//    }

    if (compiler.flags.emit_ir) {
        std::error_code error_code;
        raw_fd_ostream dest(package->path, error_code);
        if (error_code) return false;
        self->module->print(dest, nullptr);
        dest.close();
        if (dest.has_error()) {
            warn("Error printing to file: %s", dest.error().message().c_str());
            return false;
        }
    }

    if (true) { // TODO: compiler.flags.timing
        TimerGroup::printAll(errs());
    }

    return true;
}

bool llvm_build_module(Package *package) {
    IRContext *context = llvm_create_context(package);
    package->userdata = context;
    for (int i = 0; i < arrlen(package->stmts); i++) {
        llvm_emit_stmt(context, package->stmts[i]);
    }
    return llvm_validate(context);
}

#if DEBUG
void print(Value *val) {
    std::string buf;
    raw_string_ostream os(buf);
    val->print(os, nullptr);
    os.flush();
    puts(buf.c_str());
}

void print(Type *ty) {
    std::string buf;
    raw_string_ostream os(buf);
    ty->print(os, nullptr);
    os.flush();
    puts(buf.c_str());
}

void print(Module *module) {
    std::string buf;
    raw_string_ostream os(buf);
    module->print(os, nullptr);
    os.flush();
    puts(buf.c_str());
}
#endif
