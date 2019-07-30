
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
#include <llvm/LinkAllPasses.h>


#pragma clang diagnostic pop

using namespace llvm;

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

struct BuiltinTypes {
    IntegerType *i1;
    union {
        IntegerType *i8;
        IntegerType *u8;
    };
    union {
        IntegerType *i16;
        IntegerType *u16;
    };
    union {
        IntegerType *i32;
        IntegerType *u32;
    };
    union {
        IntegerType *i64;
        IntegerType *u64;
    };
    Type *f32;
    Type *f64;
    IntegerType *intptr;
    PointerType *rawptr;
};

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

            DIFile *package_file = dbg.builder->createFile(package->sources->filename, package->path);

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
            ty.f32 = Type::getFloatTy(context);
            ty.f64 = Type::getDoubleTy(context);
            ty.intptr = IntegerType::get(context, target->getPointerSizeInBits(0));
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
    if (Triple(sys::getProcessTriple()).isOSDarwin())
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

bool llvm_validate(IRContext *context) {
    bool broken = verifyModule(*context->module);
    if (broken) {
        context->module->print(errs(), nullptr);
        ASSERT(false);
    }
    return broken;
}

Type *llvm_type(IRContext *c, Ty *type, bool do_not_hide_behind_pointer = false) {
    if (type->sym && type->sym->userdata) return (Type *) type->sym->userdata;
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
            std::vector<Type *> params;
            i64 num_params = arrlen(type->tfunc.params);
            if (type->flags&FUNC_CVARGS) num_params -= 1;
            for (i64 i = 0; i < num_params; i++) {
                Type *param = llvm_type(c, type->tfunc.params[i]);
                params.push_back(param);
            }
            Type *result;
            if (type->tfunc.result->kind == TYPE_VOID) {
                result = llvm_void(c);
            } else if (arrlen(type->tfunc.result->taggregate.fields) == 1) {
                result = llvm_type(c, type->tfunc.result->taggregate.fields->type);
            } else {
                result = llvm_type(c, type->tfunc.result);
            }
            Type *fn = FunctionType::get(result, params, type->flags&FUNC_CVARGS);
            if (do_not_hide_behind_pointer) return fn;
            return PointerType::get(fn, 0);
        }
        case TYPE_ARRAY: {
            Type *base = llvm_type(c, type->tarray.eltype);
            Type *ty = ArrayType::get(base, type->tarray.length);
            return ty;
        }
        case TYPE_SLICE: {
            if (type == type_string) return c->ty.rawptr; // FIXME: Temp hack while we don't have slices
            fatal("Unimplemented slice type");
        }
        case TYPE_STRUCT: {
            if (type->flags&OPAQUE) {
                ASSERT(type->sym); // TODO: Frontend check? Opaque can only be named?
                const char *name = type->sym->external_name ?: type->sym->name;
                StructType *ty = StructType::create(c->context, name);
                type->sym->userdata = ty;
                return ty;
            }
            std::vector<Type *> elements;
            for (int i = 0; i < arrlen(type->taggregate.fields); i++) {
                Type *element = llvm_type(c, type->taggregate.fields[i].type);
                elements.push_back(element);
            }
            if (type->sym) {
                const char *name = type->sym->external_name ?: type->sym->name;
                StructType *ty = StructType::create(c->context, elements, name);
                if (type->sym) type->sym->userdata = ty;
                return ty;
            }
            return StructType::get(c->context, elements);
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

DIType *llvm_debug_type(IRContext *c, Ty *type, bool do_not_hide_behind_pointer = false) {
    using namespace dwarf;
    switch (type->kind) {
        case TYPE_INVALID:
        case TYPE_COMPLETING:
        case TYPE_VOID:
            return NULL;
        case TYPE_BOOL: fatal("unimp");
        case TYPE_ENUM: fatal("unimp");
        case TYPE_INT: {
            bool is_signed = (type->flags&SIGNED) != 0;
            switch (type->size) {
                case 1:  return is_signed ? c->dbg.i8  : c->dbg.u8;
                case 2: return is_signed ? c->dbg.i16 : c->dbg.u16;
                case 4: return is_signed ? c->dbg.i32 : c->dbg.u32;
                case 8: return is_signed ? c->dbg.i64 : c->dbg.u64;
                default:
                    char name[1024] = {0};
                    snprintf(name, sizeof name, "%c%u", is_signed ? 'i' : 'u', type->size * 8);
                    return c->dbg.builder->createBasicType(
                        name, type->size * 8, is_signed ? DW_ATE_signed : DW_ATE_unsigned);
            }
        }
        case TYPE_FLOAT:
            switch (type->size) {
                case 4: return c->dbg.f32;
                case 8: return c->dbg.f64;
                default:
                    char name[1024] = {0};
                    snprintf(name, sizeof name, "f%u", type->size * 8);
                    return c->dbg.builder->createBasicType(name, type->size * 8, DW_ATE_float);
            }
        ptr:
        case TYPE_PTR: {
            DIType *pointee = llvm_debug_type(c, type->tptr.base);
            return c->dbg.builder->createPointerType(pointee, c->data_layout.getPointerSizeInBits());
        }
        case TYPE_FUNC: {
            std::vector<Metadata *> params;
            i64 num_params = arrlen(type->tfunc.params);
            if (type->flags&FUNC_CVARGS) num_params -= 1;
            for (i64 i = 0; i < num_params; i++) {
                DIType *param = llvm_debug_type(c, type->tfunc.params[i]);
                params.push_back(param);
            }
            // TODO: Result types are currently useless ... how can we incorperate them?
//            DIType *result;
//            if (type->tfunc.result->kind == TYPE_VOID) {
//                result = NULL; // Void type?
//            } else if (arrlen(type->tfunc.result->taggregate.fields) == 1) {
//                result = llvm_debug_type(c, type->tfunc.result->taggregate.fields->type);
//            } else {
//                result = llvm_debug_type(c, type->tfunc.result);
//            }
            if (type->flags&FUNC_CVARGS)
                params.push_back(c->dbg.builder->createUnspecifiedParameter());
            DITypeRefArray p_types = c->dbg.builder->getOrCreateTypeArray(params);
            DINode::DIFlags flags = DINode::DIFlags::FlagZero;
            unsigned call_conv = 0; // FIXME: Calling convention
            DIType *type = c->dbg.builder->createSubroutineType(p_types, flags, call_conv);
            if (do_not_hide_behind_pointer) return type;
            return c->dbg.builder->createPointerType(type, c->data_layout.getPointerSizeInBits());
        }
        case TYPE_ARRAY: {
            std::vector<llvm::Metadata *> subscripts;
            Ty *eltype = type;
            while (eltype->kind == TYPE_ARRAY) { // NOTE: That 0 is probably wrong
                subscripts.push_back(c->dbg.builder->getOrCreateSubrange(0, type->tarray.length));
                eltype = type->tarray.eltype;
            }
            Type *irtype = llvm_type(c, type);
            u64 size = c->data_layout.getTypeSizeInBits(irtype);
            u32 align = c->data_layout.getPrefTypeAlignment(irtype) * 8;
            DIType *deltype = llvm_debug_type(c, type->tarray.eltype);
            DINodeArray arr = c->dbg.builder->getOrCreateArray(subscripts);
            return c->dbg.builder->createArrayType(size, align, deltype, arr);
        }
        case TYPE_SLICE: {
            if (type == type_string) goto ptr; // FIXME: Temp hack while we don't have slices
            fatal("Unimplemented slice type");
        }
        case TYPE_STRUCT: {
            if (type->flags&OPAQUE) {
                PosInfo pos = package_posinfo(c->package, type->sym->decl->range.start);
                DIType *dtype = c->dbg.builder->createForwardDecl(
                    DW_TAG_structure_type, type->sym->external_name ?: type->sym->name,
                    c->dbg.file, c->dbg.file, pos.line); // NOTE: scopes being set correctly crashes
                return dtype;
            }
            std::vector<Metadata *> members;
            for (u32 i = 0; i < arrlen(type->taggregate.fields); i++) {
                TyField field = type->taggregate.fields[i];
                DIType *dtype = llvm_debug_type(c, field.type);
                DIDerivedType *member = c->dbg.builder->createMemberType(
                    arrlast(c->dbg.scopes), field.name, c->dbg.file, 0,
                    field.type->size * 8, field.type->align * 8, field.offset * 8,
                    DINode::DIFlags::FlagZero, dtype);
                members.push_back(member);
            }
            DINodeArray members_arr = c->dbg.builder->getOrCreateArray(members);
            PosInfo pos = package_posinfo(c->package, type->sym->decl->range.start);
            DIType *dtype = c->dbg.builder->createStructType(
                arrlast(c->dbg.scopes), type->sym->external_name ?: type->sym->name,
                c->dbg.file, pos.line, type->size * 8, type->align * 8,
                DINode::DIFlags::FlagZero, NULL, members_arr);
            return dtype;
        }

        case TYPE_UNION:  fatal("unimp");
        case TYPE_ANY:    fatal("unimp");
        default:
            fatal("Unhandled type");
    }
// TODO: Can we use this for type aliasing Type :: u8
// DIType *dtype = c->dbg.builder->createTypedef(
//     base, type->sym->external_name ?: type->sym->name,
//     c->dbg.file, pos.line, arrlast(c->dbg.scopes));
}

AllocaInst *emit_entry_alloca(IRContext *self, Type *type, const char *name, u32 alignment_bytes) {
    IRFunction *fn = &arrlast(self->fn);
    IRBuilder<> b(fn->entry_block);
    if (fn->last_entry_alloca) {
        if (Instruction *next = fn->last_entry_alloca->getNextNode())
            b.SetInsertPoint(next);
    } else {
        b.SetInsertPoint(fn->entry_block, fn->entry_block->begin());
        fn->entry_block->end();
    }
    AllocaInst *alloca = b.CreateAlloca(type, 0, name ?: "");
    fn->last_entry_alloca = alloca;
    if (alignment_bytes) alloca->setAlignment(alignment_bytes);
    return alloca;
}

// MARK: - conversions

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

void emit_agg_store(IRContext *self, Value *val, Value *dst) {
    // Prefer scalar stores to first-class aggregate stores
    if (StructType *sty = dyn_cast<StructType>(val->getType())) {
        for (unsigned i = 0, e = sty->getNumElements(); i != e; i++) {
            Value *elptr = self->builder.CreateStructGEP(dst, i);
            Value *el = self->builder.CreateExtractValue(val, i);

            Type *target_type = elptr->getType()->getPointerElementType();
            u32 align = self->data_layout.getPrefTypeAlignment(target_type);
            self->builder.CreateAlignedStore(el, elptr, align);
        }
    } else {
        Type *target_type = dst->getType()->getPointerElementType();
        u32 align = self->data_layout.getPrefTypeAlignment(target_type);
        self->builder.CreateAlignedStore(val, dst, align);
    }
}

Value *create_element_bitcast(IRContext *self, Value *addr, Type *ty) {
    unsigned addrspace = addr->getType()->getPointerAddressSpace();
    Type *ptr_ty = ty->getPointerTo(addrspace);
    return self->builder.CreateBitCast(addr, ptr_ty);
}

void create_store(IRContext *self, Value *src, Value *dst) {
    Type *src_ty = src->getType();
    Type *dst_ty = dst->getType()->getPointerElementType();
    ASSERT(src_ty == dst_ty);
    u32 align = self->data_layout.getPrefTypeAlignment(dst_ty);
    self->builder.CreateAlignedStore(src, dst, align);
}

Value *create_load(IRContext *self, Value *val) {
    Type *ty = val->getType()->getPointerElementType();
    u32 align = self->data_layout.getPrefTypeAlignment(ty);
    return self->builder.CreateAlignedLoad(ty, val, align);
}

Value *remove_load(IRContext *self, Value *val) {
    if (LoadInst *load = dyn_cast<LoadInst>(val)) {
        val = load->getPointerOperand();
        load->eraseFromParent();
    }
    return val;
}

void create_coerced_store(IRContext *self, Value *src, Value *dst) {
    Type *src_ty = src->getType();
    Type *dst_ty = dst->getType()->getPointerElementType();
    if (src_ty == dst_ty) {
        u32 align = self->data_layout.getPrefTypeAlignment(dst_ty);
        self->builder.CreateAlignedStore(src, dst, align);
        return;
    }

    u32 src_align = self->data_layout.getPrefTypeAlignment(dst_ty);
    u64 src_size = self->data_layout.getTypeAllocSize(src_ty);

    if (StructType *dst_struct_ty = dyn_cast<StructType>(dst_ty)) {
        dst = enter_struct_pointer_for_coerced_access(self, dst, dst_struct_ty, src_size);
        dst_ty = dst->getType()->getPointerElementType();
    }

    // If the src and dst are int or ptr types, ext or trunc to desired
    if ((isa<IntegerType>(src_ty) || isa<PointerType>(src_ty)) &&
        (isa<IntegerType>(dst_ty) || isa<PointerType>(dst_ty))) {
        src = coerce_int_or_ptr(self, src, dst_ty);
        u32 dst_align = self->data_layout.getPrefTypeAlignment(dst_ty);
        self->builder.CreateAlignedStore(src, dst, dst_align);
        return;
    }

    u64 dst_size = self->data_layout.getTypeAllocSize(dst_ty);

    // If store is legal, just bitcast the src pointer
    if (src_size <= dst_size) {
        dst = create_element_bitcast(self, dst, src_ty);
        emit_agg_store(self, src, dst);
    } else {
        // do coercion through memory
        AllocaInst *tmp = emit_entry_alloca(self, src_ty, "coerce.alloca", src_align);
        self->builder.CreateAlignedStore(src, tmp, src_align);

        Value *src_casted = create_element_bitcast(self, tmp, self->ty.i8);
        Value *dst_casted = create_element_bitcast(self, dst, self->ty.i8);
        u32 dst_align = self->data_layout.getPrefTypeAlignment(dst_ty);
        self->builder.CreateMemCpy(dst_casted, dst_align, src_casted, src_align, dst_size);
    }
}

Value *create_coerce(IRContext *self, Value *val, Expr *expr, bool is_lvalue = false) {
    Ty *dst = hmget(self->package->operands, expr).type;
    if (dst == type_cvarg) return val; // FIXME: C Varg rules
    Type *dst_ty = llvm_type(self, dst);
start:
    Type *val_ty = val->getType();
    if (val_ty == dst_ty) return val;
    if (is_lvalue && val_ty->getPointerElementType() == dst_ty) return val;
    switch (val_ty->getTypeID()) {
        case Type::VoidTyID: return val;
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
                    u64 src_size = val_ty->getPrimitiveSizeInBits();
                    u64 dst_size = dst_ty->getPrimitiveSizeInBits();
                    if (src_size < dst_size)      val = self->builder.CreateFPExt(val, dst_ty);
                    else if (src_size > dst_size) val = self->builder.CreateFPTrunc(val, dst_ty);
                    return val;
                }
                case Type::IntegerTyID: {
                    if (is_signed(dst)) val = self->builder.CreateFPToSI(val, dst_ty);
                    else                val = self->builder.CreateFPToUI(val, dst_ty);
                    return val;
                }
                default:
                    break;
            }
            break;
        case Type::IntegerTyID: { // Coercions from signed to unsigned is disallowed
            if (is_signed(dst)) return self->builder.CreateSExtOrTrunc(val, dst_ty);
            if (isa<PointerType>(dst_ty)) return val = self->builder.CreateIntToPtr(val, dst_ty);
            return self->builder.CreateZExtOrTrunc(val, dst_ty);
        }
        case Type::FunctionTyID: {
            if (isa<PointerType>(dst_ty)) return self->builder.CreateBitOrPointerCast(val, dst_ty);
            return val;
        }
        case Type::StructTyID: return val;
        case Type::ArrayTyID: return val;
        case Type::PointerTyID: {
            if (isa<IntegerType>(dst_ty) && dst_ty->getPrimitiveSizeInBits() == 1) {
                PointerType *src_ty = (PointerType *) val->getType();
                return self->builder.CreateICmpNE(val, ConstantPointerNull::get(src_ty));
            }
            if (isa<IntegerType>(dst_ty)) {
                val = self->builder.CreatePtrToInt(val, self->ty.intptr);
                goto start;
            }
            if (isa<FunctionType>(val_ty)) return val;
            if (val_ty->getPointerElementType() == dst_ty) return create_load(self, val); // FIXME: Do not do this....
            // FIXME: IF isa<ArrayType> What do?
            return self->builder.CreatePointerBitCastOrAddrSpaceCast(val, dst_ty);
        }
        case Type::VectorTyID:
        case Type::LabelTyID:
        case Type::MetadataTyID:
        case Type::X86_MMXTyID:
        case Type::TokenTyID: fatal("Unsupported type in IR");
    }
    return val;
}

Value *create_cast(IRContext *self, Value *val, Expr *expr, bool is_lvalue = false) {
    Ty *dst = hmget(self->package->operands, expr).type;
    if (dst == type_cvarg) return val; // FIXME: C Varg rules
    Type *dst_ty = llvm_type(self, dst);
start:
    Type *val_ty = val->getType();
    if (val_ty == dst_ty) return val;
    if (is_lvalue && val_ty->getPointerElementType() == dst_ty) return val;
    switch (val_ty->getTypeID()) {
        case Type::VoidTyID: return val;
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
                    u64 src_size = val_ty->getPrimitiveSizeInBits();
                    u64 dst_size = dst_ty->getPrimitiveSizeInBits();
                    if (src_size < dst_size)      val = self->builder.CreateFPExt(val, dst_ty);
                    else if (src_size > dst_size) val = self->builder.CreateFPTrunc(val, dst_ty);
                    return val;
                }
                case Type::IntegerTyID: { // TODO: Coercion rules shouldn't allow ftoi
                    if (is_signed(dst)) val = self->builder.CreateFPToSI(val, dst_ty);
                    else                val = self->builder.CreateFPToUI(val, dst_ty);
                    return val;
                }
                default:
                    break;
            }
            break;
        case Type::IntegerTyID: { // Coercions from signed to unsigned is disallowed
            if (is_signed(dst)) return self->builder.CreateSExtOrTrunc(val, dst_ty);
            if (isa<PointerType>(dst_ty)) return val = self->builder.CreateIntToPtr(val, dst_ty);
            return self->builder.CreateZExtOrTrunc(val, dst_ty);
        }
        case Type::FunctionTyID: {
            if (isa<PointerType>(dst_ty)) return self->builder.CreateBitOrPointerCast(val, dst_ty);
            return val;
        }
        case Type::StructTyID: return val;
        case Type::ArrayTyID: return val;
        case Type::PointerTyID: {
            if (isa<IntegerType>(dst_ty) && dst_ty->getPrimitiveSizeInBits() == 1) {
                PointerType *src_ty = (PointerType *) val->getType();
                return self->builder.CreateICmpNE(val, ConstantPointerNull::get(src_ty));
            }
            if (isa<IntegerType>(dst_ty)) {
                val = self->builder.CreatePtrToInt(val, self->ty.intptr);
                goto start;
            }
            if (isa<FunctionType>(val_ty)) return val;
            if (val_ty->getPointerElementType() == dst_ty) return create_load(self, val); // FIXME: Do not do this....
            // FIXME: IF isa<ArrayType> What do?
            return self->builder.CreatePointerBitCastOrAddrSpaceCast(val, dst_ty);
        }
        case Type::VectorTyID:
        case Type::LabelTyID:
        case Type::MetadataTyID:
        case Type::X86_MMXTyID:
        case Type::TokenTyID: fatal("Unsupported type in IR");
    }
    return val;
}

// MARK: - Emission

Value *emit_expr(IRContext *self, Expr *expr, bool is_lvalue = false);
void emit_decl(IRContext *self, Decl *decl);
void emit_stmt(IRContext *self, Stmt *stmt);

Value *emit_expr_nil(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    Type *type = llvm_type(self, operand.type);
    set_debug_pos(self, expr->range);
    return ConstantPointerNull::get((PointerType *) type);
}

Value *emit_expr_int(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    Type *type = llvm_type(self, operand.type);
    set_debug_pos(self, expr->range);
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
    IRContext ctx(self, package);
    ctx.dbg.source_file = package_posinfo(package, sym->decl->range.start).source;
    if (compiler.flags.debug) {
        arrpush(ctx.dbg.scopes, self->dbg.unit);
        ctx.dbg.file = ctx.dbg.builder->createFile(ctx.dbg.source_file->filename, package->path);
        arrpush(ctx.dbg.scopes, ctx.dbg.file);
    }
    emit_decl(&ctx, sym->decl);
    ASSERT(sym->userdata);
    return (Value *) sym->userdata;
}

Value *emit_expr_name(IRContext *self, Expr *expr) {
    Sym *sym = hmget(self->package->symbols, expr);
    if (!sym->userdata) {
        emit_sym(self, self->package, sym);
        ASSERT(sym->userdata);
    }
    if (isa<Function>((Value *) sym->userdata)) return (Value *) sym->userdata;
    return create_load(self, (Value *) sym->userdata);
}

Value *emit_expr_field(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr->efield.expr);
    if (operand.flags&PACKAGE) { // operand.val is a pointer to the package Symbol
        Sym *package_sym = (Sym *) operand.val.p;
        Sym *sym = hmget(self->package->symbols, expr->efield.name);
        if (!sym->userdata) { // TODO: Switch some of the context stuff (DIFile) etc.
            emit_sym(self, package_sym->package, sym);
        }
        if (isa<Function>((Value *) sym->userdata)) return (Value *) sym->userdata;
        return create_load(self, (Value *) sym->userdata);
    }
    switch (operand.type->kind) {
        case TYPE_STRUCT:
            break;
    }
    fatal("Unhandled field base kind");
}

Value *emit_expr_compound(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    switch (operand.type->kind) {
        case TYPE_STRUCT:
        case TYPE_UNION:
            fatal("Unimplemented");
        case TYPE_ARRAY: {
            Type *type = llvm_type(self, operand.type, true);
            Value *value = Constant::getNullValue(type);
            if (!expr->ecompound.fields) return value;
            bool is_constant = true;
            std::vector<Value *> values;
            for (i64 i = 0; i < arrlen(expr->ecompound.fields); i++) {
                Value *el = emit_expr(self, expr->ecompound.fields[i].val);
                is_constant |= isa<Constant>(el);
                values.push_back(el);
            }
            u32 alignment = self->data_layout.getPrefTypeAlignment(type);
            AllocaInst *alloca = emit_entry_alloca(self, type, "compound.lit", alignment);
            if (is_constant) {
                for (i64 i = 0; i < arrlen(expr->ecompound.fields); i++) {
                    CompoundField field = expr->ecompound.fields[i];
                    u64 target_index = hmget(self->package->operands, field.key).val.u;
                    value = self->builder.CreateInsertValue(value, values[i], {(u32) target_index});
                }
                ASSERT(isa<Constant>(value));
                GlobalVariable *global = new GlobalVariable(*self->module, type, true, GlobalValue::PrivateLinkage, (Constant *)value, "compound.lit");
                self->builder.CreateMemCpy(alloca, alignment, global, global->getAlignment(), type->getPrimitiveSizeInBits() / 8);
                value = alloca;
            } else {
                for (i64 i = 0; i < arrlen(expr->ecompound.fields); i++) {
                    CompoundField field = expr->ecompound.fields[i];
                    u64 target_index = hmget(self->package->operands, field.key).val.u;
                    value = self->builder.CreateInsertValue(alloca, values[i], {0, (u32) target_index});
                }
            }
            return alloca;
        }
        case TYPE_SLICE:
        default:
            fatal("Unimplmented");
    }
}

Value *emit_expr_cast(IRContext *self, Expr *expr) {
    Ty *dst = hmget(self->package->operands, expr->ecast.type).type;
    Type *dst_ty = llvm_type(self, dst);
    Value *val = emit_expr(self, expr->ecast.expr);
start:
    Type *val_ty = val->getType();
    if (val_ty == dst_ty) return val;
    switch (val_ty->getTypeID()) {
        case Type::VoidTyID: return val;
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
                    u64 src_size = val_ty->getPrimitiveSizeInBits();
                    u64 dst_size = dst_ty->getPrimitiveSizeInBits();
                    if (src_size < dst_size)      val = self->builder.CreateFPExt(val, dst_ty);
                    else if (src_size > dst_size) val = self->builder.CreateFPTrunc(val, dst_ty);
                    return val;
                }
                case Type::IntegerTyID: {
                    if (is_signed(dst)) val = self->builder.CreateFPToSI(val, dst_ty);
                    else                val = self->builder.CreateFPToUI(val, dst_ty);
                    return val;
                }
                default:
                    break;
            }
            break;
        case Type::IntegerTyID: { // Coercions from signed to unsigned is disallowed
            if (is_signed(dst)) return self->builder.CreateSExtOrTrunc(val, dst_ty);
            if (isa<PointerType>(dst_ty)) return val = self->builder.CreateIntToPtr(val, dst_ty);
            return self->builder.CreateZExtOrTrunc(val, dst_ty);
        }
        case Type::FunctionTyID: {
            if (isa<PointerType>(dst_ty)) return self->builder.CreateBitOrPointerCast(val, dst_ty);
            return val;
        }
        case Type::StructTyID: return val;
        case Type::ArrayTyID: return val;
        case Type::PointerTyID: {
            if (isa<IntegerType>(dst_ty) && dst_ty->getPrimitiveSizeInBits() == 1) {
                PointerType *src_ty = (PointerType *) val->getType();
                return self->builder.CreateICmpNE(val, ConstantPointerNull::get(src_ty));
            }
            if (isa<IntegerType>(dst_ty)) {
                val = self->builder.CreatePtrToInt(val, self->ty.intptr);
                goto start;
            }
            if (isa<FunctionType>(val_ty)) return val;
            if (val_ty->getPointerElementType() == dst_ty) return create_load(self, val); // FIXME: Do not do this....
            // FIXME: IF isa<ArrayType> What do?
            return self->builder.CreatePointerBitCastOrAddrSpaceCast(val, dst_ty);
        }
        case Type::VectorTyID:
        case Type::LabelTyID:
        case Type::MetadataTyID:
        case Type::X86_MMXTyID:
        case Type::TokenTyID: fatal("Unsupported type in IR");
    }
    return val;
}

Value *emit_expr_paren(IRContext *self, Expr *expr) {
    return emit_expr(self, expr->eparen);
}

Value *emit_expr_unary(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    Value *val = emit_expr(self, expr->eunary, expr->flags == OP_AND);
    set_debug_pos(self, expr->range);
    switch ((Op) expr->flags) {
        case OP_ADD:
        case OP_AND:
            return val;
        case OP_SUB:
            if (operand.type->kind == TYPE_FLOAT) return self->builder.CreateFNeg(val);
            else                                  return self->builder.CreateNeg(val);
        case OP_NOT: // fallthrough
        case OP_BNOT: return self->builder.CreateNot(val);
        case OP_LSS:  return create_load(self, val);
        default:
            fatal("Unhandled unary op case %d", expr->flags);
    }
}

Value *emit_expr_binary(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    bool is_int = is_integer(operand.type) || is_ptr(operand.type);
    Type *type = llvm_type(self, operand.type);
    Value *lhs = emit_expr(self, expr->ebinary.elhs);
    Value *rhs = emit_expr(self, expr->ebinary.erhs);
    set_debug_pos(self, expr->range);
    IRBuilder<> b = self->builder;
    switch (expr->flags) {
        case OP_ADD: return is_int ? b.CreateAdd(lhs, rhs) : b.CreateFAdd(lhs, rhs); // FIXME: Pointers?
        case OP_SUB: return is_int ? b.CreateSub(lhs, rhs) : b.CreateFSub(lhs, rhs); // FIXME: Pointers?
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
        case OP_LSS: // FIXME: Pointers?
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
    Value *cond = emit_expr(self, expr->eternary.econd);
    Value *pass = cond;
    if (expr->eternary.epass)
        pass = emit_expr(self, expr->eternary.epass);
    Value *fail = emit_expr(self, expr->eternary.efail);
    return self->builder.CreateSelect(cond, pass, fail);
}

Value *emit_expr_call(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr->ecall.expr);
    std::vector<Value *> args;
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
        args.push_back(val);
    }
    Value *fn = emit_expr(self, expr->ecall.expr);
    set_debug_pos(self, expr->range);
    return self->builder.CreateCall(fn, args);
}

Value *emit_expr_index(IRContext *self, Expr *expr) {
    Operand value_operand = hmget(self->package->operands, expr->eindex.expr);
    Operand index_operand = hmget(self->package->operands, expr->eindex.index);
    Value *value = emit_expr(self, expr->eindex.expr, LVALUE);
    Value *index = emit_expr(self, expr->eindex.index);
    // NOTE: LLVM doesn't have unsigned integers and an index in the upper-half
    // of an unsigned integer would get wrapped and become negative. We can
    // prevent this by ZExt-ing the index
    if (!is_signed(index_operand.type))
        index = self->builder.CreateZExtOrBitCast(index, self->ty.u64);
    switch (value_operand.type->kind) {
        case TYPE_ARRAY: {
            Value *zero = ConstantInt::get(self->ty.i32, 0);
            Value *addr = self->builder.CreateGEP(value, {zero, index});
            return create_load(self, addr);
        }
        case TYPE_PTR: {
            Value *addr = self->builder.CreateGEP(value, index);
            return create_load(self, addr);
        }
        default:
            fatal("Unhandled case %s", describe_ast(self->package, expr));
    }
}

Value *emit_expr_slice(IRContext *self, Expr *expr) { return NULL; }

Function *emit_expr_func(IRContext *self, Expr *expr) {
    Operand operand = hmget(self->package->operands, expr);
    FunctionType *type = (FunctionType *) llvm_type(self, operand.type, true);
    const char *name = NULL;
    if (arrlen(self->symbols))
        name = arrlast(self->symbols)->external_name ?: arrlast(self->symbols)->name;
    Function::LinkageTypes linkage = Function::LinkageTypes::ExternalLinkage;
    Function *fn = Function::Create(type, linkage, name ?: "", self->module);
    if (compiler.flags.debug) {
        DIType *dbg_type = llvm_debug_type(self, operand.type, true);
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
        Value *return_value = create_load(self, function->result_value);
        self->builder.CreateRet(return_value);
    } else if (operand.type->tfunc.result->kind == TYPE_STRUCT) {
        StructType *type = dyn_cast<StructType>(fn->getReturnType());
        ASSERT(type);
        DataLayout dl = self->module->getDataLayout();
        Value *return_value = create_load(self, function->result_value);
        self->builder.CreateRet(return_value);
    } else {
        ASSERT(operand.type->tfunc.result->kind == TYPE_VOID);
        self->builder.CreateRetVoid();
    }

    // Move the return block to the end, just for readability
    if (compiler.flags.dump_ir || compiler.flags.emit_ir)
        function->return_block->moveAfter(self->builder.GetInsertBlock());

    if (compiler.flags.debug) {
        self->dbg.builder->finalizeSubprogram(fn->getSubprogram());
        arrpop(self->dbg.scopes);
    }

//    if (verifyFunction(*fn, &errs())) {
//        printf("\n====================\n");
//        self->module->print(errs(), nullptr);
//        ASSERT(false);
//    }

    return fn;
}

Value *emit_expr_functype(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }
Value *emit_expr_slicetype(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }
Value *emit_expr_array(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }
Value *emit_expr_pointer(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }
Value *emit_expr_struct(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }
Value *emit_expr_union(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }
Value *emit_expr_enum(IRContext *self, Expr *expr) { fatal("Unimplemented %s", __FUNCTION__); }

Value *emit_expr(IRContext *self, Expr *expr, bool is_lvalue) {
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
        case EXPR_FUNC:      val = emit_expr_func(self, expr); break;
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
    if (is_lvalue) val = remove_load(self, val);
    return create_coerce(self, val, expr, is_lvalue);
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
            Operand operand = hmget(self->package->operands, expr->ecall.expr);
            i64 num_results = arrlen(operand.type->tfunc.result->taggregate.fields);
            if (num_results == 1) {
                Value *lhs = emit_expr(self, stmt->sassign.lhs[lhs_index++], LVALUE);
                set_debug_pos(self, stmt->range);
                create_coerced_store(self, rhs, lhs);
            } else {
                // create some stack space to land the returned struct onto
                AllocaInst *result_address = emit_entry_alloca(self, rhs->getType(), nullptr, 0);
                create_coerced_store(self, rhs, result_address);

                for (i64 result_index = 0; result_index < num_lhs; result_index++) {
                    Expr *lhs_expr = stmt->sassign.lhs[lhs_index++];
                    Value *lhs = emit_expr(self, lhs_expr, LVALUE);
                    set_debug_pos(self, stmt->range);

                    Value *addr = self->builder.CreateStructGEP(
                        rhs->getType(), result_address, (u32) result_index);
                    Value *val = create_load(self, addr);

                    create_coerced_store(self, val, lhs);
                }
            }
        } else {
            Expr *lhs_expr = stmt->sassign.lhs[lhs_index++];
            Value *lhs = emit_expr(self, lhs_expr, LVALUE);
            set_debug_pos(self, stmt->range);
            create_coerced_store(self, rhs, lhs);
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
        Value *val = emit_expr(self, expr);
        arrpush(values, val);
    }
    llvm_debug_unset_pos(self);
    if (num_returns > 1) {
        for (u32 i = 0; i < num_returns; i++) {
            Value *result_el_ptr = self->builder.CreateStructGEP(ret_type, fn.result_value, i);
            create_store(self, values[i], result_el_ptr);
        }
    } else if (num_returns == 1) {
        create_coerced_store(self, values[0], fn.result_value);
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
                break;
            case GOTO_CONTINUE:
                target = arrlast(fn->loop_cond_blocks);
                break;
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
    if (stmt->flags == FOR_AGGREGATE) {
        Constant *zero = ConstantInt::get(self->ty.intptr, 0);
        Constant *one = ConstantInt::get(self->ty.intptr, 1);
        Sym *value_sym = hmget(self->package->symbols, stmt->sfor.value_name);
        Type *value_type = llvm_type(self, value_sym->type);
        Sym *index_sym = NULL;
        if (stmt->sfor.index_name) hmget(self->package->symbols, stmt->sfor.index_name);
        u32 value_align = self->data_layout.getPrefTypeAlignment(value_type);
        u32 index_align = self->data_layout.getPrefTypeAlignment(self->ty.intptr);
        const char *index_name = index_sym ? index_sym->name : "index";
        Value *value = emit_entry_alloca(self, value_type, value_sym->name ?: "value", value_align);
        Value *index = emit_entry_alloca(self, self->ty.intptr, index_name, index_align);
        create_store(self, zero, index);
        value_sym->userdata = value;
        if (index_sym) index_sym->userdata = index;

        Value *aggregate;
        Value *length;
        Operand op = hmget(self->package->operands, stmt->sfor.aggregate);
        switch (op.type->kind) {
            case TYPE_ARRAY: {
                aggregate = emit_expr(self, stmt->sfor.aggregate, LVALUE);
                length = Constant::getNullValue(self->ty.intptr);
                break;
            }
            default: fatal("Unhandled type for emission of for aggregate");
        }
        cond = BasicBlock::Create(self->context, "for.cond", fn->function);
        step = BasicBlock::Create(self->context, "for.step", fn->function);
        body = BasicBlock::Create(self->context, "for.body", fn->function);
        post = BasicBlock::Create(self->context, "for.post", fn->function);

        self->builder.CreateBr(cond);
        self->builder.SetInsertPoint(cond);

        Value *within_bounds = self->builder.CreateICmpSLT(create_load(self, index), length);
        self->builder.CreateCondBr(within_bounds, body, post);
        arrpush(fn->loop_cond_blocks, cond);
        arrpush(fn->post_blocks, post);

        self->builder.SetInsertPoint(body);
        Value *elptr = self->builder.CreateGEP(aggregate, {zero, create_load(self, index)});
        create_store(self, create_load(self, elptr), value);

        emit_stmt(self, stmt->sfor.body);

        b32 has_jump = self->builder.GetInsertBlock()->getTerminator() != NULL;
        if (!has_jump) self->builder.CreateBr(step);
        self->builder.SetInsertPoint(step);
        Value *index_incr = self->builder.CreateAdd(create_load(self, index), one);
        create_store(self, index_incr, index);
        self->builder.CreateBr(cond);
        self->builder.SetInsertPoint(post);
        if (compiler.flags.dump_ir || compiler.flags.emit_ir)
            post->moveAfter(self->builder.GetInsertBlock());
        return;
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
        self->builder.SetInsertPoint(cond);
        Value *cond_val = emit_expr(self, stmt->sfor.cond);
        self->builder.CreateCondBr(cond_val, body, post);
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
    if (!self->builder.GetInsertBlock()->getTerminator()) self->builder.CreateBr(post);
    self->builder.SetInsertPoint(post);
    if (compiler.flags.debug) arrpop(self->dbg.scopes);
    if (compiler.flags.dump_ir || compiler.flags.emit_ir)
        post->moveAfter(self->builder.GetInsertBlock());
}

void emit_stmt_switch(IRContext *self, Stmt *stmt) {
    BasicBlock *current_block = self->builder.GetInsertBlock();
    IRFunction *fn = &arrlast(self->fn);
    BasicBlock *post = BasicBlock::Create(self->context, "switch.post", fn->function);
    BasicBlock *default_block = NULL;
    arrpush(fn->post_blocks, post);
    size_t num_cases = arrlen(stmt->sswitch.cases);
    std::vector<BasicBlock *> case_blocks;
    for (i64 i = 0; i < num_cases; i++) {
        if (arrlen(stmt->sswitch.cases[i].matches)) {
            BasicBlock *then = BasicBlock::Create(self->context, "switch.then.case", fn->function);
            case_blocks.push_back(then);
        } else {
            BasicBlock *then = BasicBlock::Create(self->context, "switch.default", fn->function);
            default_block = then;
            case_blocks.push_back(then);
        }
    }
    Value *value = emit_expr(self, stmt->sswitch.subject);
    std::vector<std::vector<Value *>> matches;
    for (i64 i = 0; i < num_cases; i++) {
        if (i + 1 < num_cases) arrpush(fn->next_cases, case_blocks[i + 1]);
        BasicBlock *case_block = case_blocks[i];
        self->builder.SetInsertPoint(case_block);
        SwitchCase scase = stmt->sswitch.cases[i];
        i64 num_matches = arrlen(scase.matches);
        if (num_matches) {
            std::vector<Value *> vals;
            for (i64 match_index = 0; match_index < num_matches; match_index++) {
                Value *val = emit_expr(self, scase.matches[match_index]);
                vals.push_back(val);
            }
            matches.push_back(vals);
        }
        emit_stmt(self, scase.body);
        if (!self->builder.GetInsertBlock()->getTerminator()) self->builder.CreateBr(post);
    }
    self->builder.SetInsertPoint(current_block);
    SwitchInst *sswitch = self->builder.CreateSwitch(
        value, default_block ?: post, (u32) case_blocks.size());
    for (i64 i = 0; i < case_blocks.size(); i++) {
        for (i64 match_index = 0; match_index < matches[i].size(); match_index++) {
            sswitch->addCase((ConstantInt *)matches[i][match_index], case_blocks[i]);
        }
    }
    self->builder.SetInsertPoint(post);
}

void emit_decl_var_global(IRContext *self, Decl *decl) {
    Expr *name = *decl->dvar.names;
    Sym *sym = hmget(self->package->symbols, name);
    if (sym->userdata) return; // Already emitted
    Constant *init = NULL;
    if (decl->dvar.vals) {
        Expr *expr = *decl->dvar.vals;
        Value *val = emit_expr(self, expr);
        init = dyn_cast<Constant>(val);
        if (!init) fatal("unimplemented non constant global variables");
    } else {
        init = Constant::getNullValue(llvm_type(self, sym->type));
    }
    Type *type = llvm_type(self, sym->type);
    GlobalVariable *global = new GlobalVariable(
        *self->module, type, /* IsConstant */ false, GlobalValue::ExternalLinkage,
        init, sym->external_name ?: sym->name);
    global->setAlignment(sym->type->align);
    global->setExternallyInitialized(false);
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

void declare_auto_variable(IRContext *self, Sym *sym) {
    PosInfo pos = package_posinfo(self->package, sym->decl->range.start);
    BasicBlock *block = self->builder.GetInsertBlock();
    DIScope *scope = arrlast(self->dbg.scopes);
    DIExpression *expr = self->dbg.builder->createExpression();
    DILocation *loc = DebugLoc::get(pos.line, pos.column, scope);
    DIType *type = llvm_debug_type(self, sym->type);
    DILocalVariable *d = self->dbg.builder->createAutoVariable(scope, sym->name, self->dbg.file, pos.line, type);
    self->dbg.builder->insertDeclare((Value *) sym->userdata, d, expr, loc, block);
}

void emit_decl_var(IRContext *self, Decl *decl) {
    for (u32 index = 0; index < arrlen(decl->dvar.names); index++) {
        Expr *name = decl->dvar.names[index];
        Sym *sym = hmget(self->package->symbols, name);
        if (sym->userdata) return; // Already emitted

        Value *rhs = NULL;
        if (decl->dvar.vals) {
            Expr *expr = decl->dvar.vals[index];
            Operand operand = hmget(self->package->operands, expr);
            rhs = emit_expr(self, expr);

            if (expr->kind == EXPR_CALL && operand.type->kind == TYPE_STRUCT &&
                (operand.type->flags&TUPLE)) {
                while (index < arrlen(decl->dvar.names)) {
                    name = decl->dvar.names[index];
                    sym = hmget(self->package->symbols, name);
                    Type *type = llvm_type(self, sym->type);
                    AllocaInst *alloca = emit_entry_alloca(self, type, sym->name, sym->type->align);
                    sym->userdata = alloca;
                    set_debug_pos(self, name->range);
                    if (compiler.flags.debug) declare_auto_variable(self, sym);
                    Value *result = self->builder.CreateExtractValue(rhs, {index++});
                    create_coerced_store(self, result, alloca);
                }
                return;
            }
        }
        Type *type = llvm_type(self, sym->type);
        AllocaInst *alloca = emit_entry_alloca(self, type, sym->name, sym->type->align);
        sym->userdata = alloca;
        set_debug_pos(self, name->range);
        if (compiler.flags.debug) declare_auto_variable(self, sym);
        if (rhs) create_coerced_store(self, rhs, alloca);
    }
}

void emit_decl_val(IRContext *self, Decl *decl) {
    Sym *sym = hmget(self->package->symbols, decl->dval.name);
    if (sym->userdata) return; // Already emitted
    set_debug_pos(self, decl->dval.name->range);
    Type *type = llvm_type(self, sym->type);
    if (sym->type->kind & SYM_TYPE) return;
    arrpush(self->symbols, sym);
    Value *value = emit_expr(self, decl->dval.val);
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
    set_debug_pos(self, decl->range);

    Type *type = llvm_type(self, operand.type);
    if (operand.type->kind == TYPE_FUNC) {
//    if (FunctionType *fn_ty = dyn_cast<FunctionType>(type)) {
        FunctionType *fn_ty = (FunctionType *) type->getPointerElementType();
        Function *fn = Function::Create(
            fn_ty, Function::ExternalLinkage, sym->external_name, self->module);
        // FIXME: Calling convention
        fn->setCallingConv(CallingConv::C);
//        if (compiler.flags.debug) {
//            DIType *dbg_type = llvm_debug_type(self, operand.type);
//            PosInfo pos = package_posinfo(self->package, decl->dforeign.name->range.start);
//            DISubprogram *sp = self->dbg.builder->createFunction(
//                arrlast(self->dbg.scopes), sym->name, fn->getName(), self->dbg.file, pos.line,
//                (DISubroutineType *) dbg_type, pos.line, DINode::DIFlags::FlagZero);
//            fn->setSubprogram(sp);
//            self->dbg.builder->finalizeSubprogram(sp);
//        }
        sym->userdata = fn;
        return;
    }

    Constant *val = self->module->getOrInsertGlobal(sym->external_name ?: sym->name, type);
    GlobalVariable *var = (GlobalVariable *) val;
    var->setExternallyInitialized(true);
    var->setConstant(false);
    sym->userdata = var;
}

void emit_decl_foreignblock(IRContext *self, Decl *decl) {
    for (i64 i = 0; i < arrlen(decl->dforeign_block.decls); i++) {
        emit_decl_foreign(self, decl->dforeign_block.decls[i]);
    }
}

void emit_stmt(IRContext *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_VAR:
            if (!arrlen(self->fn)) return emit_decl_var_global(self, (Decl *) stmt);
            else return emit_decl_var(self, (Decl *) stmt);
        case (StmtKind) DECL_VAL: return emit_decl_val(self, (Decl *) stmt);
        case (StmtKind) DECL_IMPORT: return emit_decl_import(self, (Decl *) stmt);
        case (StmtKind) DECL_LIBRARY: return emit_decl_library(self, (Decl *) stmt);
        case (StmtKind) DECL_FOREIGN: return emit_decl_foreign(self, (Decl *) stmt);
        case (StmtKind) DECL_FOREIGN_BLOCK: return emit_decl_foreignblock(self, (Decl *) stmt);
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
        case DECL_FOREIGN_BLOCK: return emit_decl_foreignblock(self, decl);
        default:
            fatal("Unrecognized DeclKind %s", describe_ast_kind(decl->kind));
    }
}

void llvm_emit_stmt(IRContext *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_FILE: {
            if (!compiler.flags.debug) return;
            Source *file = ((Decl *) stmt)->dfile;
            self->dbg.source_file = file;
            self->dbg.file = self->dbg.builder->createFile(file->filename, self->package->path);
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
        case (StmtKind) DECL_LIBRARY:
        case (StmtKind) DECL_FOREIGN:
        case (StmtKind) DECL_FOREIGN_BLOCK:
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

    char object_name[MAX_PATH];
    package_object_path(package, object_name);

    std::error_code ec;
    raw_fd_ostream dest(object_name, ec);

    if (ec) {
        fatal("Could not open object file: %s\n", ec.message().c_str());
        return false;
    }

//    if (compiler.flags.dump_ir) {
//        std::string buf;
//        raw_string_ostream os(buf);
//        self->module->print(os, nullptr);
//        os.flush();
//        puts(buf.c_str());
//    }

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

    module_pm.add(createBasicAAWrapperPass());
    module_pm.add(createInstructionCombiningPass());
    module_pm.add(createAggressiveDCEPass());
    module_pm.add(createReassociatePass());
    module_pm.add(createPromoteMemoryToRegisterPass());

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

    if (compiler.flags.dump_ir) {
        std::string buf;
        raw_string_ostream os(buf);
        self->module->print(os, nullptr);
        os.flush();
        puts(buf.c_str());
    }

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

    return false; // no error
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
    val->print(os);
    os.flush();
    puts(buf.c_str());
}

void print(Type *ty) {
    std::string buf;
    raw_string_ostream os(buf);
    ty->print(os);
    os.flush();
    puts(buf.c_str());
}

void print(BasicBlock *bb) {
    std::string buf;
    raw_string_ostream os(buf);
    bb->print(os);
    os.flush();
    puts(buf.c_str());
}

void print(Metadata *metadata) {
    std::string buf;
    raw_string_ostream os(buf);
    metadata->print(os);
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
