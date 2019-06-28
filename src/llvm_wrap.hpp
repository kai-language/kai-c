#pragma once

// requires none

// package.h
typedef struct Package Package;

typedef struct IRFile IRFile;
typedef struct IRScope IRScope;
typedef struct IRLayout IRLayout;
typedef struct IRTarget IRTarget;
typedef struct IRModule IRModule;

typedef struct IRType IRType;
typedef struct IRStruct IRStruct;
typedef struct IRVoid IRVoid;
typedef struct IRInteger IRInteger;
typedef struct IRFloat IRFloat;
typedef struct IRArray IRArray;
typedef struct IRPointer IRPointer;
typedef struct IRFunction IRFunction;

typedef struct IRBlock IRBlock;
typedef struct IRAlloca IRAlloca;
typedef struct IRGlobal IRGlobal;

typedef struct IRValue IRValue;

typedef struct IRContext IRContext;

typedef struct IRFunctionArgs IRFunctionArgs;

typedef struct DBGType DBGType;
typedef struct DBGFunction DBGFunction;

#ifdef __cplusplus
extern "C" {
#endif

IRContext *llvm_backend_init(Package *pkg);

IRType *llvm_i1(IRContext *);
IRType *llvm_i8(IRContext *);
IRType *llvm_i16(IRContext *);
IRType *llvm_i32(IRContext *);
IRType *llvm_i64(IRContext *);
IRType *llvm_u8(IRContext *);
IRType *llvm_u16(IRContext *);
IRType *llvm_u32(IRContext *);
IRType *llvm_u64(IRContext *);
IRType *llvm_f32(IRContext *);
IRType *llvm_f64(IRContext *);
IRType *llvm_void(IRContext *);
IRType *llvm_integer(IRContext *, u64 bits);
IRType *llvm_float(IRContext *, u64 bits);
IRType *llvm_pointer(IRContext *, IRType *);
IRType *llvm_array(IRContext *, IRType *, u64 size);
IRType *llvm_function(IRContext *, IRType **params, IRType *result, bool vargs);
IRType *llvm_struct(IRContext *, IRType **elements);
IRFunctionArgs *llvm_function_args_start(IRContext *c, IRFunction *fn);
void llvm_function_args_end(IRContext *c, IRFunctionArgs *args);

IRValue *llvm_constant_null(IRContext *, IRType *);
IRValue *llvm_constant_float(IRContext *, IRType *, f64 val);
IRValue *llvm_constant_integer(IRContext *, IRType *, u64 val, bool is_signed);
IRValue *llvm_global_string_pointer(IRContext *, const char *string, u32 len);

IRValue *llvm_create_load(IRContext *, IRValue *p, u32 align);
IRValue *llvm_create_trunc_or_bitcast(IRContext *, IRValue *v, IRType *ty);
IRValue *llvm_create_ptr_to_int(IRContext *, IRValue *v, IRType *ty);
IRValue *llvm_create_select(IRContext *, IRValue *cond, IRValue *pass, IRValue *fail);
IRFunction *llvm_create_function(IRContext *, IRType *ty, const char *name);
IRBlock *llvm_create_block(IRContext *, const char *name, IRFunction *fn);
void llvm_set_insert_block(IRContext *, IRBlock *block);
void llvm_set_insert_instr(IRContext *, IRValue *instr);

IRValue *llvm_create_neg(IRContext *, IRValue *v);
IRValue *llvm_create_fneg(IRContext *, IRValue *v);
IRValue *llvm_create_not(IRContext *, IRValue *v);

IRValue *llvm_create_add(IRContext *,  IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fadd(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_sub (IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fsub(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_mul (IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fmul(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_sdiv(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_udiv(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fdiv(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_srem(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_urem(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_frem(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_and (IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_or  (IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_xor (IRContext *, IRValue *lhs, IRValue *rhs);

IRValue *llvm_create_ashr(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_lshr(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_shl (IRContext *, IRValue *lhs, IRValue *rhs);

IRValue *llvm_create_icmp_slt(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_ult(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_sgt(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_ugt(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_sle(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_ule(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_sge(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_uge(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fcmp_olt(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fcmp_ogt(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fcmp_ole(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fcmp_oge(IRContext *, IRValue *lhs, IRValue *rhs);

IRValue *llvm_create_icmp_eq(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_icmp_ne(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fcmp_eq(IRContext *, IRValue *lhs, IRValue *rhs);
IRValue *llvm_create_fcmp_ne(IRContext *, IRValue *lhs, IRValue *rhs);

void llvm_debug_set_pos(IRContext *, u32 line, u32 col);
void llvm_debug_unset_pos(IRContext *);
void llvm_debug_function(IRContext *, IRFunction *, DBGType *, const char *name, u32 line);
DBGType *llvm_debug_type_integer(IRContext *, bool is_signed, u32 size);
DBGType *llvm_debug_type_function(IRContext *, DBGType **params, DBGType *result, u32 num_params);

bool llvm_validate(IRContext *);

#ifdef __cplusplus
} // extern "C"
#endif

