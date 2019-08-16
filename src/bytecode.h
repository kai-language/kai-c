#pragma once

// requires nothing

// checker.h
typedef union Val Val;

typedef struct BCOperand BCOperand;
struct BCOperand {
    bool is_immediate;
    Val val;
};

typedef struct BCBlock BCBlock;
struct BCBlock {
    u8 *code;
};

typedef struct BCBuilder BCBuilder;
struct BCBuilder {
    BCBlock *block;
};

typedef struct VM VM;
struct VM {
    u8 *code;
    u8 *ip;
    Val *registers;
    Val *stack;
    u64 flgs;
};

typedef u32 Reg;

#define RZ0 ((BCOperand) {0})
#define RIP 0x1
#define RFP 0x2
#define RSP 0x3

void e_hlt (BCBuilder *b);
void e_nop (BCBuilder *b);
void e_add (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_addf(BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_sub (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_subf(BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_mul (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_mulf(BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_div (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_divf(BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_mod (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_xor (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_and (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_or  (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_shl (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_shr (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_ld1 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_ld2 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_ld4 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_ld8 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_st1 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_st2 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_st4 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_st8 (BCBuilder *b, Reg x, BCOperand y, BCOperand z);
void e_mov (BCBuilder *b, Reg y, BCOperand z);
void e_ftoi(BCBuilder *b, Reg y, BCOperand z);
void e_itof(BCBuilder *b, Reg y, BCOperand z);
void e_push(BCBuilder *b, BCOperand z);
void e_pop (BCBuilder *b, Reg z);
void e_call(BCBuilder *b, BCOperand z);
void e_ret (BCBuilder *b);
void e_cmp (BCBuilder *b, BCOperand y, BCOperand z);
void e_jmp (BCBuilder *b, BCOperand z);
void e_je  (BCBuilder *b, BCOperand z);
void e_jne (BCBuilder *b, BCOperand z);
void e_jl  (BCBuilder *b, BCOperand z);
void e_jle (BCBuilder *b, BCOperand z);
void e_jg  (BCBuilder *b, BCOperand z);
void e_jge (BCBuilder *b, BCOperand z);

BCOperand imm(u64 val);
BCOperand imf(f64 val);
BCOperand reg(u32 reg);

void vm_init(VM *vm, u8 *code, u32 highest_register);
void vm_interp(VM *vm);
void vm_dump(VM *vm);

