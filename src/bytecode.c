
#include "all.h"
#include "bytecode.h"
#include "ast.h"
#include "types.h"
#include "arena.h"
#include "package.h"
#include "checker.h"

#define R0  0x0
#define R1  0x1
#define R2  0x2
#define R3  0x3
#define R4  0x4
#define RBP 0x5
#define RSP 0x6
#define RIP 0x7

#define F0  0x0
#define F1  0x1
#define F2  0x2
#define F3  0x3
#define F4  0x4
#define F5  0x5
#define F6  0x6
#define F7  0x7

#define FIXUP 0xF1F1F1F1

/*
 +--------+---------------------------------------------------------------+
 | Kind   |31                           16    13    10     7             0|
 +--------+-------------------------------+-----+-----+-----+-------------+
 | iABCDx |             Dx 16             | C 3 | B 3 | A 3 |     op7     |
 +--------+-------------------------------+-----+-----+-----+-------------+
 | iABCx  |                Cx 19                | B 3 | A 3 |     op7     |
 +--------+-------------------------------------+-----+-----+-------------+
 | iABx   |                   Bx 22                   | A 3 |     op7     |
 +--------+-------------------------------------------+-----+-------------+
 | iAx    |                      Ax 25                      |     op7     |
 +--------+-------------------------------------------------+-------------+
 */

typedef enum Opcode Opcode;
enum Opcode {
//| Name | Opcode | Format | Description                          |
//+------+--------+--------+--------------------------------------+
    HLT  = 0x00, // iAx
    ADD  = 0x02, // iABCDx   r(A) = r(B) + r(C) + Dx
    ADDF = 0x03, // iABCx    f(A) = f(B) + f(C)
    SUB  = 0x04, // iABCDx   r(A) = r(B) - r(C) - Dx
    SUBF = 0x05, // iABCx    f(A) = f(B) - f(C)
    MUL  = 0x06, // iABCDx   r(A) = r(B) * (r(C) + Dx)
    MULF = 0x07, // iABCx    f(A) = f(B) * f(C)
    DIV  = 0x08, // iABCDx   r(A) = r(B) / (r(C) + Dx)
    DIVF = 0x09, // iABCx    f(A) = f(B) / f(C)
    MOD  = 0x0A, // iABCDx   r(A) = r(B) % (r(C) + Dx)
    XOR  = 0x0B, // iABCDx   r(A) = r(B) ^ (r(C) + Dx)
    SHL  = 0x0C, // iABCDx   r(A) = r(B) << (r(C) + Dx)
    SHR  = 0x0D, // iABCDx   r(A) = r(B) >> (r(C) + Dx)
    AND  = 0x0E, // iABCx    r(A) = r(B) & r(C)
    OR   = 0x0F, // iABCx    r(A) = r(B) | r(C)

    LD1  = 0x10, // iABCx    r(A) = *(r(B) + Cx)
    ST1  = 0x11, // iABCx    *(r(A) + Cx) = r(B)
    LD2  = 0x12, // iABCx    r(A) = *(r(B) + Cx)
    ST2  = 0x13, // iABCx    *(r(A) + Cx) = r(B)
    LD4  = 0x14, // iABCx    r(A) = *(r(B) + Cx)
    ST4  = 0x15, // iABCx    *(r(A) + Cx) = r(B)
    LD8  = 0x16, // iABCx    r(A) = *(r(B) + Cx)
    ST8  = 0x17, // iABCx    *(r(A) + Cx) = r(B)
    LDI  = 0x18, // iABx     r(A) = Bx
    STI  = 0x19, // iABx     *(A) = Bx
    LDF4 = 0x1A, // iABCx    f(A) = *(r(B) + Cx)
    LDF8 = 0x1B, // iABCx    f(A) = *(r(B) + Cx)
    STF4 = 0x1C, // iABCx    *(r(A) + Cx) = f(B)
    STF8 = 0x1D, // iABCx    *(r(A) + Cx) = f(B)

    MOV  = 0x20, // iABCDx   (C ? r : f)(A) = (D ? r : f)(B)
    FTOI = 0x21, // iABx     r(A) = (u64) f(B)
    ITOF = 0x22, // iABx     f(A) = (f64) r(B)
    PUSH = 0x23, // iABx     *(rsp) = r(A); rsp += 1
    POP  = 0x24, // iAx      r(A) = *(rsp); rsp -= 1
    CALL = 0x25, // iAx      *(rsp) = rip; rsp += 1; rip += Ax
    RET  = 0x26, // iAx      rsp += 1; rip = *(rsp)
    CMP  = 0x27, // iABCx    flgs = r(A) - r(B) - Cx

    JMP  = 0x30, // iAx      rip += Ax
    JE   = 0x31, // iAx      if(flgs == 0) rip += Ax
    JNE  = 0x32, // iAx      if(flgs != 0) rip += Ax
    JL   = 0x33, // iAx      if(flgs <  0) rip += Ax
    JLE  = 0x34, // iAx      if(flgs <= 0) rip += Ax
    JG   = 0x35, // iAx      if(flgs >  0) rip += Ax
    JGE  = 0x36, // iAx      if(flgs >= 0) rip += Ax

    AST  = 0x3F, // iAx
};

// size and position of opcode arguments.
#define SIZE_OP       7
#define SIZE_A        3
#define SIZE_B        3
#define SIZE_C        3
#define SIZE_Dx       16
#define SIZE_Cx       19
#define SIZE_Bx       22
#define SIZE_Ax       25

#define POS_OP        0
#define POS_A        (POS_OP + SIZE_OP)
#define POS_B        (POS_A + SIZE_A)
#define POS_C        (POS_B + SIZE_B)
#define POS_D        (POS_C + SIZE_C)

#define MAX_A        ((1<<SIZE_A)-1)
#define MAX_B        ((1<<SIZE_B)-1)
#define MAX_C        ((1<<SIZE_C)-1)
#define MAX_D        ((1<<SIZE_Dx)-1)
#define MAX_Ax       ((1<<SIZE_Ax)-1)
#define MAX_Bx       ((1<<SIZE_Bx)-1)
#define MAX_Cx       ((1<<SIZE_Cx)-1)
#define MAX_Dx       ((1<<SIZE_Dx)-1)

// creates a mask with 'n' 1 bits at position 'p'
#define MASK1(n,p)    ((~((~(u32)0)<<(n)))<<(p))
// creates a mask with 'n' 0 bits at position 'p'
#define MASK0(n,p)    (~MASK1(n,p))

#define iABCDx(o,a,b,c,d)         (o << POS_OP) \
                                | (a << POS_A)  \
                                | (b << POS_B)  \
                                | (c << POS_C)  \
                                | (d << POS_D)

#define iABCx(o,a,b,c)            (o << POS_OP) \
                                | (a << POS_A)  \
                                | (b << POS_B)  \
                                | (c << POS_C)

#define iABx(o,a,b)               (o << POS_OP) \
                                | (a << POS_A)  \
                                | (b << POS_B)

#define iAx(o,a)                  (o << POS_OP) \
                                | (a << POS_A)

void gen_bytecode_expr(BytecodeGenerator *self, Expr *expr, u8 ra);
void instr_str(BytecodeGenerator *self, u32 instr, char *buf, u64 buf_len);

u8 num_imm_bits[] = {
    [HLT]  = SIZE_Ax,
    [ADD]  = SIZE_Dx,
    [ADDF] = SIZE_Dx,
    [SUB]  = SIZE_Dx,
    [SUBF] = SIZE_Dx,
    [MUL]  = SIZE_Cx,
    [MULF] = SIZE_Cx,
    [DIV]  = SIZE_Cx,
    [DIVF] = SIZE_Cx,
    [MOD]  = SIZE_Cx,
    [XOR]  = SIZE_Cx,
    [SHL]  = SIZE_Dx,
    [SHR]  = SIZE_Dx,

    [LD1]  = SIZE_Dx,
    [LD2]  = SIZE_Dx,
    [LD4]  = SIZE_Dx,
    [LD8]  = SIZE_Dx,
    [LDI]  = SIZE_Bx,

    [ST1]  = SIZE_Cx,
    [ST2]  = SIZE_Cx,
    [ST4]  = SIZE_Cx,
    [ST8]  = SIZE_Cx,
    [STI]  = SIZE_Cx,
    [STF4] = SIZE_Cx,
    [STF8] = SIZE_Cx,

    [MOV]  = SIZE_Bx,
    [PUSH] = SIZE_Bx,
    [POP]  = SIZE_Ax,
    [CALL] = SIZE_Ax,
    [RET]  = SIZE_Ax,

    [CMP]  = SIZE_Cx,

    [JMP]  = SIZE_Ax,
    [JE]   = SIZE_Ax,
    [JNE]  = SIZE_Ax,
    [JL]   = SIZE_Ax,
    [JLE]  = SIZE_Ax,
    [JG]   = SIZE_Ax,
    [JGE]  = SIZE_Ax,

    [AST] = SIZE_Ax,
};

u32 *encode(BytecodeGenerator *self, u32 instr) {
    arrput(self->instructions, instr);
    return &arrlast(self->instructions);
}

u32 *e_hlt(BytecodeGenerator *self, u8 rd, u32 imm) {
    return encode(self, iAx(HLT, 0));
}

u32 *e_add(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(ADD, ra, rb, rc, dx));
}

u32 *e_addf(BytecodeGenerator *self, u8 fa, u8 fb, u8 fc) {
    return encode(self, iABCx(ADDF, fa, fb, fc));
}

u32 *e_sub(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(SUB, ra, rb, rc, dx));
}

u32 *e_subf(BytecodeGenerator *self, u8 fa, u8 fb, u8 fc) {
    return encode(self, iABCx(SUBF, fa, fb, fc));
}

u32 *e_mul(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(MUL, ra, rb, rc, dx));
}

u32 *e_mulf(BytecodeGenerator *self, u8 fa, u8 fb, u8 fc) {
    return encode(self, iABCx(MULF, fa, fb, fc));
}

u32 *e_div(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(DIV, ra, rb, rc, dx));
}

u32 *e_divf(BytecodeGenerator *self, u8 fa, u8 fb, u8 fc) {
    return encode(self, iABCx(DIVF, fa, fb, fc));
}

u32 *e_mod(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(MOD, ra, rb, rc, dx));
}

u32 *e_xor(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(XOR, ra, rb, rc, dx));
}

u32 *e_shl(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(SHL, ra, rb, rc, dx));
}

u32 *e_shr(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc, u16 dx) {
    return encode(self, iABCDx(SHR, ra, rb, rc, dx));
}

u32 *e_and(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc) {
    return encode(self, iABCx(AND, ra, rb, rc));
}

u32 *e_or(BytecodeGenerator *self, u8 ra, u8 rb, u8 rc) {
    return encode(self, iABCx(OR, ra, rb, rc));
}

u32 *e_ld1(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(LD1, ra, rb, cx));
}

u32 *e_ld2(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(LD2, ra, rb, cx));
}

u32 *e_ld4(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(LD4, ra, rb, cx));
}

u32 *e_ld8(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(LD8, ra, rb, cx));
}

u32 *e_ldi(BytecodeGenerator *self, u8 ra, u32 bx) {
    ASSERT(bx <= MAX_Bx);
    return encode(self, iABx(LDI, ra, bx));
}

u32 *e_st1(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(ST1, ra, rb, cx));
}

u32 *e_st2(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(ST2, ra, rb, cx));
}

u32 *e_st4(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(ST4, ra, rb, cx));
}

u32 *e_st8(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(ST8, ra, rb, cx));
}

u32 *e_ldf4(BytecodeGenerator *self, u8 fa, u8 rb, i32 cx) {
    return encode(self, iABCx(LDF4, fa, rb, cx));
}

u32 *e_ldf8(BytecodeGenerator *self, u8 fa, u8 rb, i32 cx) {
    return encode(self, iABCx(LDF8, fa, rb, cx));
}

u32 *e_stf4(BytecodeGenerator *self, u8 ma, u8 fb, i32 cx) {
    return encode(self, iABCx(STF4, ma, fb, cx));
}

u32 *e_stf8(BytecodeGenerator *self, u8 ma, u8 fb, i32 cx) {
    return encode(self, iABCx(STF8, ma, fb, cx));
}

u32 *e_mov(BytecodeGenerator *self, u8 ra, u8 rb, u8 c, u8 d) {
    return encode(self, iABCDx(MOV, ra, rb, c, d));
}

u32 *e_ftoi(BytecodeGenerator *self, u8 ra, u8 fb) {
    return encode(self, iABx(FTOI, ra, fb));
}

u32 *e_itof(BytecodeGenerator *self, u8 fa, u8 rb) {
    return encode(self, iABx(ITOF, fa, rb));
}

u32 *e_push(BytecodeGenerator *self, u8 ra) {
    return encode(self, iAx(PUSH, ra));
}

u32 *e_pop(BytecodeGenerator *self, u8 ra) {
    return encode(self, iAx(POP, ra));
}

u32 *e_call(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(CALL, ax));
}

u32 *e_ret(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(RET, ax));
}

u32 *e_cmp(BytecodeGenerator *self, u8 ra, u8 rb, i32 cx) {
    return encode(self, iABCx(CMP, ra, rb, cx));
}

u32 *e_jmp(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JMP, ax));
}

u32 *e_je(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JE, ax));
}

u32 *e_jne(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JNE, ax));
}

u32 *e_jl(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JL, ax));
}

u32 *e_jle(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JLE, ax));
}

u32 *e_jg(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JG, ax));
}

u32 *e_jge(BytecodeGenerator *self, i32 ax) {
    return encode(self, iAx(JGE, ax));
}

u32 *e_ast(BytecodeGenerator *self, void *ast) {
    u64 offset = ((u8*)ast - (u8*)self->package) >> 3;
    ASSERT(offset < MAX_Ax);
    return encode(self, iAx(AST, (u32) offset));
}

u64 bytecode_store_data(BytecodeGenerator *self, Operand operand) {
    if (operand.flags&TYPE) {
        return 0; // TODO: Type table
    }
    arrsetcap(self->data, 1024); // TMP
    u64 offset = arrlen(self->data);
    u64 size_including_alignment = ALIGN_UP(operand.type->size, operand.type->align);
    switch (operand.type->kind) {
        case TYPE_BOOL:
        case TYPE_INT:
        case TYPE_ENUM:
        case TYPE_FLOAT:
        case TYPE_PTR: {
            arrsetlen(self->data, offset + size_including_alignment);
            memset(self->data + offset, 0, size_including_alignment);
            memcpy(self->data + offset, &operand.val.u, size_including_alignment);
            return offset;
        }
        case TYPE_FUNC: {
            return 0;
        }
        case TYPE_ARRAY: {
            u64 size = operand.type->tarray.length * operand.type->tarray.eltype->align;
            arrsetlen(self->data, offset + size_including_alignment);
            memcpy(self->data + offset, operand.val.p, size);
            return offset;
        }
        case TYPE_SLICE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ANY: fatal("Unimplement constant emission in bytecode");
        default: fatal("Unhandled type kind %d", operand.type->kind);
    }
}

void bytecode_const_operand(BytecodeGenerator *self, Operand operand, u8 ra) {
    ASSERT(operand.flags&CONST);
    switch (operand.type->kind) {
        case TYPE_BOOL: e_ldi(self, ra, (u32) operand.val.u); break;
        case TYPE_INT:
        case TYPE_ENUM: {
            if (operand.val.u <= MAX_Bx) {
                e_ldi(self, ra, (u32) operand.val.u);
            } else if (operand.val.u <= UINT32_MAX) {
                u32 *instr = e_ld4(self, ra, R0, FIXUP);
                arrput(self->fixups, (BCSym){ GLOBAL });
                BCSym *sym = &arrlast(self->fixups);
                arrput(sym->fixups, instr);
                sym->offset_in_data = bytecode_store_data(self, operand);
            } else {
                u32 *instr = e_ld8(self, ra, R0, FIXUP);
                BCSym *sym = &arrlast(self->fixups);
                arrput(sym->fixups, instr);
                sym->offset_in_data = bytecode_store_data(self, operand);
            }
            break;
        }
        case TYPE_FLOAT: {
            u32 *instr = e_ldf8(self, ra, R0, FIXUP);
            arrput(self->fixups, (BCSym){ GLOBAL });
            BCSym *sym = &arrlast(self->fixups);
            arrput(sym->fixups, instr);
            sym->offset_in_data = bytecode_store_data(self, operand);
            break;
        }
        case TYPE_PTR:
        case TYPE_FUNC:
        case TYPE_ARRAY: {
            u32 *instr = e_ld8(self, ra, R0, FIXUP);
            arrput(self->fixups, (BCSym){ GLOBAL });
            BCSym *sym = &arrlast(self->fixups);
            arrput(sym->fixups, instr);
            sym->offset_in_data = bytecode_store_data(self, operand);
            break;
        }
        case TYPE_SLICE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ANY: {
            u32 *instr = e_ld8(self, ra, R0, FIXUP);
            arrput(self->fixups, (BCSym){ GLOBAL });
            BCSym *sym = &arrlast(self->fixups);
            arrput(sym->fixups, instr);
            sym->offset_in_data = bytecode_store_data(self, operand);
            break;
        }
        default: fatal("Unhandled type kind %d", operand.type->kind);
    }
}

void bytecode_stack_sym(BytecodeGenerator *self, Sym *sym) {
    ASSERT(sym->kind != SYM_ARG);
    BCStackFrame *frame = &arrlast(self->frames);
    BCSym *prev = NULL;
    BCSym *bcsym = arena_alloc(&self->package->arena, sizeof *bcsym);
    bcsym->kind = STACK;
    if (frame->syms) prev = arrlast(frame->syms);
    bcsym->frame_offset = prev ? prev->frame_offset + 8 : 8;
}

void bytecode_param_sym(BytecodeGenerator *self, Sym *sym) {
    BCStackFrame *frame = &arrlast(self->frames);
    BCSym *prev = NULL;
    BCSym *bcsym = arena_alloc(&self->package->arena, sizeof *bcsym);
    bcsym->kind = STACK;
    if (frame->args) prev = arrlast(frame->syms);
    bcsym->frame_offset = prev ? prev->frame_offset - 8 : -8;
    sym->userdata = bcsym;
}

void gen_bytecode_constant_fixup(BytecodeGenerator *self, Expr *expr, u32 *instr) {
    arrput(self->fixups, (BCSym){ GLOBAL });
    BCSym *sym = &arrlast(self->fixups);
    arrput(sym->fixups, instr);
    Operand op = hmget(self->package->operands, expr);
    sym->offset_in_data = bytecode_store_data(self, op);
}

void gen_bytecode_expr_nil(BytecodeGenerator *self, Expr *expr, u8 ra) {
    e_ast(self, expr);
    e_xor(self, ra, ra, ra, ra);
}

void gen_bytecode_expr_int(BytecodeGenerator *self, Expr *expr, u8 ra) {
    e_ast(self, expr);
    if (expr->eint <= MAX_Bx) {
        e_ldi(self, ra, (u32) expr->eint);
    } else {
        u32 *instr = e_ld8(self, ra, R0, FIXUP);
        arrput(self->fixups, (BCSym){ GLOBAL });
        BCSym *sym = &arrlast(self->fixups);
        arrput(sym->fixups, instr);
        Operand op = hmget(self->package->operands, expr);
        sym->offset_in_data = bytecode_store_data(self, op);
    }
}

void gen_bytecode_expr_float(BytecodeGenerator *self, Expr *expr, u8 fa) {
    e_ast(self, expr);
    u32 *instr = e_ldf8(self, fa, R0, FIXUP);
    gen_bytecode_constant_fixup(self, expr, instr);
}

void gen_bytecode_expr_str(BytecodeGenerator *self, Expr *expr, u8 ra) {
    e_ast(self, expr);
    u32 *instr = e_ld8(self, ra, R0, FIXUP);
    gen_bytecode_constant_fixup(self, expr, instr);
}

void gen_bytecode_expr_name(BytecodeGenerator *self, Expr *expr, u8 ra) {
    e_ast(self, expr);
    Operand op = hmget(self->package->operands, expr);
    if (op.flags&CONST) {
        bytecode_const_operand(self, op, ra);
        return;
    }
    Sym *sym = op.val.p;
    BCSym *bcsym = sym->userdata;
    if (!bcsym) {

    }
}

void gen_bytecode_expr_compound(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_paren(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_unary(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_binary(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_ternary(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_call(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_field(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_index(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_func(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_func_type(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_array(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_pointer(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_struct(BytecodeGenerator *self, Expr *expr, u8 ra) {}
void gen_bytecode_expr_union(BytecodeGenerator *self, Expr *expr, u8 ra) {}

void gen_bytecode_expr(BytecodeGenerator *self, Expr *expr, u8 ra) {
    switch (expr->kind) {
        case EXPR_NIL:      gen_bytecode_expr_nil(self, expr, ra); break;
        case EXPR_INT:      gen_bytecode_expr_int(self, expr, ra); break;
        case EXPR_FLOAT:    gen_bytecode_expr_float(self, expr, ra); break;
        case EXPR_STR:      gen_bytecode_expr_str(self, expr, ra); break;
        case EXPR_NAME:     gen_bytecode_expr_name(self, expr, ra); break;
        case EXPR_COMPOUND: gen_bytecode_expr_compound(self, expr, ra); break;
//        case EXPR_CAST:     gen_bytecode_expr_cast(self, expr, ra); break;
        case EXPR_PAREN:    gen_bytecode_expr_paren(self, expr, ra); break;
        case EXPR_UNARY:    gen_bytecode_expr_unary(self, expr, ra); break;
        case EXPR_BINARY:   gen_bytecode_expr_binary(self, expr, ra); break;
        case EXPR_TERNARY:  gen_bytecode_expr_ternary(self, expr, ra); break;
        case EXPR_CALL:     gen_bytecode_expr_call(self, expr, ra); break;
        case EXPR_FIELD:    gen_bytecode_expr_field(self, expr, ra); break;
        case EXPR_INDEX:    gen_bytecode_expr_index(self, expr, ra); break;
        case EXPR_FUNC:     gen_bytecode_expr_func(self, expr, ra); break;
        case EXPR_FUNCTYPE: gen_bytecode_expr_func_type(self, expr, ra); break;
        case EXPR_ARRAY:    gen_bytecode_expr_array(self, expr, ra); break;
        case EXPR_POINTER:  gen_bytecode_expr_pointer(self, expr, ra); break;
        case EXPR_STRUCT:   gen_bytecode_expr_struct(self, expr, ra); break;
        case EXPR_UNION:    gen_bytecode_expr_union(self, expr, ra); break;
//        case EXPR_ENUM:     gen_bytecode_expr_enum(self, expr, ra); break;
        default: fatal("Unrecognized ExprKind %p", expr->kind);
    }
}

void gen_bytecode_decl_val(BytecodeGenerator *self, Decl *decl) {
    e_ast(self, decl);
    Sym *sym = hmget(self->package->symdecls, decl->dval.name);
    arrput(self->fixups, (BCSym) { GLOBAL });
    BCSym *bcsym = &arrlast(self->fixups);
    sym->userdata = bcsym;
    Operand op = hmget(self->package->operands, decl->dval.val);
    bcsym->offset_in_data = bytecode_store_data(self, op);
}

void gen_bytecode_decl_var(BytecodeGenerator *self, Decl *decl) {
    e_ast(self, decl);
    for (i64 i = 0; i < arrlen(decl->dvar.names); i++) {
        Sym *sym = hmget(self->package->symdecls, decl->dvar.names[i]);
        BCSym *bcsym = arena_alloc(&self->package->arena, sizeof *bcsym);
        if (self->frames) {
            bcsym->kind = STACK;
            BCStackFrame *frame = &arrlast(self->frames);
            arrput(frame->syms, bcsym);
        } else {
            bytecode_stack_sym(self, sym);
        }
    }
}

void gen_bytecode_decl_foreign(BytecodeGenerator *self, Decl *decl) {}
void gen_bytecode_decl_foreign_block(BytecodeGenerator *self, Decl *decl) {}
void gen_bytecode_stmt_label(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_assign(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_return(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_defer(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_goto(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_block(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_if(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_for(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt_switch(BytecodeGenerator *self, Stmt *stmt) {}
void gen_bytecode_stmt(BytecodeGenerator *self, Stmt *stmt) {
    switch (stmt->kind) {
        case (StmtKind) DECL_VAL:          gen_bytecode_decl_val(self, (Decl *) stmt); break;
        case (StmtKind) DECL_VAR:          gen_bytecode_decl_var(self, (Decl *) stmt); break;
        case (StmtKind) DECL_FOREIGN:      gen_bytecode_decl_foreign(self, (Decl *) stmt); return;
        case (StmtKind) DECL_FOREIGNBLOCK: gen_bytecode_decl_foreign_block(self, (Decl *) stmt); return;
        case (StmtKind) DECL_IMPORT:       return;
        case STMT_LABEL:                   gen_bytecode_stmt_label(self, stmt); return;
        case STMT_ASSIGN:                  gen_bytecode_stmt_assign(self, stmt); return;
        case STMT_RETURN:                  gen_bytecode_stmt_return(self, stmt); return;
        case STMT_DEFER:                   gen_bytecode_stmt_defer(self, stmt); return;
        case STMT_USING:                   return;
        case STMT_GOTO:                    gen_bytecode_stmt_goto(self, stmt); return;
        case STMT_BLOCK:                   gen_bytecode_stmt_block(self, stmt); return;
        case STMT_IF:                      gen_bytecode_stmt_if(self, stmt); return;
        case STMT_FOR:                     gen_bytecode_stmt_for(self, stmt); return;
        case STMT_SWITCH:                  gen_bytecode_stmt_switch(self, stmt); return;
        default:
            if (stmt->kind < STMT_KIND_BASE) {
                gen_bytecode_expr(self, (Expr *) stmt, R1);
                return;
            }
            fatal("Unrecognized StmtKind %p", stmt->kind);
    }
    u32 instr = arrlast(self->instructions);
    char buf[1024];
    instr_str(self, instr, buf, 1024);
    printf("%s\n", buf);
}

const char *op[] = {
    [HLT]  = "hlt",
    [ADD]  = "add",
    [ADDF] = "addf",
    [SUB]  = "sub",
    [SUBF] = "subf",
    [MUL]  = "mul",
    [MULF] = "mulf",
    [DIV]  = "div",
    [DIVF] = "divf",
    [MOD]  = "mod",
    [XOR]  = "xor",
    [SHL]  = "shl",
    [SHR]  = "shr",
    [AND]  = "and",
    [OR]   = "or",

    [LD1]  = "ld1",
    [ST1]  = "st1",
    [LD2]  = "ld2",
    [ST2]  = "st2",
    [LD4]  = "ld4",
    [ST4]  = "st4",
    [LD8]  = "ld8",
    [ST8]  = "st8",
    [LDI]  = "ldi",
    [STI]  = "sti",
    [LDF4] = "ldf4",
    [LDF8] = "ldf8",
    [STF4] = "stf4",
    [STF8] = "stf8",

    [MOV]  = "mov",
    [FTOI] = "ftoi",
    [ITOF] = "itof",
    [PUSH] = "push",
    [POP]  = "pop",
    [CALL] = "call",
    [RET]  = "ret",
    [CMP]  = "cmp",

    [JMP]  = "jmp",
    [JE]   = "je",
    [JNE]  = "jne",
    [JL]   = "jl",
    [JLE]  = "jle",
    [JG]   = "jg",
    [JGE]  = "jge",

    [AST] = "ast",
};

#define MS(I, N, P) (((I)&MASK1(N, P)) >> (P))
const char *gr(u32 instr, u32 pos) {
    const char *gr[] = {
        [R0]  = "r0",
        [R1]  = "r1",
        [R2]  = "r2",
        [R3]  = "r3",
        [R4]  = "r4",
        [RBP] = "rbp",
        [RSP] = "rsp",
        [RIP] = "rip",
    };
    u32 reg = MS(instr, SIZE_A, pos);
    return gr[reg];
}

const char *fr(u32 instr, u32 pos) {
    static const char *fr[] = {
        [F0] = "f0",
        [F1] = "f1",
        [F2] = "f2",
        [F3] = "f3",
        [F4] = "f4",
        [F5] = "f5",
        [F6] = "f6",
        [F7] = "f7",
    };
    u32 reg = MS(instr, SIZE_A, pos);
    return fr[reg];
}

i32 imm(u32 instr, u32 size) {
    return MS(instr, size, 32 - size);
}

void instr_str(BytecodeGenerator *self, u32 instr, char *buf, u64 buf_len) {
    u32 opcode = instr & MASK1(SIZE_OP, POS_OP);
    switch (opcode) {
        case HLT:  // iAx
            snprintf(buf, buf_len, "%s", op[opcode]);
            return;
        case ADD:  // iABCDx   r(A) = r(B) + r(C) + Dx
        case SUB:  // iABCDx   r(A) = r(B) - r(C) - Dx
        case MUL:  // iABCDx   r(A) = r(B) * (r(C) + Dx)
        case DIV:  // iABCDx   r(A) = r(B) / (r(C) + Dx)
        case MOD:  // iABCDx   r(A) = r(B) % (r(C) + Dx)
        case XOR:  // iABCDx   r(A) = r(B) ^ (r(C) + Dx)
        case SHL:  // iABCDx   r(A) = r(B) << (r(C) + Dx)
        case SHR:  // iABCDx   r(A) = r(B) >> (r(C) + Dx)
            snprintf(buf, buf_len, "%s %s, %s, %s, #%d", op[opcode],
                     gr(instr, POS_A), gr(instr, POS_B), gr(instr, POS_B), imm(instr, SIZE_Dx));
            return;
        case AND:  // iABCx    r(A) = r(B) & r(C)
        case OR:   // iABCx    r(A) = r(B) | r(C)
            snprintf(buf, buf_len, "%s %s, %s, %s", op[opcode], 
                gr(instr, POS_A), gr(instr, POS_B), gr(instr, POS_C));
            return;
        case ADDF: // iABCx    f(A) = f(B) + f(C)
        case SUBF: // iABCx    f(A) = f(B) - f(C)
        case MULF: // iABCx    f(A) = f(B) * f(C)
        case DIVF: // iABCx    f(A) = f(B) / f(C)
            snprintf(buf, buf_len, "%s %s, %s, %s", op[opcode], 
                fr(instr, POS_A), fr(instr, POS_B), fr(instr, POS_C));
            return;
        case LD1:  // iABCx    r(A) = *(r(B) + Cx)
        case ST1:  // iABCx    *(r(A) + Cx) = r(B)
        case LD2:  // iABCx    r(A) = *(r(B) + Cx)
        case ST2:  // iABCx    *(r(A) + Cx) = r(B)
        case LD4:  // iABCx    r(A) = *(r(B) + Cx)
        case ST4:  // iABCx    *(r(A) + Cx) = r(B)
        case LD8:  // iABCx    r(A) = *(r(B) + Cx)
        case ST8:  // iABCx    *(r(A) + Cx) = r(B)
            snprintf(buf, buf_len, "%s %s, %s, #%d", op[opcode],
                     gr(instr, POS_A), gr(instr, POS_B), imm(instr, SIZE_Cx));
            return;
        case LDF4: // iABCx    f(A) = *(r(B) + Cx)
        case LDF8: // iABCx    f(A) = *(r(B) + Cx)
            snprintf(buf, buf_len, "%s %s, %s, #%d", op[opcode],
                     fr(instr, POS_A), gr(instr, POS_B), imm(instr, SIZE_Cx));
            return;
        case LDI:  // iABx     r(A) = Bx
            snprintf(buf, buf_len, "%s #%d", op[opcode], imm(instr, SIZE_Bx));
            return;
        case STI:  // iABx     *(A) = Bx
            snprintf(buf, buf_len, "%s %s, #%d", op[opcode],
                     gr(instr, POS_A), imm(instr, SIZE_Bx));
            return;
        case STF4: // iABCx    *(r(A) + Cx) = f(B)
        case STF8: // iABCx    *(r(A) + Cx) = f(B)
        case CMP:  // iABCx    flgs = r(A) - r(B) - Cx
            snprintf(buf, buf_len, "%s %s, %s, #%d", op[opcode],
                     gr(instr, POS_A), gr(instr, POS_B), imm(instr, SIZE_Cx));
            return;
        case MOV:  // iABCDx   (C ? r : f)(A) = (D ? r : f)(B)
        case FTOI: // iABx     r(A) = (u64) f(B)
            snprintf(buf, buf_len, "%s %s, %s", op[opcode], gr(instr, POS_A), fr(instr, POS_B));
            return;
        case ITOF: // iABx     f(A) = (f64) r(B)
            snprintf(buf, buf_len, "%s %s, %s", op[opcode], fr(instr, POS_A), gr(instr, POS_B));
            return;
        case PUSH: // iABx     *(rsp) = r(A); rsp += 1
        case POP:  // iAx      r(A) = *(rsp); rsp -= 1
            snprintf(buf, buf_len, "%s %s", op[opcode], gr(instr, SIZE_Bx));
            return;
        case RET:  // iAx      rsp += 1; rip = *(rsp)
            snprintf(buf, buf_len, "%s", op[opcode]);
            return;
        case CALL: // iAx      *(rsp) = rip; rsp += 1; rip += Ax
        case JMP:  // iAx      rip += Ax
        case JE:   // iAx      if(flgs == 0) rip += Ax
        case JNE:  // iAx      if(flgs != 0) rip += Ax
        case JL:   // iAx      if(flgs <  0) rip += Ax
        case JLE:  // iAx      if(flgs <= 0) rip += Ax
        case JG:   // iAx      if(flgs >  0) rip += Ax
        case JGE:  // iAx      if(flgs >= 0) rip += Ax
            snprintf(buf, buf_len, "%s #%d", op[opcode], imm(instr, SIZE_Ax));
            return;
        case AST: { // iAx
            u32 offset = imm(instr, SIZE_Ax) << 3;
            Ast *ast = ((void*) self->package) + offset;
            const char *str = describe_ast(self->package, ast);
            char *term = strchr(str, '\n');
            for (int i = 0; i < 3 && term && *term; i++) {
                *term++ = '.';
            }
            if (term) *term = '\0';
            PosInfo pos = package_posinfo(self->package, ast->range.start);
            snprintf(buf, buf_len, "; %s (%s:%u:%u)",
                     describe_ast(self->package, ast),
                     pos.source->filename, pos.line, pos.column);
            return;
        }
    }
}
