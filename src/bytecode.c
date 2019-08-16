
#include "all.h"
#include "checker.h"
#include "bytecode.h"
#include "arena.h"
#include "package.h"

typedef struct BytecodeProgram BytecodeProgram;
struct BytecodeProgram {
    Package *package;
    u8 *code;
    u8 *data;
    u8 *rodata;
};

/*
 ┌──────────────────────────────────────────────────┐
 │0                     7 8        10       13      │
 ├───────────────────────┬────────┬────────┬────────┤
 │          op8          │   X    │   Y    │   Z    │
 └───────────────────────┴────────┴────────┴────────┘
 ┌──────────────────────────────────────────────────┐
 │                     Operand                      │
 ├────────────────┬─────────────────────────────────┤
 │    Op Mode     │              size               │
 ├───┬────────────┼─────────────────────────────────┤
 │ 0 │  register  │ bytes = 1 << (size - 1)         │
 ├───┼────────────┼─────────────────────────────────┤
 │ 1 │ immediate  │ bytes = 1 << size               │
 └───┴────────────┴─────────────────────────────────┘
 */

typedef enum Opcode Opcode;
enum Opcode {
//| Name | Opcode | Description                          |
//+------+--------+--------------------------------------+
    HLT  = 0x00,
    NOP  = 0x01,
    ADD  = 0x02, // X = Y + Z
    ADDF = 0x03,
    SUB  = 0x04, // X = Y - Z
    SUBF = 0x05,
    MUL  = 0x06, // X = Y * Z
    MULF = 0x07,
    DIV  = 0x08, // X = Y / Z
    DIVF = 0x09,
    MOD  = 0x0A, // X = Y % Z
    XOR  = 0x0B, // X = Y ^ Z
    AND  = 0x0C, // X = Y & Z
    OR   = 0x0D, // X = Y | Z
    SHL  = 0x0E, // X = Y << Z
    SHR  = 0x0F, // X = Y >> Z

    LD1  = 0x10, // X = *(Y + Z)
    ST1  = 0x11, // *(X + Y) = Z
    LD2  = 0x12,
    ST2  = 0x13,
    LD4  = 0x14,
    ST4  = 0x15,
    LD8  = 0x16,
    ST8  = 0x17,

    MOV  = 0x20, // Y = Z
    FTOI = 0x21, // Y = ftoi(Z)
    ITOF = 0x22, // Y = itof(Z)
    PUSH = 0x23, // *(rsp) = Z; rsp += 1
    POP  = 0x24, // Z = *(rsp); rsp -= 1
    CALL = 0x25, // *(rsp) = rip; rsp += 1; rip += Z
    RET  = 0x26, // rsp += 1; rip = *(rsp)
    CMP  = 0x27, // flgs = Y - Z

    JMP  = 0x30, // rip += Z
    JE   = 0x31, // if(flgs == 0) rip += Z
    JNE  = 0x32, // if(flgs != 0) rip += Z
    JL   = 0x33, // if(flgs <  0) rip += Z
    JLE  = 0x34, // if(flgs <= 0) rip += Z
    JG   = 0x35, // if(flgs >  0) rip += Z
    JGE  = 0x36, // if(flgs >= 0) rip += Z

    AST  = 0xA5,
};

u8 size_for_register(Reg reg) {
    u8 size = 0;
    if (reg > 0) size = 1;
    if (reg > UINT8_MAX)  size = 2;
    if (reg > UINT16_MAX) size = 3;
    return size;
}

u8 size_for_operand(BCOperand op) {
    if (!op.is_immediate) return size_for_register((u32) op.val.u);
    u8 size = 0;
    if (op.val.u > UINT8_MAX)  size = 1;
    if (op.val.u > UINT16_MAX) size = 2;
    if (op.val.u > UINT32_MAX) size = 3;
    return size;
}

void enc_register(BCBuilder *b, Reg reg) {
    u64 val = reg;
    if (!val) return;

    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    if (!val) return;

    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    if (!val) return;

    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    arrput(b->block->code, val & 0xff);
    val = val >> 8;
}

void enc_operand(BCBuilder *b, BCOperand op) {
    u64 val = op.val.u;
    if (!op.is_immediate && !val) return;
    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    if (!val) return;

    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    if (!val) return;

    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    if (!val) return;

    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    arrput(b->block->code, val & 0xff);
    val = val >> 8;
    arrput(b->block->code, val & 0xff);
}

void enc(BCBuilder *b, Opcode opcode, Reg x, BCOperand y, BCOperand z) {
    arrput(b->block->code, opcode);
    u8 op_x = size_for_register(x);
    u8 op_y = size_for_operand(y);
    u8 op_z = size_for_operand(z);
    if (y.is_immediate) op_y |= 0x4;
    if (z.is_immediate) op_z |= 0x4;
    u8 operands = (op_x << 6) | (op_y << 3) | op_z;
    arrput(b->block->code, operands);
    enc_register(b, x);
    enc_operand(b, y);
    enc_operand(b, z);
}

BCOperand imm(u64 val) {
    BCOperand op = { true, .val.u = val };
    return op;
}

BCOperand imf(f64 val) {
    BCOperand op = { true, .val.f = val };
    return op;
}

BCOperand reg(u32 reg) {
    BCOperand op = { false, .val.u = reg };
    return op;
}

void e_hlt (BCBuilder *b)                                  { arrput(b->block->code, HLT); }
void e_nop (BCBuilder *b)                                  { arrput(b->block->code, NOP); }
void e_add (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, ADD,  x, y, z); }
void e_addf(BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, ADDF, x, y, z); }
void e_sub (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, SUB,  x, y, z); }
void e_subf(BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, SUBF, x, y, z); }
void e_mul (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, MUL,  x, y, z); }
void e_mulf(BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, MULF, x, y, z); }
void e_div (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, DIV,  x, y, z); }
void e_divf(BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, DIVF, x, y, z); }
void e_mod (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, MOD,  x, y, z); }
void e_xor (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, XOR,  x, y, z); }
void e_and (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, AND,  x, y, z); }
void e_or  (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, OR,   x, y, z); }
void e_shl (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, SHL,  x, y, z); }
void e_shr (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, SHR,  x, y, z); }
void e_ld1 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, LD1,  x, y, z); }
void e_ld2 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, LD2,  x, y, z); }
void e_ld4 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, LD4,  x, y, z); }
void e_ld8 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, LD8,  x, y, z); }
void e_st1 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, ST1,  x, y, z); }
void e_st2 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, ST2,  x, y, z); }
void e_st4 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, ST4,  x, y, z); }
void e_st8 (BCBuilder *b, Reg x, BCOperand y, BCOperand z) { enc(b, ST8,  x, y, z); }
void e_mov (BCBuilder *b, Reg y, BCOperand z)              { enc(b, MOV,  0, reg(y), z); }
void e_ftoi(BCBuilder *b, Reg y, BCOperand z)              { enc(b, FTOI, 0, reg(y), z); }
void e_itof(BCBuilder *b, Reg y, BCOperand z)              { enc(b, ITOF, 0, reg(y), z); }
void e_push(BCBuilder *b, BCOperand z)                     { enc(b, PUSH, 0, RZ0, z); }
void e_pop (BCBuilder *b, Reg z)                           { enc(b, POP,  0, RZ0, reg(z)); }
void e_call(BCBuilder *b, BCOperand z)                     { enc(b, CALL, 0, RZ0, z); }
void e_ret (BCBuilder *b)                                  { arrput(b->block->code, RET); }
void e_cmp (BCBuilder *b, BCOperand y, BCOperand z)        { enc(b, CMP, 0, y, z); }
void e_jmp (BCBuilder *b, BCOperand z)                     { enc(b, JMP, 0, RZ0, z); }
void e_je  (BCBuilder *b, BCOperand z)                     { enc(b, JE,  0, RZ0, z); }
void e_jne (BCBuilder *b, BCOperand z)                     { enc(b, JNE, 0, RZ0, z); }
void e_jl  (BCBuilder *b, BCOperand z)                     { enc(b, JL,  0, RZ0, z); }
void e_jle (BCBuilder *b, BCOperand z)                     { enc(b, JLE, 0, RZ0, z); }
void e_jg  (BCBuilder *b, BCOperand z)                     { enc(b, JG,  0, RZ0, z); }
void e_jge (BCBuilder *b, BCOperand z)                     { enc(b, JGE, 0, RZ0, z); }

i32 reg_size[] = { 0, 1, 2, 4 };
i32 imm_size[] = { 1, 2, 4, 8 };
const char *reg_names[] = { "rzo", "rip", "rfp", "rsp" };

u64 read_bytes(i32 n, u8 *mem) {
    u64 val = 0;
    for (int i = 0; i < n; i++)
        val |= ((u64) mem[i] << (i * 8));
    return val;
}

void disasmreg(u64 reg) {
    if (reg < (sizeof(reg_names) / sizeof(*reg_names)))
        printf("%s ", reg_names[reg]);
    else
        printf("r%llu ", reg);
}

i32 disasm3p(const char *name, u8 *code) {
    u8 *start = code++;
    printf("%s ", name);

    u8 operands = code[0];
    code += 1;

    u8 x_size = (operands & 0xC0) >> 6;
    i32 bytes = reg_size[x_size];
    u64 val = read_bytes(bytes, code);
    code += bytes;
    disasmreg(val);

    bool y_is_immediate = (operands & 0x20) != 0;
    u8   y_size = (operands & 0x18) >> 3;
    bool z_is_immediate = (operands & 0x4) != 0;
    u8   z_size = (operands & 0x3);

    if (y_is_immediate) {
        i32 bytes = imm_size[y_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        printf("#%llu ", val);
    } else {
        i32 bytes = reg_size[y_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        disasmreg(val);
    }

    if (z_is_immediate) {
        i32 bytes = imm_size[z_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        printf("#%llu ", val);
    } else {
        i32 bytes = reg_size[z_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        disasmreg(val);
    }

    return (i32) (code - start);
}

i32 disasm2p(const char *name, u8 *code) {
    u8 *start = code++;
    printf("%s ", name);

    u8 operands = code[0];
    code += 1;

    bool y_is_immediate = (operands & 0x20) != 0;
    u8   y_size = (operands & 0x18) >> 3;
    bool z_is_immediate = (operands & 0x4) != 0;
    u8   z_size = (operands & 0x3);

    if (y_is_immediate) {
        i32 bytes = imm_size[y_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        printf("#%llu ", val);
    } else {
        i32 bytes = reg_size[y_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        disasmreg(val);
    }

    if (z_is_immediate) {
        i32 bytes = imm_size[z_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        printf("#%llu ", val);
    } else {
        i32 bytes = reg_size[z_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        disasmreg(val);
    }

    return (i32) (code - start);
}

i32 disasm1p(const char *name, u8 *code) {
    u8 *start = code++;
    printf("%s ", name);

    u8 operands = code[0];
    code += 1;

    bool z_is_immediate = (operands & 0x04) != 0;
    u8   z_size = operands & 0x3;

    if (z_is_immediate) {
        i32 bytes = imm_size[z_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        printf("#%llu ", val);
    } else {
        i32 bytes = reg_size[z_size];
        u64 val = read_bytes(bytes, code);
        code += bytes;
        disasmreg(val);
    }

    return (i32) (code - start);
}

i32 disasm0p(const char *name, u8 *code) {
    u8 *start = code++;
    printf("%s", name);
    return (i32) (code - start);
}

void disassemble(u8 *code, const char *name) {
    printf("== %s ==\n", name);
    for (i32 i = 0; i < arrlen(code);) {
        u8 instruction = code[i];
        switch (instruction) {
            case HLT:  i += disasm0p("hlt",  code + i); break;
            case NOP:  i += disasm0p("nop",  code + i); break;
            case ADD:  i += disasm3p("add",  code + i); break;
            case ADDF: i += disasm3p("addf", code + i); break;
            case SUB:  i += disasm3p("sub",  code + i); break;
            case SUBF: i += disasm3p("subf", code + i); break;
            case MUL:  i += disasm3p("mul",  code + i); break;
            case MULF: i += disasm3p("mulf", code + i); break;
            case DIV:  i += disasm3p("div",  code + i); break;
            case DIVF: i += disasm3p("divf", code + i); break;
            case MOD:  i += disasm3p("mod",  code + i); break;
            case XOR:  i += disasm3p("xor",  code + i); break;
            case AND:  i += disasm3p("and",  code + i); break;
            case OR:   i += disasm3p("or",   code + i); break;
            case SHL:  i += disasm3p("shl",  code + i); break;
            case SHR:  i += disasm3p("shr",  code + i); break;
            case LD1:  i += disasm3p("ld1",  code + i); break;
            case LD2:  i += disasm3p("ld2",  code + i); break;
            case LD4:  i += disasm3p("ld4",  code + i); break;
            case LD8:  i += disasm3p("ld8",  code + i); break;
            case ST1:  i += disasm3p("st1",  code + i); break;
            case ST2:  i += disasm3p("st2",  code + i); break;
            case ST4:  i += disasm3p("st4",  code + i); break;
            case ST8:  i += disasm3p("st8",  code + i); break;
            case MOV:  i += disasm2p("mov",  code + i); break;
            case FTOI: i += disasm2p("ftoi", code + i); break;
            case ITOF: i += disasm2p("itof", code + i); break;
            case PUSH: i += disasm1p("push", code + i); break;
            case POP:  i += disasm1p("pop",  code + i); break;
            case CALL: i += disasm1p("call", code + i); break;
            case RET:  i += disasm0p("ret",  code + i); break;
            case CMP:  i += disasm2p("cmp",  code + i); break;
            case JMP:  i += disasm1p("jmp",  code + i); break;
            case JE:   i += disasm1p("je",   code + i); break;
            case JNE:  i += disasm1p("jne",  code + i); break;
            case JL:   i += disasm1p("jl",   code + i); break;
            case JLE:  i += disasm1p("jle",  code + i); break;
            case JG:   i += disasm1p("jg",   code + i); break;
            case JGE:  i += disasm1p("jge",  code + i); break;
            case AST:  i += disasm1p("ast",  code + i); break;
        }
        printf("\n");
    }
}

typedef struct VMOperand VMOperand;
struct VMOperand {
    bool is_imm;
    u64 val;
};

typedef struct VMInstructionOperands VMInstructionOperands;
struct VMInstructionOperands {
    u8 operand;
    VMOperand x;
    VMOperand y;
    VMOperand z;
};

VMInstructionOperands vm_decode_operands(VM *vm) {
    VMInstructionOperands op;
    u8 operand = *vm->ip++;
    op.operand = operand;
    u8 nx_bytes = reg_size[(operand & 0xC0) >> 6];
    u8 ny_bytes;
    u8 nz_bytes;

    bool y_is_imm = (operand & 0x20) != 0;
    bool z_is_imm = (operand & 0x04) != 0;

    if (y_is_imm) ny_bytes = imm_size[(operand & 0x18) >> 3];
    else          ny_bytes = reg_size[(operand & 0x18) >> 3];
    if (z_is_imm) nz_bytes = imm_size[(operand & 0x3)];
    else          nz_bytes = reg_size[(operand & 0x3)];
    op.x = (VMOperand){ false, read_bytes(nx_bytes, vm->ip) };
    vm->ip += nx_bytes;
    op.y = (VMOperand){ y_is_imm, read_bytes(ny_bytes, vm->ip) };
    vm->ip += ny_bytes;
    op.z = (VMOperand){ z_is_imm, read_bytes(nz_bytes, vm->ip) };
    vm->ip += nz_bytes;
    return op;
}

Val vm_val(VM *vm, VMOperand op) {
    if (op.is_imm) return (Val){ op.val };
    ASSERT(arrlen(vm->registers) > op.val); // FIXME: Need to allocated more as needed
    return vm->registers[op.val];
}

void vm_init(VM *vm, u8 *code, u32 highest_register) {
    vm->code = code;
    vm->ip = code;
    arrsetlen(vm->registers, highest_register + 1 + sizeof(reg_names) / sizeof(*reg_names));
    memset(vm->registers, 0, (highest_register + 1 + sizeof(reg_names) / sizeof(*reg_names)) * 8);
}

void vm_interp(VM *vm) {
    u8 *end = vm->code + arrlen(vm->code);
    while (vm->ip < end) {
        u8 instruction = *vm->ip++;
        switch (instruction) { // No operands
            case HLT: goto end;
            case NOP: continue;
            case RET: UNIMPLEMENTED();
        }

        VMInstructionOperands op = vm_decode_operands(vm);
        switch (instruction) {
            case ADD:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u + vm_val(vm, op.z).u;
                break;
            case ADDF:
                vm->registers[op.x.val].f = vm_val(vm, op.y).f + vm_val(vm, op.z).f;
                break;
            case SUB:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u - vm_val(vm, op.z).u;
                break;
            case SUBF:
                vm->registers[op.x.val].f = vm_val(vm, op.y).f - vm_val(vm, op.z).f;
                break;
            case MUL:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u * vm_val(vm, op.z).u;
                break;
            case MULF:
                vm->registers[op.x.val].f = vm_val(vm, op.y).f * vm_val(vm, op.z).f;
                break;
            case DIV:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u / vm_val(vm, op.z).u;
                break;
            case DIVF:
                vm->registers[op.x.val].f = vm_val(vm, op.y).f / vm_val(vm, op.z).f;
                break;
            case MOD:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u % vm_val(vm, op.z).u;
                break;
            case XOR:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u ^ vm_val(vm, op.z).u;
                break;
            case AND:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u & vm_val(vm, op.z).u;
                break;
            case OR:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u | vm_val(vm, op.z).u;
                break;
            case SHL:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u << vm_val(vm, op.z).u;
                break;
            case SHR:
                vm->registers[op.x.val].u = vm_val(vm, op.y).u >> vm_val(vm, op.z).u;
                break;
            case LD1:
                vm->registers[op.x.val].u = read_bytes(1, vm_val(vm, op.y).p + vm_val(vm, op.z).i);
                break;
            case LD2:
                vm->registers[op.x.val].u = read_bytes(2, vm_val(vm, op.y).p + vm_val(vm, op.z).i);
                break;
            case LD4:
                vm->registers[op.x.val].u = read_bytes(4, vm_val(vm, op.y).p + vm_val(vm, op.z).i);
                break;
            case LD8:
                vm->registers[op.x.val].u = read_bytes(8, vm_val(vm, op.y).p + vm_val(vm, op.z).i);
                break;
            case ST1:
                *((u8 *) (vm_val(vm, op.x).p + vm_val(vm, op.y).i)) = (u8) vm_val(vm, op.z).u;
                break;
            case ST2:
                *((u16 *) (vm_val(vm, op.x).p + vm_val(vm, op.y).i)) = (u16) vm_val(vm, op.z).u;
                break;
            case ST4:
                *((u32 *) (vm_val(vm, op.x).p + vm_val(vm, op.y).i)) = (u32) vm_val(vm, op.z).u;
                break;
            case ST8:
                *((u64 *) (vm_val(vm, op.x).p + vm_val(vm, op.y).i)) = (u64) vm_val(vm, op.z).u;
                break;
            case MOV:
                vm->registers[op.y.val] = vm_val(vm, op.z);
                break;
            case FTOI:
                vm->registers[op.y.val].i = (i64) vm_val(vm, op.z).f;
                break;
            case ITOF:
                vm->registers[op.y.val].f = (f64) vm_val(vm, op.z).i;
                break;
            case PUSH:
                arrpush(vm->stack, vm_val(vm, op.z));
                break;
            case POP:
                vm->registers[op.z.val] = arrpop(vm->stack);
                break;
            case CALL:
                arrpush(vm->stack, vm->registers[RFP]);
                vm->ip += vm_val(vm, op.z).i;
                break;
            case CMP:
                vm->flgs = vm_val(vm, op.y).i - vm_val(vm, op.z).i;
                break;
            case JMP:
                vm->ip += vm_val(vm, op.z).i;
                break;
            case JE:
                vm->ip += (!(vm->flgs)) * vm_val(vm, op.z).i;
                break;
            case JNE:
                vm->ip += (!!(vm->flgs)) * vm_val(vm, op.z).i;
                break;
            case JL:
                if (vm->flgs < 0)  vm->ip += vm_val(vm, op.z).i;
                break;
            case JLE:
                if (vm->flgs <= 0) vm->ip += vm_val(vm, op.z).i;
                break;
            case JG:
                if (vm->flgs > 0)  vm->ip += vm_val(vm, op.z).i;
                break;
            case JGE:
                if (vm->flgs <= 0) vm->ip += vm_val(vm, op.z).i;
                break;
            case AST:  break;
        }
    }
end:
    vm->registers[0].u = 0;
    vm->registers[RIP].u = (u64) (vm->ip - vm->code);
    return;
}

void vm_dump(VM *vm) {
    for (i32 i = 0; i < arrlen(vm->registers); i++) {
        disasmreg(i);
        printf("%llu\n", vm->registers[i].u);
    }
}
