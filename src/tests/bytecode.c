
#include "all.h"
#include "checker.h"
#include "bytecode.h"

#if TEST
#define SETUP() \
VM vm; \
BCBlock block = {0}; \
BCBuilder builder = {&block}; \

void test_bytecode_integer_arithmetic() {
    SETUP();

    e_add(&builder, 4, imm(4), RZ0);    // r4 = 4  + rzo
    e_add(&builder, 4, reg(4), imm(2)); // r4 = r4 + 2
    e_sub(&builder, 5, reg(4), imm(1)); // r5 = r4 - 1
    e_mul(&builder, 6, reg(5), imm(2)); // r6 = r5 * 2
    e_div(&builder, 7, reg(6), imm(2)); // r7 = r6 / 2
    e_mod(&builder, 8, reg(7), imm(2)); // r8 = r7 % 2
    e_hlt(&builder);

    vm_init(&vm, block.code, 9);
    vm_interp(&vm);

    ASSERT(vm.registers[4].u == 6);
    ASSERT(vm.registers[5].u == 5);
    ASSERT(vm.registers[6].u == 10);
    ASSERT(vm.registers[7].u == 5);
    ASSERT(vm.registers[8].u == 1);
    arrsetlen(block.code, 0);
}


void test_bytecode_floating_point_arithmetic() {
    SETUP();

    e_addf(&builder, 4, RZ0,    imf(4.f)); // r4 = rzo + 4
    e_addf(&builder, 4, reg(4), imf(2.f)); // r4 = r4 + 2
    e_subf(&builder, 5, reg(4), imf(1.f)); // r5 = r4 - 1
    e_mulf(&builder, 6, reg(5), imf(2.f)); // r6 = r5 * 2
    e_divf(&builder, 7, reg(6), imf(2.f)); // r7 = r6 / 2
    e_hlt(&builder);

    vm_init(&vm, block.code, 9);
    vm_interp(&vm);

    ASSERT(vm.registers[4].f == 6.f);
    ASSERT(vm.registers[5].f == 5.f);
    ASSERT(vm.registers[6].f == 10.f);
    ASSERT(vm.registers[7].f == 5.f);
    arrsetlen(block.code, 0);
}

void test_bytecode_load_and_stores_move() {
    SETUP();

    u64 mem;
    u64 mem2;
    u64 mem4;
    u64 mem8;

    e_st1(&builder, 0, imm((u64) &mem),  imm(8));
    e_st2(&builder, 0, imm((u64) &mem2), imm(8));
    e_st4(&builder, 0, imm((u64) &mem4), imm(8));
    e_st8(&builder, 0, imm((u64) &mem8), imm(UINT64_MAX));
    e_ld1(&builder, 4, imm((u64) &mem), RZ0);
    e_ld2(&builder, 5, imm((u64) &mem), RZ0);
    e_ld4(&builder, 6, imm((u64) &mem), RZ0);
    e_ld8(&builder, 7, imm((u64) &mem), RZ0);
    e_mov(&builder, 8, reg(5));
    e_mov(&builder, 9, imm(42));
    e_hlt(&builder);

    vm_init(&vm, block.code, 9);
    vm_interp(&vm);

    ASSERT(mem  == 8);
    ASSERT(mem2 == 8);
    ASSERT(mem4 == 8);
    ASSERT(mem8 == UINT64_MAX);
    ASSERT(vm.registers[4].u == 8);
    ASSERT(vm.registers[5].u == 8);
    ASSERT(vm.registers[6].u == 8);
    ASSERT(vm.registers[7].u == 8);
    ASSERT(vm.registers[8].u == 8);
    ASSERT(vm.registers[9].u == 42);
}

void test_bytecode_conversions() {
    SETUP();

    e_mov(&builder, 4, imm(50));
    e_itof(&builder, 4, reg(4));

    e_mov(&builder, 5, imf(42.f));
    e_ftoi(&builder, 5, reg(5));

    e_hlt(&builder);

    vm_init(&vm, block.code, 5);
    vm_interp(&vm);

    ASSERT(vm.registers[4].f == 50.f);
    ASSERT(vm.registers[5].u == 42);
}

void test_bytecode_cmp() {

}
#undef SETUP
#endif
