
#include "all.h"
#include "bytecode2.h"
#include "arena.h"
#include "package.h"

typedef struct BytecodeProgram BytecodeProgram;
struct BytecodeProgram {
    Package *package;
    u8 *code;
    u8 *data;
    u8 *rodata;
};

typedef struct FunctionEmitter FunctionEmitter;
struct FunctionEmitter {
    Package *package;
    u8 *code;
    u8 *data;
};

#define RIP 0x0
#define RFP 0x1
#define RSP 0x2

/*
 ┌──────────────────────────────────────────────────┐
 │0                     7 8        10       13      │
 ├───────────────────────┬────────┬────────┬────────┤
 │          op8          │   A    │   B    │   C    │
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
    ADD  = 0x02, // A = B + C
    ADDF = 0x03,
    SUB  = 0x04, // A = B - C
    SUBF = 0x05,
    MUL  = 0x06, // A = B * C
    MULF = 0x07,
    DIV  = 0x08, // A = B / C
    DIVF = 0x09,
    MOD  = 0x0A, // A = B % C
    XOR  = 0x0B, // A = B ^ C
    AND  = 0x0C, // A = B & C
    OR   = 0x0D, // A = B | C
    SHL  = 0x0E, // A = B << C
    SHR  = 0x0F, // A = B >> C

    LD1  = 0x10, // A = *(B + C)
    ST1  = 0x11, // *(A + B) = C
    LD2  = 0x12,
    ST2  = 0x13,
    LD4  = 0x14,
    ST4  = 0x15,
    LD8  = 0x16,
    ST8  = 0x17,

    MOV  = 0x20, // A = B
    FTOI = 0x21, // A = ftoi(B)
    ITOF = 0x22, // A = itof(B)
    PUSH = 0x23, // *(rsp) = A; rsp += 1
    POP  = 0x24, // A = *(rsp); rsp -= 1
    CALL = 0x25, // *(rsp) = rip; rsp += 1; rip += A
    RET  = 0x26, // rsp += 1; rip = *(rsp)
    CMP  = 0x27, // flgs = A - B

    JMP  = 0x30, // rip += A
    JE   = 0x31, // if(flgs == 0) rip += A
    JNE  = 0x32, // if(flgs != 0) rip += A
    JL   = 0x33, // if(flgs <  0) rip += A
    JLE  = 0x34, // if(flgs <= 0) rip += A
    JG   = 0x35, // if(flgs >  0) rip += A
    JGE  = 0x36, // if(flgs >= 0) rip += A

    AST  = 0xA5,
};

u32 *e_hlt(BCFunction *b) {

}
