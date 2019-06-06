#pragma once

// ast.h
typedef struct Stmt Stmt;

// package.h
typedef struct Package Package;

// checker.h
typedef struct Sym Sym;

// bytecode.c
typedef struct Fixup Fixup;

typedef struct BytecodeProgram BytecodeProgram;
struct BytecodeProgram {

};

typedef enum BCSymKind BCSymKind;
enum BCSymKind {
    CODE,
    STACK,
    GLOBAL,
};

typedef struct BCSym BCSym;
struct BCSym {
    BCSymKind kind;
    Sym *sym;
    union {
        u32 instr_offset;
        i32 frame_offset;
        struct {
            u64 offset_in_data;
            u32 **fixups; // arr
        };
    };
};

typedef struct BCStackFrame BCStackFrame;
struct BCStackFrame {
    BCSym **args; // arr
    BCSym **syms; // arr
};

typedef struct BytecodeGenerator BytecodeGenerator;
struct BytecodeGenerator {
    Package *package;
    BCStackFrame *frames; // arr
    BCSym *fixups;
    u32 *instructions;
    u8 *data;
    u8 *stack;
};
