#pragma once

// requires none

// package.h
typedef struct Package Package;

typedef struct LLFile LLFile;
typedef struct LLScope LLScope;
typedef struct LLLayout LLLayout;
typedef struct LLTarget LLTarget;
typedef struct LLModule LLModule;

typedef struct LLType LLType;
typedef struct LLStruct LLStruct;
typedef struct LLVoid LLVoid;
typedef struct LLInteger LLInteger;
typedef struct LLFloat LLFloat;
typedef struct LLArray LLArray;
typedef struct LLPointer LLPointer;
typedef struct LLFunction LLFunction;

typedef struct LLBasicBlock LLBasicBlock;
typedef struct LLAlloca LLAlloca;
typedef struct LLGlobal LLGlobal;

typedef struct LLValue LLValue;

typedef struct LLContext LLContext;

LLContext *llvm_init(Package *pkg);
