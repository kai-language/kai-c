#pragma once

// requires none

// package.h
typedef struct Package Package;

typedef struct IRFile IRFile;
typedef struct IRScope IRScope;
typedef struct IRLayout IRLayout;
typedef struct IRTarget IRTarget;
typedef struct IRModule IRModule;

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

typedef struct IRContext IRContext;

typedef struct IRFunctionArgs IRFunctionArgs;

typedef struct DBGType DBGType;
typedef struct DBGFunction DBGFunction;

#ifdef __cplusplus
extern "C" {
#endif

bool llvm_build_module(Package *package);
bool llvm_emit_object(Package *package);

#ifdef __cplusplus
} // extern "C"
#endif

