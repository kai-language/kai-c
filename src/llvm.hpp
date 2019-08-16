#pragma once

// requires none

// package.h
typedef struct Package Package;

#ifdef __cplusplus
extern "C" {
#endif

bool llvm_build_module(Package *package);
bool llvm_emit_object(Package *package);

#ifdef __cplusplus
} // extern "C"
#endif

