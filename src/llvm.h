#pragma once

// requires nothing

// package.h
typedef struct Package Package;

typedef struct Emitter Emitter;

bool llvm_build_module(Package *package);
