
#include "all.h"
#include "llvm.h"
#include "llvm_wrap.hpp"

void backend_init(Package *pkg) {

    LLContext *ctx = llvm_init(pkg);
}
