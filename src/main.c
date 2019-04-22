
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-result"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#pragma clang diagnostic pop

#include "all.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "compiler.h"

#define DEBUG_IMPLEMENTATION
#include "debug.h"

#define PROFILER_IMPLEMENTATION
#include "profiler.h"

Compiler compiler;
u64 source_memory_usage = 0;

#if !TEST
int main(int argc, const char **argv) {
    profiler_init();
    compiler_init(&compiler, argc, argv);
    compile(&compiler);
    profiler_output();
    return 0;
}
#endif
