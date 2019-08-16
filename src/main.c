
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
#include "string.h"

#define DEBUG_IMPLEMENTATION
#include "debug.h"

#define PROFILER_IMPLEMENTATION
#include "profiler.h"

Compiler compiler;
u64 source_memory_usage = 0;

#define hmsize(hm) hmlenu(hm) * sizeof *hm + sizeof *hm + sizeof(stbds_array_header)
#define arrsize(arr) arrlenu(arr) * sizeof *arr + sizeof(stbds_array_header)

#if !TEST
int main(int argc, const char **argv) {
    profiler_init();
    compiler_init(&compiler, argc, argv);
    bool success = compile(&compiler);
    if (!success) {
        const char *stage = compiler_stage_name(compiler.failure_stage);
        printf("error: Compilation failed during %s\n", stage);
    }
    profiler_output();

    u64 total_memory_usage = source_memory_usage;
    total_memory_usage += hmsize(compiler.interns);
    total_memory_usage += arrsize(compiler.ordered_symbols);
    total_memory_usage += compiler.arena.used_size;
    total_memory_usage += compiler.strings.used_size;
    for (i64 i = 0; i < hmlen(compiler.packages); i++) {
        total_memory_usage += compiler.packages[i].value->arena.used_size;
    }
    verbose("Processed %.2fKB of source files", (f64) source_memory_usage / 1024.f);
    verbose("Memory usage: %.2fKB\n", (f64) total_memory_usage / 1024.f);
    return 0;
}
#endif
