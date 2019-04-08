
#ifndef compiler_h
#define compiler_h

#include "flags.h"

#define MAX_GLOBAL_SEARCH_PATHS 256

typedef struct Compiler Compiler;
typedef struct CheckerWork CheckerWork;

extern Compiler compiler;

Package *import_package(const char *path, Package *importer);
void read_package_source_files(Package *pkg);

struct CheckerWork {
    Package *package;
    Stmt *stmt;
};

struct Compiler {
    int arg_count;
    const char **args;

    CompilerFlags flags;
    char input_name[MAX_PATH];
    char output_name[MAX_PATH];
    Os target_os;
    Arch target_arch;
    Output target_output;
    TargetMetrics target_metrics;

    const char *global_search_paths[MAX_GLOBAL_SEARCH_PATHS];
    int num_global_search_paths;

    Package builtin_package;
    Map package_map;
    DynamicArray(Package *) packages;

    Queue parsing_queue;
    Queue checking_queue;
};
#endif
