#pragma once

// Requires package.h queue.h

// string.h
typedef struct InternedString InternedString;

// checker.h
typedef struct Sym Sym;

// package.h
typedef struct PackageMapEntry PackageMapEntry;

typedef enum Os {
    Os_Unknown,
    Os_Linux,
    Os_Darwin,
    Os_Windows,
    NUM_OSES,
} Os;

typedef enum Arch {
    Arch_Unknown,
    Arch_x86_64,
    Arch_x86,
    Arch_arm,
    Arch_arm64,
    NUM_ARCHES,
} Arch;

typedef struct TargetMetrics TargetMetrics;
struct TargetMetrics {
    u32 Width;
    u32 Align;
};

//typedef enum Output Output;
typedef enum Output {
    OutputType_Exec = 0,
    OutputType_Static,
    OutputType_Dynamic
} Output;

typedef struct CompilerFlags CompilerFlags;
struct CompilerFlags {
    b32 parse_comments;
    b32 error_codes;
    b32 error_colors;
    b32 error_source;
    b32 builtins;
    b32 verbose;
    b32 version;
    b32 help;
    b32 emit_ir;
    b32 emit_header;
    b32 dump_ir;
    b32 debug;
    b32 link;
};

#define MAX_GLOBAL_SEARCH_PATHS 16

typedef struct Compiler Compiler;
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

    InternedString *interns;
    Arena strings;
    Arena arena;

    Scope *global_scope;
    Package builtin_package;
    PackageMapEntry *packages;

    Sym **ordered_symbols;

    Queue parsing_queue;
    Queue checking_queue;
};

typedef struct CheckerWork CheckerWork;
struct CheckerWork {
    Package *package;
    Stmt *stmt;
};

extern Compiler compiler;

void compiler_init(Compiler *compiler, int argc, const char **argv);
bool compile(Compiler *compiler);
