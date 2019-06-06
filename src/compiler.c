
#include "all.h"
#include "os.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "ast.h"
#include "compiler.h"
#include "string.h"
#include "lexer.h"
#include "parser.h"
#include "checker.h"
#include "types.h"
#include "bytecode.h"

void add_global_search_path(Compiler *compiler, const char *path) {
    verbose("Adding global search path %s", path);
    compiler->global_search_paths[compiler->num_global_search_paths++] = str_intern(path);
}

void init_global_search_paths(Compiler *compiler) {
    TRACE(INIT);
    add_global_search_path(compiler, "vendor");
    add_global_search_path(compiler, "packages");
    const char *kaipath_env = getenv("KAIPATH");
    if (kaipath_env) {
        char path[MAX_PATH];
        path_copy(path, kaipath_env);
        path_join(path, "packages");
        add_global_search_path(compiler, path);
    } else {
        verbose("No KAIPATH environment variable set");
    }
}

static 
char *remove_kai_extension(char *filename) {
    char *dot = strrchr(filename, '.');
    if (!dot) return filename;
    if (strcmp(dot, ".kai") == 0) {
        *dot = '\0';
    }
    return filename;
}

const char *OsNames[NUM_OSES] = {
    [Os_Unknown] = "unknown",
    [Os_Linux] = "Linux",
    [Os_Darwin] = "Darwin",
    [Os_Windows] = "Windows"
};

const char *ArchNames[NUM_ARCHES] = {
    [Arch_Unknown] = "unknown",
    [Arch_x86_64]  = "x86_64",
    [Arch_x86]     = "x86",
    [Arch_arm]     = "arm",
    [Arch_arm64]   = "arm64",
};

// Type details
TargetMetrics Os_Linux_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = { .Width = 64, .Align = 64 },
};

TargetMetrics Os_Darwin_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = { .Width = 64, .Align = 64 },
};

TargetMetrics Os_Windows_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = { .Width = 64, .Align = 64 },
    [Arch_x86]    = { .Width = 32, .Align = 32 },
};

Os OsForName(const char *name) {
    for (int i = 0; i < NUM_OSES; i++) {
        if (OsNames[i] != NULL && strcmp(OsNames[i], name) == 0)
            return (Os) i;
    }
    return Os_Unknown;
}

Arch ArchForName(const char *name) {
    for (int i = 0; i < NUM_ARCHES; i++) {
        if (ArchNames[i] != NULL && strcmp(ArchNames[i], name) == 0)
            return (Arch) i;
    }
    return Arch_Unknown;
}

typedef enum CLIFlagKind CLIFlagKind;
enum CLIFlagKind {
    CLIFlagKindBool,
    CLIFlagKindEnum,
    CLIFlagKindPath,
    CLIFlagKindString,
};

typedef struct CLIFlag CLIFlag;
struct CLIFlag {
    CLIFlagKind kind;
    const char *name;
    const char *alias;
    const char **options;
    const char *argumentName;
    const char *help;
    int nOptions;
    long offs;
};

CompilerFlags default_flags = {
    .parse_comments = false,
    .error_codes    = true,
    .error_colors   = true,
    .error_source   = true,
    .builtins       = true,
    .verbose        = false,
    .version        = false,
    .help           = false,
    .emit_ir        = false,
    .emit_header    = false,
    .dump_ir        = false,
    .debug          = true,
    .link           = true,
};

static
Compiler *parsed_compiler;

static const char *OutputTypeNames[] = {
    "exec",
    "static",
    "dynamic"
};

#define FLAG_BOOL(NAME, SHORT_NAME, PTR, HELP) \
{ CLIFlagKindBool, (NAME), (SHORT_NAME), .offs = offsetof(Compiler, PTR), .help = (HELP) }

#define FLAG_ENUM(NAME, PTR, OPTIONS, HELP) \
{ CLIFlagKindEnum, (NAME), .offs = offsetof(Compiler, PTR), .options = (OPTIONS), .nOptions = sizeof(OPTIONS) / sizeof(*OPTIONS), .help = (HELP) }

#define FLAG_PATH(NAME, SHORT_NAME, PTR, ARG_NAME, HELP) \
{ CLIFlagKindPath, (NAME), (SHORT_NAME), .offs = offsetof(Compiler, PTR), .argumentName = (ARG_NAME), .help = (HELP) }

#define FLAG_STRING(NAME, SHORT_NAME, PTR, ARG_NAME, HELP) \
{ CLIFlagKindString, (NAME), (SHORT_NAME), .offs = offsetof(Compiler, PTR), .argumentName = (ARG_NAME), .help = (HELP) }

CLIFlag CLIFlags[] = {
    FLAG_BOOL("help",    "h",  flags.help,    "Print help information"),
    FLAG_BOOL("version", NULL, flags.version, "Prints compiler version"),

    FLAG_BOOL("verbose", "v",  flags.verbose, "Enable verbose output"),
    FLAG_BOOL("dump-ir", NULL, flags.dump_ir,  "Dump LLVM IR"),
    FLAG_BOOL("emit-ir", NULL, flags.emit_ir,  "Emit LLVM IR file(s)"),

    FLAG_BOOL("emit-header", NULL, flags.emit_header, "Emit C header file(s)"),

    FLAG_BOOL("error-codes",  NULL, flags.error_codes,  "Show error codes along side error location"),
    FLAG_BOOL("error-colors", NULL, flags.error_colors, "Show errors in souce code by highlighting in color"),
    FLAG_BOOL("error-source", NULL, flags.error_source, "Show source code when printing errors"),

    FLAG_BOOL("builtins", NULL, flags.builtins, "Include automatically the builtins package"),

    FLAG_BOOL("parse-comments", NULL, flags.parse_comments, ""),
    FLAG_BOOL("debug", "g", flags.debug, "Include debug symbols"),
    FLAG_BOOL("link", NULL, flags.link,  "Link object files"),

    FLAG_PATH("output", "o", output_name, "file", "Output file (default: <input>)"),

    FLAG_ENUM("os", target_os, OsNames, "Target operating system (default: current)"),
    FLAG_ENUM("arch", target_arch, ArchNames, "Target architecture (default: current)"),
    FLAG_ENUM("type", target_output, OutputTypeNames, "Final output type (default: exec)"),
};

CLIFlag *flag_for_name(const char *name) {
    size_t nFlags = sizeof(CLIFlags) / sizeof(*CLIFlags);
    for (size_t i = 0; i < nFlags; i++) {
        if (strcmp(CLIFlags[i].name, name) == 0)       return &CLIFlags[i];
        else if (CLIFlags[i].alias && strcmp(CLIFlags[i].alias, name) == 0) return &CLIFlags[i];
    }
    return NULL;
}

void parse_flags(Compiler *compiler, int *pargc, const char ***pargv) {
    TRACE(INIT);
    compiler->flags = default_flags;
    parsed_compiler = compiler;
    int argc = *pargc;
    const char **argv = *pargv;
    int i;
    for (i = 1; i < argc; i++) {
        const char *arg = argv[i];
        const char *name = arg;
        if (*name == '-') {
            name++;
            if (*name == '-') name++;

            bool inverse = false;
            if (strncmp("no-", name, 3) == 0) {
                inverse = true;
                name += 3;
            }
            CLIFlag *flag = flag_for_name(name);
            if (!flag || (inverse && flag->kind != CLIFlagKindBool)) {
                printf("Unknown flag %s\n", arg);
                continue;
            }
            switch (flag->kind) {
                case CLIFlagKindBool:;
                    b32 *ptr = ((void *) compiler) + flag->offs;
                    *ptr = inverse ? false : true;
                    break;

                case CLIFlagKindEnum: {
                    const char *option;
                    if (i + 1 < argc) {
                        i++;
                        option = argv[i];
                    } else {
                        printf("No value argument after -%s\n", arg);
                        break;
                    }
                    bool found = false;
                    for (int k = 0; k < flag->nOptions; k++) {
                        if (strcmp(flag->options[k], option) == 0) {
                            int *ptr = ((void *) compiler) + flag->offs;
                            *ptr = k;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        printf("Invalid value %s for %s. Expected (", option, arg);
                        for (int k = 0; k < flag->nOptions; k++) {
                            printf("%s", flag->options[k]);
                            printf("|");
                        }
                        printf(")\n");
                        break;
                    }
                    break;
                }

                case CLIFlagKindPath:
                    if (i + 1 < argc) {
                        i++;
                        path_copy((void *) compiler + flag->offs, argv[i]);
                    } else {
                        printf("No value argument after -%s\n", arg);
                    }
                    break;

                case CLIFlagKindString:
                    if (i + 1 < argc) {
                        i++;
                        const char **ptr = ((void *) compiler) + flag->offs;
                        *ptr = argv[i];
                    } else {
                        printf("No value argument after -%s\n", arg);
                    }
                    break;

                default:
                    ASSERT(false);
            }
        } else {
            break;
        }
    }
    *pargc = argc - i;
    *pargv = argv + i;
    if (argc - i == 1) {
        path_copy(compiler->input_name, argv[i]);
    }
}

void print_usage(const char *prog_name) {
    printf("Usage: %s [flags] <input>\n", prog_name);
    size_t nFlags = sizeof(CLIFlags) / sizeof(*CLIFlags);
    for (size_t i = 0; i < nFlags; i++) {
        char invokation[40];
        char help[100];

        CLIFlag flag = CLIFlags[i];

        int iLen = 0;
        int hLen = snprintf(help, sizeof(help), "%s", flag.help ? flag.help : "");

        if (flag.alias) iLen = snprintf(invokation, sizeof(invokation), "-%s ", flag.alias);
        iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, "-%s", flag.name);

        switch (flag.kind) {
            case CLIFlagKindEnum:
                ASSERT(flag.nOptions > 0);
                iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, " <");
                iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, "%s", flag.options[0]);
                for (int i = 1; i < flag.nOptions; i++) {
                    iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, "|%s", flag.options[i]);
                }
                iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, ">");
                break;

            case CLIFlagKindBool:
                hLen += snprintf(help + hLen, sizeof(help) - hLen, " (default)");
                break;

            case CLIFlagKindPath:
                iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, " <%s>", flag.argumentName);
                break;

            case CLIFlagKindString:
                iLen += snprintf(invokation + iLen, sizeof(invokation) - iLen, " <%s>", flag.argumentName);
                break;
        }
        printf(" %-40s %s\n", invokation, help);
    }
}

void configure_defaults(Compiler *compiler) {
    TRACE(INIT);
    if (!strlen(compiler->output_name)) {
        char path[MAX_PATH];
        path_copy(path, compiler->input_name);
        char *file = path_file(path);
        file = remove_kai_extension(file);
        path_copy(compiler->output_name, file);
    }
    SysInfo info = get_current_sysinfo();
    if (compiler->target_os == Os_Unknown) {
        compiler->target_os = OsForName(info.name);
    }
    if (compiler->target_arch == Arch_Unknown) {
        compiler->target_arch = ArchForName(info.arch);
    }
    if (compiler->target_os == Os_Unknown || compiler->target_arch == Arch_Unknown) {
        printf("Unsupported Os or Arch: %s %s\n",
               OsNames[compiler->target_os], ArchNames[compiler->target_arch]);
        exit(1);
    }
    switch (compiler->target_os) {
        case Os_Linux:
            compiler->target_metrics = Os_Linux_ArchSupport[compiler->target_arch];
            break;
        case Os_Darwin:
            compiler->target_metrics = Os_Darwin_ArchSupport[compiler->target_arch];
            break;
        case Os_Windows:
            compiler->target_metrics = Os_Windows_ArchSupport[compiler->target_arch];
            break;
        default: break;
    }
    
#define DECLARE_BUILTIN_TYPE(TYPE, NAME) \
{ \
    Sym *sym = arena_calloc(&compiler->arena, sizeof *sym); \
    sym->name = str_intern(NAME); \
    sym->state = SYM_CHECKED; \
    sym->kind = SYM_TYPE; \
    sym->type = TYPE; \
    TYPE->symbol = sym; \
    scope_declare(compiler->global_scope, sym); \
}
    compiler->global_scope = arena_calloc(&compiler->arena, sizeof *compiler->global_scope);
    DECLARE_BUILTIN_TYPE(type_any, "any");
    DECLARE_BUILTIN_TYPE(type_void, "void");
    DECLARE_BUILTIN_TYPE(type_bool, "bool");
    DECLARE_BUILTIN_TYPE(type_i8, "i8");
    DECLARE_BUILTIN_TYPE(type_i16, "i16");
    DECLARE_BUILTIN_TYPE(type_i32, "i32");
    DECLARE_BUILTIN_TYPE(type_i64, "i64");
    DECLARE_BUILTIN_TYPE(type_u8, "u8");
    DECLARE_BUILTIN_TYPE(type_u16, "u16");
    DECLARE_BUILTIN_TYPE(type_u32, "u32");
    DECLARE_BUILTIN_TYPE(type_u64, "u64");
    DECLARE_BUILTIN_TYPE(type_f32, "f32");
    DECLARE_BUILTIN_TYPE(type_f64, "f64");
    DECLARE_BUILTIN_TYPE(type_int, "int");
    DECLARE_BUILTIN_TYPE(type_uint, "uint");
    DECLARE_BUILTIN_TYPE(type_intptr, "intptr");
    DECLARE_BUILTIN_TYPE(type_uintptr, "uintptr");
    DECLARE_BUILTIN_TYPE(type_rawptr, "rawptr");
    type_string = type_slice(type_u8, NONE);
    DECLARE_BUILTIN_TYPE(type_string, "string");
}

void output_version_and_build_info(void) {
    printf("%s\n\n", VERSION);
    const char *y = "✔";
    const char *n = "✘";
    bool debug = false;
#if DEBUG
    debug = true;
#endif
    printf("-DDEBUG %s\n", debug ? y : n);
}

void compiler_init(Compiler *compiler, int argc, const char **argv) {
    TRACE(INIT);
    const char *prog_name = argv[0];
    parse_flags(compiler, &argc, &argv);
    if (compiler->flags.version) {
        output_version_and_build_info();
        exit(0);
    }
    if (argc != 1 || compiler->flags.help) {
        print_usage(prog_name);
        exit(1);
    }
    configure_defaults(compiler);
    init_global_search_paths(compiler);
    parser_init_interns();
}

bool compile(Compiler *compiler) {
    TRACE(GENERAL);
    if (compiler->flags.builtins) {
        Package *builtins = import_package("builtin", NULL);
        if (!builtins) warn("Failed to compile builtin package"); 
    }
    Package *main = import_package(compiler->input_name, NULL);
    if (!main) fatal("Failed to compile '%s'", compiler->input_name);
    for (;;) {
        COUNTER1(IMPORT, "parsing_queue", INT("length", (int) compiler->parsing_queue.size));
        Package *pkg = queue_pop_front(&compiler->parsing_queue);
        if (pkg) {
            parse_package(pkg);
            continue;
        }
        COUNTER1(IMPORT, "checking_queue", INT("length", (int) compiler->checking_queue.size));
        CheckerWork *work = queue_pop_front(&compiler->checking_queue);
        if (work && !work->package->errors) {
            verbose("Checking stmt within package %s", work->package->path);
            bool requeue = check(work->package, work->stmt);
            if (requeue) {
                queue_push_back(&compiler->checking_queue, work);
                verbose("Requeuing stmt within package %s", work->package->path);
            }
            continue;
        }
        break;
    }
    bool was_errors = false;
    for (i64 i = 0; i < hmlen(compiler->packages); i++) {
        if (compiler->packages[i].value->errors) {
            was_errors = true;
            output_errors(compiler->packages[i].value);
        }
    }
    BytecodeGenerator bc = {0};
    for (i64 i = 0; i < hmlen(compiler->packages); i++) {
        bc.package = compiler->packages[i].value;
        for (i64 j = 0; j < arrlen(bc.package->stmts); j++) {
            gen_bytecode_stmt(&bc, bc.package->stmts[j]);
        }
    }
    return !was_errors;
}

#if TEST
void init_test_compiler(Compiler *compiler, const char *flags) {
    *compiler = (Compiler){0};
    char *str = "";
    if (flags) {
        str = xmalloc(strlen(flags) + 1);
        strcpy(str, flags);
    }

    const char **args = NULL;
    arrput(args, "kai");

    char *arg = strtok(str, " ");
    while(arg) {
        arrput(args, arg);
        arg = strtok(NULL, " ");
    }

    arrput(args, "test.kai");

    int argc = (int) arrlen(args);
    compiler_init(compiler, argc, args);
}

void test_flagParsingAndDefaults() {
    init_test_compiler(&compiler, "-o outputName -v -os Darwin");
    ASSERT(compiler.flags.verbose);
    ASSERT(strcmp(compiler.output_name, "outputName") == 0);
    ASSERT(compiler.target_os == Os_Darwin);

    init_test_compiler(&compiler, NULL);
    ASSERT(compiler.target_arch != Arch_Unknown);
    ASSERT(compiler.target_os != Arch_Unknown);
}
#endif

