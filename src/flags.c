
#include "flags.h"
#include "compiler.h"

typedef enum CLIFlagKind {
    CLIFlagKindBool,
    CLIFlagKindEnum,
    CLIFlagKindPath,
    CLIFlagKindString,
} CLIFlagKind;

typedef struct CLIFlag {
    CLIFlagKind kind;
    const char *name;
    const char *alias;
    const char **options;
    const char *argumentName;
    const char *help;
    int nOptions;
    union {
        void *raw;
        char *path;
        int *i;
        b32 *b;
        const char **s;
    } ptr;
} CLIFlag;

CompilerFlags default_flags = {
    .parseComments = false,
    .errorCodes    = true,
    .errorColors   = true,
    .errorSource   = true,
    .verbose       = false,
    .version       = false,
    .help          = false,
    .emitIR        = false,
    .emitHeader    = false,
    .dumpIR        = false,
    .debug         = true,
    .link          = true,
};

Compiler parsed_compiler;

static const char *OutputTypeNames[] = {
    "exec",
    "static",
    "dynamic"
};

#define FLAG_BOOL(NAME, SHORT_NAME, PTR, HELP) \
{ CLIFlagKindBool, (NAME), (SHORT_NAME), .ptr.b = &parsed_compiler.PTR, .help = (HELP) }

#define FLAG_ENUM(NAME, PTR, OPTIONS, HELP) \
{ CLIFlagKindEnum, (NAME), .ptr = &parsed_compiler.PTR, .options = (OPTIONS), .nOptions = sizeof(OPTIONS) / sizeof(*OPTIONS), .help = (HELP) }

#define FLAG_PATH(NAME, SHORT_NAME, PTR, ARG_NAME, HELP) \
{ CLIFlagKindPath, (NAME), (SHORT_NAME), .ptr.path = parsed_compiler.PTR, .argumentName = (ARG_NAME), .help = (HELP) }

#define FLAG_STRING(NAME, SHORT_NAME, PTR, ARG_NAME, HELP) \
{ CLIFlagKindString, (NAME), (SHORT_NAME), .ptr.s = &parsed_compiler.PTR, .argumentName = (ARG_NAME), .help = (HELP) }

CLIFlag CLIFlags[] = {
    FLAG_BOOL("help",    "h",  flags.help,    "Print help information"),
    FLAG_BOOL("version", NULL, flags.version, "Prints compiler version"),

    FLAG_BOOL("verbose", "v",  flags.verbose, "Enable verbose output"),
    FLAG_BOOL("dump-ir", NULL, flags.dumpIR,  "Dump LLVM IR"),
    FLAG_BOOL("emit-ir", NULL, flags.emitIR,  "Emit LLVM IR file(s)"),

    FLAG_BOOL("emit-header", NULL, flags.emitHeader, "Emit C header file(s)"),

    FLAG_BOOL("error-codes",  NULL, flags.errorCodes,  "Show error codes along side error location"),
    FLAG_BOOL("error-colors", NULL, flags.errorColors, "Show errors in souce code by highlighting in color"),
    FLAG_BOOL("error-source", NULL, flags.errorSource, "Show source code when printing errors"),

    FLAG_BOOL("parse-comments", NULL, flags.parseComments, ""),
    FLAG_BOOL("debug", "g", flags.debug, "Include debug symbols"),
    FLAG_BOOL("link", NULL, flags.link,  "Link object files"),

    FLAG_PATH("output", "o", output_name, "file", "Output file (default: <input>)"),

    FLAG_ENUM("os", target_os, OsNames, "Target operating system (default: current)"),
    FLAG_ENUM("arch", target_arch, ArchNames, "Target architecture (default: current)"),
    FLAG_ENUM("type", target_output, OutputTypeNames, "Final output type (default: exec)"),
};

CLIFlag *FlagForName(const char *name) {
    size_t nFlags = sizeof(CLIFlags) / sizeof(*CLIFlags);
    for (size_t i = 0; i < nFlags; i++) {
        if (strcmp(CLIFlags[i].name, name) == 0)       return &CLIFlags[i];
        else if (CLIFlags[i].alias && strcmp(CLIFlags[i].alias, name) == 0) return &CLIFlags[i];
    }
    return NULL;
}

void ParseFlags(Compiler *compiler, int *pargc, const char ***pargv) {
    parsed_compiler.flags = default_flags;
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
            CLIFlag *flag = FlagForName(name);
            if (!flag || (inverse && flag->kind != CLIFlagKindBool)) {
                printf("Unknown flag %s\n", arg);
                continue;
            }
            switch (flag->kind) {
                case CLIFlagKindBool:
                    *flag->ptr.b = inverse ? false : true;
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
                            *flag->ptr.i = k;
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
                        path_copy(flag->ptr.path, argv[i]);
                    } else {
                        printf("No value argument after -%s\n", arg);
                    }
                    break;

                case CLIFlagKindString:
                    if (i + 1 < argc) {
                        i++;
                        *flag->ptr.s = argv[i];
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
    *compiler = parsed_compiler;
    *pargc = argc - i;
    *pargv = argv + i;
    if (argc - i == 1) {
        path_copy(compiler->input_name, argv[i]);
    }
}

void PrintUsage(const char *prog_name) {
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
                if (*flag.ptr.b && flag.ptr.b != &compiler.flags.help)
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

#if TEST
void test_flagParsingAndDefaults() {
    InitTestCompiler(&compiler, "-o outputName -v -os Darwin");
    ASSERT(compiler.flags.verbose);
    ASSERT(strcmp(compiler.output_name, "outputName") == 0);
    ASSERT(compiler.target_os == Os_Darwin);

    InitTestCompiler(&compiler, NULL);
//    InitCompiler(&compiler, 0, NULL);
    ASSERT(compiler.target_arch != Arch_Unknown);
    ASSERT(compiler.target_os != Arch_Unknown);
}
#endif
