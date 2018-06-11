
#include "flags.h"

bool FlagParseComments;
bool FlagErrorCodes = true;
bool FlagVerbose;
bool FlagVersion;
bool FlagHelp;
bool FlagEmitIR;
bool FlagDumpIR;
bool FlagDebug = true;

const char *InputName;
const char *OutputName;
int TargetOs;
int TargetArch;

CLIFlag CLIFlags[] = {
    { CLIFlagKind_Bool, "help", "h", .ptr.b = &FlagHelp,  .help = "Prints help information" },
    { CLIFlagKind_Bool, "version", .ptr.b = &FlagVersion, .help = "Prints version information" },

    { CLIFlagKind_String, "output", "o", .ptr.s = &OutputName, .argumentName = "file", .help = "Output file (default: out_<input>)" },

    { CLIFlagKind_Bool, "verbose", "v", .ptr.b = &FlagVerbose,         .help = "Enable verbose output" },
    { CLIFlagKind_Bool, "dump-ir", .ptr.b = &FlagDumpIR,               .help = "Dump LLVM IR" },
    { CLIFlagKind_Bool, "emit-ir", .ptr.b = &FlagEmitIR,               .help = "Emit LLVM IR file(s)" },
    { CLIFlagKind_Bool, "emit-times", .ptr = NULL,                     .help = "Emit times for each stage of compilation" },
    { CLIFlagKind_Bool, "error-codes", .ptr.b = &FlagErrorCodes,       .help = "Display error codes along side error location" },
    { CLIFlagKind_Bool, "parse-comments", .ptr.b = &FlagParseComments, .help = NULL },
    { CLIFlagKind_Bool, "debug", "g",  .ptr.b = &FlagDebug,            .help = "Include debug symbols"},

    { CLIFlagKind_Enum, "os", .ptr.i = &TargetOs, .options = OsNames, .nOptions = sizeof(OsNames) / sizeof(*OsNames), .help = "Target operating system (default: current)" },
    { CLIFlagKind_Enum, "arch", .ptr.i = &TargetArch, .options = ArchNames, .nOptions = sizeof(ArchNames) / sizeof(*ArchNames), .help = "Target architecture (default: current)" },
};

CLIFlag *FlagForName(const char *name) {
    size_t nFlags = sizeof(CLIFlags) / sizeof(*CLIFlags);
    for (size_t i = 0; i < nFlags; i++) {
        if (strcmp(CLIFlags[i].name, name) == 0)       return &CLIFlags[i];
        else if (CLIFlags[i].alias && strcmp(CLIFlags[i].alias, name) == 0) return &CLIFlags[i];
    }
    return NULL;
}

void ParseFlags(int *pargc, const char ***pargv) {
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
            if (!flag || (inverse && flag->kind != CLIFlagKind_Bool)) {
                printf("Unknown flag %s\n", arg);
                continue;
            }
            switch (flag->kind) {
                case CLIFlagKind_Bool:
                    *flag->ptr.b = inverse ? false : true;
                    break;

                case CLIFlagKind_String:
                    if (i + 1 < argc) {
                        i++;
                        *flag->ptr.s = argv[i];
                    } else {
                        printf("No value argument after -%s\n", arg);
                    }
                    break;

                case CLIFlagKind_Enum: {
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
        InputName = argv[i];
    }
}

bool HaveInitializedUnsetFlagsToDefaults = false;
void InitUnsetFlagsToDefaults() {
    if (HaveInitializedUnsetFlagsToDefaults) return;
    HaveInitializedUnsetFlagsToDefaults = true;

    if (OutputName == NULL) {
        // TODO: should we use the basename of InputName?
        size_t prefixLen = sizeof("out_") - 1;

        static char outputPathBuff[MAX_PATH];
        OutputName = GetFileName(InputName, outputPathBuff, NULL);

        size_t len = prefixLen + strlen(OutputName) + 1;
        char *mem = Alloc(DefaultAllocator, len);
        memcpy(mem, "out_", prefixLen);

        char *filename = mem + prefixLen;

        memcpy(filename, OutputName, len - prefixLen);
        RemoveKaiExtension(filename);

        OutputName = mem;
    }

    InitDetailsForCurrentSystem();
    if (TargetOs == Os_Current) {
        TargetOs = OsForName(CurrentSystem.name);
    }
    if (TargetArch == Arch_Current) {
        TargetArch = ArchForName(CurrentSystem.machine);
    }

    if (TargetOs == -1 || TargetArch == -1) {
        printf("Unsupported Os or Arch: %s %s\n", OsNames[TargetOs], ArchNames[TargetArch]);
        exit(1);
    }
}

void PrintUsage() {
    size_t nFlags = sizeof(CLIFlags) / sizeof(*CLIFlags);
    for (size_t i = 0; i < nFlags; i++) {

        char invokation[40];
        int k = 0;
        CLIFlag flag = CLIFlags[i];

        if (flag.alias) k = snprintf(invokation, sizeof(invokation), "-%s ", flag.alias);
        k += snprintf(invokation + k, sizeof(invokation) - k, "-%s", flag.name);

        switch (flag.kind) {
            case CLIFlagKind_String:
                k += snprintf(invokation + k, sizeof(invokation) - k, " <%s>", flag.argumentName);
                break;

            case CLIFlagKind_Enum:
                ASSERT(flag.nOptions > 0);
                k += snprintf(invokation + k, sizeof(invokation) - k, " <");
                k += snprintf(invokation + k, sizeof(invokation) - k, "%s", flag.options[0]);
                for (int i = 1; i < flag.nOptions; i++) {
                    k += snprintf(invokation + k, sizeof(invokation) - k, "|%s", flag.options[i]);
                }
                k += snprintf(invokation + k, sizeof(invokation) - k, ">");
                break;

            case CLIFlagKind_Bool:
                break;
        }
        printf(" %-40s %s\n", invokation, flag.help ? flag.help : "");
    }
}

#if TEST
void test_flagParsingAndDefaults() {
    const char *args[] = {"kai", "-o", "outputName", "-v", "-os", "Darwin", "main.kai"};
    int argc = sizeof(args) / sizeof(*args);
    const char **argv = args;
    ParseFlags(&argc, &(argv));
    ASSERT(FlagVerbose);
    ASSERT(strcmp(OutputName, "outputName") == 0);
    ASSERT(TargetOs == Os_Darwin);
    ASSERT(argc == 1);

    REINIT_COMPILER();
    ASSERT(TargetArch != Arch_Current);
    ASSERT(TargetOs != Arch_Current);
    OutputName = NULL;

    REINIT_COMPILER();
    InputName = "src/main.kai";
    HaveInitializedUnsetFlagsToDefaults = false;
    OutputName = NULL;
    InitUnsetFlagsToDefaults();
    printf("%s\n", OutputName);
    ASSERT(strcmp(OutputName, "out_main") == 0);

    REINIT_COMPILER();
}
#endif
