#include "flags.h"

CLIFlag Flags[] = {
    { CLIFlagKind_Bool, "help", "h", .ptr.b = &FlagHelp,  .help = "Prints help information" },
    { CLIFlagKind_Bool, "version", .ptr.b = &FlagVersion, .help = "Prints version information" },

    { CLIFlagKind_String, "output", "o", .ptr.s = &OutputName, .argumentName = "file", .help = "Output file (default: out_<input>)" },

    { CLIFlagKind_Bool, "verbose", "v", .ptr.b = &FlagVerbose,         .help = "Enable verbose output" },
    { CLIFlagKind_Bool, "dump-ir", .ptr.b = &FlagDumpIR,               .help = "Dump LLVM IR" },
    { CLIFlagKind_Bool, "emit-ir", .ptr.b = &FlagEmitIR,               .help = "Emit LLVM IR file(s)" },
    { CLIFlagKind_Bool, "emit-times", .ptr = NULL,                     .help = "Emit times for each stage of compilation" },
    { CLIFlagKind_Bool, "error-codes", .ptr.b = &FlagErrorCodes,       .help = "Display error codes along side error location" },
    { CLIFlagKind_Bool, "parse-comments", .ptr.b = &FlagParseComments, .help = NULL },

    { CLIFlagKind_Enum, "os", .ptr.i = &TargetOs, .options = OsNames, .nOptions = sizeof(OsNames) / sizeof(*OsNames), .help = "Target operating system (default: current)" },
    { CLIFlagKind_Enum, "arch", .ptr.i = &TargetArch, .options = ArchNames, .nOptions = sizeof(ArchNames) / sizeof(*ArchNames), .help = "Target architecture (default: current)" },
};

CLIFlag *FlagForName(const char *name) {
    size_t nFlags = sizeof(Flags) / sizeof(*Flags);
    for (size_t i = 0; i < nFlags; i++) {
        if (strcmp(Flags[i].name, name) == 0)       return &Flags[i];
        else if (Flags[i].alias && strcmp(Flags[i].alias, name) == 0) return &Flags[i];
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

void InitUnsetFlagsToDefaults() {
    if (OutputName == NULL) {
        // TODO: should we use the basename of InputName?
        size_t prefixLen = sizeof("out_") - 1;
        size_t len = prefixLen + strlen(InputName) + 1;
        char *mem = Alloc(DefaultAllocator, len);
        memcpy(mem, "out_", prefixLen);

        char *filename = mem + prefixLen;

        memcpy(filename, InputName, len - prefixLen);
        RemoveKaiExtension(filename);

        OutputName = mem;
    }

    if (TargetOs == Os_Current) {
        TargetOs = OsForName(CurrentSystem.name);
    }
    if (TargetArch == Arch_Current) {
        TargetArch = ArchForName(CurrentSystem.machine);
    }
}

void PrintUsage() {
    size_t nFlags = sizeof(Flags) / sizeof(*Flags);
    for (size_t i = 0; i < nFlags; i++) {

        char invokation[40];
        int k = 0;
        CLIFlag flag = Flags[i];

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

    InitDetailsForCurrentSystem();

    InitUnsetFlagsToDefaults();
    ASSERT(TargetArch != Arch_Current);
    ASSERT(OutputName == "outputName");
    OutputName = NULL;
    InitUnsetFlagsToDefaults();
    ASSERT(strcmp(OutputName, "out_main") == 0);
}
#endif
