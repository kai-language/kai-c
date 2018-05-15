
bool FlagParseComments;
bool FlagShowErrorCodes;
bool FlagVerbose;

typedef enum CLIFlagKind CLIFlagKind;
enum CLIFlagKind {
    CLIFlagKind_Bool,
    CLIFlagKind_String,
    CLIFlagKind_Enum,
};

typedef struct CLIFlag CLIFlag;
struct CLIFlag {
    CLIFlagKind kind;
    const char *name;
    const char **options;
    const char *argumentName;
    const char *help;
    int nOptions;
    union {
        int *i;
        bool *b;
        const char **s;
    } ptr;
};

DynamicArray(CLIFlag) flags;

void DeclareFlagBool(const char *name, bool *ptr, const char *help) {
    ArrayPush(flags, (CLIFlag){ .kind = CLIFlagKind_Bool, .name = name, .help = help, .ptr.b = ptr });
}

void DeclareFlagString(const char *name, const char **ptr, const char *argumentName, const char *help) {
    ArrayPush(flags, (CLIFlag){ .kind = CLIFlagKind_String, .name = name, .argumentName = argumentName, .help = help, .ptr.s = ptr });
}

void DeclareFlagEnum(const char *name, int *ptr, const char *help, const char **options, int nOptions) {
    ArrayPush(flags, (CLIFlag){ .kind = CLIFlagKind_Enum, .name = name, .help = help, .ptr.i = ptr, .options = options, .nOptions = nOptions });
}

CLIFlag *FlagForName(const char *name) {
    size_t nFlags = ArrayLen(flags);
    for (size_t i = 0; i < nFlags; i++) {
        if (strcmp(flags[i].name, name) == 0) return &flags[i];
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
}

void PrintUsage() {
    size_t nFlags = ArrayLen(flags);
    for (size_t i = 0; i < nFlags; i++) {
        CLIFlag flag = flags[i];
        printf(" -%-32s %s\n", flag.name, flag.help ?: "");
    }
}
