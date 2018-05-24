bool FlagParseComments;
bool FlagErrorCodes;
bool FlagVerbose;
bool FlagVersion;
bool FlagHelp;
bool FlagEmitIR;
bool FlagDumpIR;

const char *InputName;
const char *OutputName;
int TargetOs;
int TargetArch;

typedef enum CLIFlagKind {
    CLIFlagKind_Bool,
    CLIFlagKind_String,
    CLIFlagKind_Enum,
} CLIFlagKind;

typedef struct CLIFlag CLIFlag;
struct CLIFlag {
    CLIFlagKind kind;
    const char *name;
    const char *alias;
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

CLIFlag *FlagForName(const char *name);
void ParseFlags(int *pargc, const char ***pargv);
void InitUnsetFlagsToDefaults();
void PrintUsage();

