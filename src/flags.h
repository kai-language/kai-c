
extern bool FlagParseComments;
extern bool FlagErrorCodes;
extern bool FlagVerbose;
extern bool FlagVersion;
extern bool FlagHelp;
extern bool FlagEmitIR;
extern bool FlagDumpIR;

extern const char *InputName;
extern const char *OutputName;
extern int TargetOs;
extern int TargetArch;

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
void InitUnsetFlagsToDefaults(void);
void PrintUsage(void);

