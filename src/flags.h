
extern bool FlagParseComments;
extern bool FlagErrorCodes;
extern bool FlagVerbose;
extern bool FlagVersion;
extern bool FlagHelp;
extern bool FlagEmitIR;
extern bool FlagDumpIR;
extern bool FlagDebug;

extern const char *InputName;
extern const char *OutputName;

extern int TargetOs;
extern int TargetArch;
extern int OutputType;

typedef enum OutputTypes {
    OutputType_Exec = 0,
    OutputType_Static,
    OutputType_Dynamic
} OutputTypes;

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

