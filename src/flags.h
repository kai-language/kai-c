
#ifndef flags_h
#define flags_h

typedef struct Compiler Compiler;
typedef struct CompilerFlags CompilerFlags;

extern CompilerFlags default_flags;

typedef enum Output {
    OutputType_Exec = 0,
    OutputType_Static,
    OutputType_Dynamic
} Output;

struct CompilerFlags {
    b32 parseComments;
    b32 errorCodes;
    b32 errorColors;
    b32 errorSource;
    b32 verbose;
    b32 version;
    b32 help;
    b32 emitIR;
    b32 emitHeader;
    b32 dumpIR;
    b32 debug;
    b32 link;
};

void ParseFlags(Compiler *compiler, int *pargc, const char ***pargv);
void InitUnsetFlagsToDefaults(void);
void PrintUsage(const char *prog_name);

#endif
