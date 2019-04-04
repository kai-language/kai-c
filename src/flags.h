
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
    bool parseComments;
    bool errorCodes;
    bool errorColors;
    bool errorSource;
    bool verbose;
    bool version;
    bool help;
    bool emitIR;
    bool emitHeader;
    bool dumpIR;
    bool debug;
    bool link;
};

void ParseFlags(Compiler *compiler, int *pargc, const char ***pargv);
void InitUnsetFlagsToDefaults(void);
void PrintUsage(const char *prog_name);

#endif
