typedef enum ArgumentType {
    String,
    Bool,
} ArgumentType;

typedef struct CommandLineArgument {
    const char *longFlag;
    const char *shortFlag;
    const char *desc;
    ArgumentType type;
    void *output;
} CommandLineArgument;

typedef struct CommandLineMetadata {
    int argc;
    char **argv;
    DynamicArray(CommandLineArgument) arguments;
    DynamicArray(char*) looseValues;
} CommandLine;

