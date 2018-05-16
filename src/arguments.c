#define IndentSize 2

void ParseFlags(struct CommandLineMetadata *arguments) {
    for (int i = 1; i < arguments->argc; i++) {
        if (strlen(arguments->argv[i]) == 0 || *arguments->argv[i] != '-') {
            continue;
        }
        
        size_t argumentCount = ArrayLen(arguments->arguments);
        
        for (int j = 0; j < argumentCount; j++) {
            char *flagStart = arguments->argv[i] + 1;
            
            bool matchingFlag = false;
            const char *longFlag = arguments->arguments[j].longFlag;
            const char *shortFlag = arguments->arguments[j].shortFlag;
            
            if (longFlag && strcmp(flagStart, longFlag) == 0) {
                matchingFlag = true;
            } else if (shortFlag && strcmp(flagStart, shortFlag) == 0) {
                matchingFlag = true;
            }
            
            // TODO: Flag names until `=` marker
            if (matchingFlag) {
                switch (arguments->arguments[j].type) {
                    case String: {
                        char **output = arguments->arguments[j].output;
                        
                        ++i;
                        
                        if (i < arguments->argc) {
                            *output = arguments->argv[i];
                        } else {
                            // TODO: Split string by `=` for values
                            *output = "";
                        }
                    }
                    case Bool: {
                        bool *output = arguments->arguments[j].output;
                        
                        *output = true;
                    }
                }
            } else {
                ArrayPush(arguments->looseValues, arguments->argv[i]);
            }
        }
    }
}

void printLine(u32 indentLevel, const char *fmt, ...) {
    while (indentLevel --> 0) {
        fprintf(stderr, "%*s", IndentSize, "");
    }
    
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    
    fprintf(stderr, "\n");
}

void PrintUsage(struct CommandLineMetadata metadata) {
    printLine(0, "OVERVIEW: Kai compiler\n");
    
    printLine(0, "USAGE: kai [options] <input>\n");
    printLine(0, "OPTIONS:");
    
    for (int i = 0; i < ArrayLen(metadata.arguments); i++) {
        printLine(1, "-%s    %s", metadata.arguments[i].longFlag, metadata.arguments[i].desc);
    }
}

void RegisterBoolArgument(const char *longFlag, const char *shortFlag, const char *desc, bool *out, struct CommandLineMetadata *arguments) {
    CommandLineArgument argument = {
        .longFlag = longFlag,
        .shortFlag = shortFlag,
        .desc = desc,
        .type = Bool,
        .output = out
    };
    
    ArrayPush(arguments->arguments, argument);
}

void RegisterStringArgument(const char *longFlag, const char *shortFlag, const char *desc, char **out, struct CommandLineMetadata *arguments) {
    CommandLineArgument argument = {
        .longFlag = longFlag,
        .shortFlag = shortFlag,
        .desc = desc,
        .type = String,
        .output = out
    };
    
    ArrayPush(arguments->arguments, argument);
}
