
bool FlagParseComments;
bool FlagShowErrorCodes;
bool FlagVerbose = true;

void parseFlag(const char *flagName, bool *out, int argc, char **argv) {
    ASSERT(out)
    
    for (int i = 1; i < argc; i++) {
        if (strlen(argv[i]) == 0 || *argv[i] != '-') {
            perror("Flag was not prefixed with '-'");
            exit(1);
        }
        
        if (strcmp(argv[i] + 1, flagName) == 0) {
            *out = true;
            return;
        }
    }
}
