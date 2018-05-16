
#include "common.c"

#include "lexer.c"
#include "compiler.c"
#include "parser.h"
#include "checker.h"

#include "parser.c"
#include "checker.c"

#define VERSION "0.0.0 (prerelease)"

#ifndef TEST
int main(int argc, const char **argv) {

    const char *programName = argv[0];
    ParseFlags(&argc, &argv);
    if (argc != 1 || FlagHelp) {
        printf("Usage: %s [flags] <input>\n", programName);
        PrintUsage();
        exit(!FlagHelp);
    }
    if (FlagVersion) {
        printf(VERSION);
        exit(0);
    }

    InitCompiler();
    Package *mainPackage = ImportPackage(InputName);
    if (!mainPackage) {
        printf("error: Failed to compile '%s'\n", InputName);
        exit(1);
    }

    u64 tokCount = 0;
    while (QueueDequeue(&parsingQueue)) {
        void *mem = ReadFile(mainPackage->fullPath);
        if (!mem) {
            printf("Failed to open file %s\n", mainPackage->fullPath);
            exit(1);
        }
        Lexer lex = MakeLexer(mem, mainPackage->path);
        Token tok = NextToken(&lex);
        for (u64 i = 0; tok.kind != TK_Eof; tok = NextToken(&lex), i++) {
            tokCount = i;
        }
        printf("File %s has %llu tokens\n", mainPackage->path, tokCount);
    }

    // Finished parsing
    ArenaFree(&parsingQueue.arena);

    return 0;
}
#endif
