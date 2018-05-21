
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

    while (QueueDequeue(&parsingQueue)) {
        parsePackage(mainPackage);
    }
    printf("File %s has %llu top level statements\n", mainPackage->path, ArrayLen(mainPackage->stmts));

    // Finished parsing
    ArenaFree(&parsingQueue.arena);

    return 0;
}
#endif
