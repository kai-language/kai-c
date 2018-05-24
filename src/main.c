
#include "common.c"

#include "error.c"
#include "compiler.c"
#include "lexer.c"
#include "ast.c"
#include "symbols.c"
#include "types.c"

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

    while (true) {
        Package *package = QueueDequeue(&parsingQueue);
        if (package) {
            parsePackage(package);
            continue;
        }

        break;
    }
    printf("File %s has %zu top level statements\n", mainPackage->path, ArrayLen(mainPackage->stmts));

    // Finished parsing
    ArenaFree(&parsingQueue.arena);

    return 0;
}
#endif
