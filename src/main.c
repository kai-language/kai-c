#include "common.c"

#include "error.c"
#include "lexer.c"
#include "compiler.c"
#include "ast.c"
#include "symbols.c"
#include "types.c"

#include "checker.h"
#include "parser.c"
#include "checker.c"

#include "llvm.h"

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
        
        CheckerWork *work = QueueDequeue(&checkingQueue);
        if (work) {
            b32 shouldRequeue = check(work->package, work->stmt);
            if (shouldRequeue) {
                QueueEnqueue(&checkingQueue, work);
            }
            continue;
        }
        
        break;
    }
    printf("File %s has %zu top level statements\n", mainPackage->path, ArrayLen(mainPackage->stmts));
    
    // Finished parsing
    ArenaFree(&parsingQueue.arena);
    ArenaFree(&checkingQueue.arena);
    
    return 0;
}
#endif
