#include "common.c"

#include "error.c"
#include "lexer.c"
#include "compiler.c"
#include "ast.c"
#include "symbols.h"
#include "types.c"

#include "checker.h"
#include "parser.c"
#include "checker.c"
#include "header.c"

#include "llvm.h"

#define VERSION "0.0.0 (prerelease)"


void outputVersionAndBuildInfo() {
    printf("%s\n\n", VERSION);

    bool debug = false;

#if DEBUG
    debug = true;
#endif

    const char *y = "✔";
    const char *n = "✘";

    printf("-DDEBUG %s\n", debug ? y : n);
}

#ifndef TEST
int main(int argc, const char **argv) {
    const char *programName = argv[0];

    ParseFlags(&argc, &argv);
    if (FlagVersion) {
        outputVersionAndBuildInfo();
        exit(0);
    }
    if (argc != 1 || FlagHelp) {
        printf("Usage: %s [flags] <input>\n", programName);
        PrintUsage();
        exit(!FlagHelp);
    }
        
    InitCompiler();
    Package *mainPackage = ImportPackage(InputName, NULL);
    if (!mainPackage) {
        printf("error: Failed to compile '%s'\n", InputName);
        exit(1);
    }
    
    while (true) {
        Package *package = QueuePopFront(&parsingQueue);
        if (package) {
            parsePackage(package);
            continue;
        }
        
        CheckerWork *work = QueuePopFront(&checkingQueue);
        if (work) {
            CheckerContext ctx = { .scope = work->package->scope };
            checkStmt(work->stmt, &ctx, work->package);
            if (ctx.mode == ExprMode_Unresolved) {
                QueuePushBack(&checkingQueue, work);
            }
            continue;
        }
        
        break;
    }

    b32 sawErrors = false;
    size_t numPackages = ArrayLen(packages);
    for (size_t i = 0; i < numPackages; i++) {
        if (HasErrors(packages[i])) {
            OutputReportedErrors(packages[i]);
            sawErrors = true;
        }
    }

    if (!sawErrors) {
        CodegenLLVM(mainPackage);

        if (OutputType != OutputType_Exec || FlagEmitHeader)
            CodegenCHeader(mainPackage);
    } else {
        return 1;
    }

    ArenaFree(&parsingQueue.arena);
    ArenaFree(&checkingQueue.arena);
    
    return 0;
}
#endif
