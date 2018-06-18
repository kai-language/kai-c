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

#include "llvm.h"

#define VERSION "0.0.0 (prerelease)"

#ifdef DEBUG
b8 _debugEnabled = true;
#else
b8 _debugEnabled = false;
#endif

#if defined(ASSERTS) || defined(DEBUG)
b8 _assertsEnabled = true;
#else
b8 _assertsEnabled = false;
#endif

#ifndef NO_ERROR_CODES
b8 _errorCodesEnabled = true;
#else
b8 _errorCodesEnabled = false;
#endif

#ifdef DIAGNOSTICS
b8 _diagnosticsEnabled = true;
#else
b8 _diagnosticsEnabled = false;
#endif


void outputVersionAndBuildInfo() {
    printf("%s\n\n", VERSION);

#define checkOrCross(info) \
    printf("%-11s %s\n", "" #info "", _##info##Enabled ? "✅" : "❌")

    checkOrCross(asserts);
    checkOrCross(debug);
    checkOrCross(errorCodes);
    checkOrCross(diagnostics);
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
            CheckerContext ctx = { .scope = pkg.scope };
            b32 shouldRequeue = checkStmt(work->package, work->stmt, &ctx);
            if (shouldRequeue) {
                QueueEnqueue(&checkingQueue, work);
            }
            continue;
        }
        
        break;
    }

    b32 sawErrors = false;
    For (packages) {
        if (HasErrors(packages[i])) {
            OutputReportedErrors(packages[i]);
            sawErrors = true;
        }
    }

    if (!sawErrors) {
        CodegenLLVM(mainPackage);
    }

    ArenaFree(&parsingQueue.arena);
    ArenaFree(&checkingQueue.arena);
    
    return 0;
}
#endif
