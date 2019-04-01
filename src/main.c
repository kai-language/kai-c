
#define VERSION "0.0.0 (prerelease)"

#include "common.c"
#include "targets.c"
#include "os.c"
#include "flags.c"
#include "symbols.h"
#include "types.c"

#include "error.c"
#include "lexer.c"
#include "ast.c"
#include "checker.h"
#include "header.c"
#include "parser.c"
#include "checker.c"

#include "compiler.c"

#include "llvm.h"

#ifndef TEST
int main(int argc, const char **argv) {
    InitCompiler(&compiler, argc, argv);
    InitKeywords();

    Package *builtinPackage = ImportPackage("builtin", NULL);
    if (!builtinPackage) {
        printf("error: Failed to compile builtin package\n");
        exit(1);
    }

    Package *mainPackage = ImportPackage(compiler.input_name, NULL);
    if (!mainPackage) {
        printf("error: Failed to compile '%s'\n", compiler.input_name);
        exit(1);
    }
    
    while (true) {
        Source *file = QueuePopFront(&compiler.parsing_queue);
        if (file) {
            parseSource(file);
            continue;
        }
        
        CheckerWork *work = QueuePopFront(&compiler.checking_queue);
        if (work) {
            if (compiler.flags.verbose) printf("Checking package %s\n", work->package->path);
            CheckerContext ctx = { .scope = work->package->scope };
            checkStmt(work->stmt, &ctx, work->package);
            if (ctx.mode == ExprMode_Unresolved) {
                QueuePushBack(&compiler.checking_queue, work);
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

        if (compiler.target_output != OutputType_Exec || compiler.flags.emitHeader)
            CodegenCHeader(mainPackage);
    } else {
        return 1;
    }

    return 0;
}
#endif
