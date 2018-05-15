
#include "common.c"

#include "lexer.c"
#include "compiler.c"
#include "parser.h"
#include "checker.h"

#include "parser.c"
#include "checker.c"

#ifndef TEST
int main(int argc, const char **argv) {

    const char *outputName = NULL;

    DeclareFlagString("o", &outputName, "file", "Output file");
    DeclareFlagBool("show-error-codes", &FlagShowErrorCodes, "display error codes along side error location");
    DeclareFlagBool("parse-comments", &FlagParseComments, NULL);
    DeclareFlagBool("verbose", &FlagVerbose, "Print diagnostics");
    DeclareFlagBool("dump-ir", NULL, "Dump LLVM IR");
    DeclareFlagBool("emit-ir", NULL, "Emit LLVM IR file(s)");
    DeclareFlagBool("emit-times", NULL, "Emit times for each stage of compilation");

    const char *programName = argv[0];
    ParseFlags(&argc, &argv);
    if (argc != 1) {
        printf("Usage: %s [flags] <input>\n", programName);
        PrintUsage();
        return 1;
    }

    const char *packageName = argv[0];

    InitCompiler();
    Package *mainPackage = ImportPackage(packageName);

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
