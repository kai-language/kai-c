#include "common.c"
#include "lexer.c"

// forward declarations
#include "compiler.c"

#include "parser.h"
#include "checker.h"
#include "arguments.h"

#include "parser.c"
#include "checker.c"
#include "arguments.c"

#ifndef TEST
int main(int argc, char **argv) {
    struct CommandLineMetadata cliMetadata = {
        .argc = argc,
        .argv = argv,
        .arguments = NULL
    };
    
    RegisterBoolArgument("show-error-codes", NULL, "Shows error details when an error occurs", &FlagShowErrorCodes, &cliMetadata);
    RegisterBoolArgument("parse-comments", NULL, "<TODO>", &FlagShowErrorCodes, &cliMetadata);
    RegisterBoolArgument("verbose", "v", "Verbosely prints compiler progress", &FlagVerbose, &cliMetadata);
    
    ParseFlags(&cliMetadata);
    
    if (argc < 2) {
        PrintUsage(cliMetadata);
        return 1;
    }
    
    InitCompiler();

    // FIXME: We need to work out what argv refers to the file
    Package *mainPackage = ImportPackage(argv[1]);

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
