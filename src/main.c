#include "common.c"
#include "lexer.c"

// forward declarations
#include "compiler.c"

#include "parser.h"
#include "checker.h"

#include "parser.c"
#include "checker.c"

#define IndentSize 2

void printUsage(u32 indentLevel, const char *fmt, ...) {
    while (indentLevel --> 0) {
        fprintf(stderr, "%*s", IndentSize, "");
    }

    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);

    fprintf(stderr, "\n");
}

void Usage() {
    printUsage(0, "OVERVIEW: Kai compiler\n");

    printUsage(0, "USAGE: kai [options] <input>\n");

    printUsage(0, "OPTIONS:");
    printUsage(1, "-dump-ir               Dump LLVM IR");
    printUsage(0, "");

    printUsage(1, "-emit-ir               Emit LLVM IR file(s)");
    printUsage(1, "-emit-times            Emit times for each stage of compilation");
}

#ifndef TEST
int main(int argc, char **argv) {
    if (argc < 2) {
        Usage();
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
