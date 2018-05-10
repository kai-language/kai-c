
#include "common.c"
#include "lexer.c"

// forward declarations
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

    const char *path = argv[1];
    const char *data = ReadFile(path);
    if (!data) {
        perror("ReadFile");
        exit(1);
    }
    Lexer lexer = MakeLexer(data, path);

    Token token;
    while ((token = NextToken(&lexer)).kind != TK_Eof) {
        printf("kind: '%s', lit: '%s'\n", DescribeTokenKind(token.kind), token.val);
    }

    return 0;
}
#endif
