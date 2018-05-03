#include "common.cpp"
#include "lexer.cpp"

// forward declarations
#include "parser.hpp"
#include "checker.hpp"

#include "parser.cpp"
#include "checker.cpp"

#define IndentSize 2

void printUsage(u32 indentLevel, char *fmt, ...) {
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

void assert_handler(char const *file, i32 line, char const *msg, ...) {
    if (msg) {
        fprintf(stderr, "Assert failure: %s:%d: %s\n", file, line, msg);
    }
    else {
        fprintf(stderr, "Assert failure: %s:%d\n", file, line);
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        Usage();
        return 1;
    }

    String path = MakeCString(argv[1]);
    Lexer _lexer;
    Lexer *lexer = &_lexer;
    b32 ok = MakeLexer(lexer, path);
    if (!ok) {
        printf("ERROR: failed to open file: %*.s\n", LIT(path));
        return 2;
    }

    Token token;
    while ((token = NextToken(lexer)).kind != TK_Eof) {
        printf("test %p", &token);
    }

    return 0;
}
