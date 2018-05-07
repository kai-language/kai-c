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

#if 0
void Backtrace() {
    #if SYSTEM_POSIX
        void* callstack[25];
        int i, frames = backtrace(callstack, ArrayCount(callstack));
        char** strs = backtrace_symbols(callstack, frames);
        for (i = 0; i < frames; ++i) {
            fprintf(stderr, "%s\n", strs[i]);
        }
        free(strs);
    #elif SYSTEM_WINDOWS
        UNIMPLEMENTED();
    #endif
}

void assertHandler(char const *file, i32 line, char const *msg, ...) {
    Backtrace();

    fprintf(stderr, "Assert failure");

    if (msg) {
        fprintf(stderr, ": ");
        va_list va;
        va_start(va, msg);
        vfprintf(stderr, msg, va);
        va_end(va);
    }

    fprintf(stderr, "\n");
}
#endif

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
        printf("kind: '%.*s', lit: '%.*s'\n", LIT(DescribeTokenKind(token.kind)), LIT(token.lit));
    }

    return 0;
}
