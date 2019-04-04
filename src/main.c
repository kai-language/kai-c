
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
#include "llvm.h"

#include "compiler.c"

#ifndef TEST
int main(int argc, const char **argv) {
    InitCompiler(&compiler, argc, argv);
    InitKeywords();

    return !Compile(&compiler);
}
#endif
