
#define VERSION "0.0.0 (prerelease)"

#include "common.c"
#include "targets.c"
#include "os.c"
#include "flags.c"
#include "symbols.h"
#include "types.c"

#include "error.c"
#include "keywords.c"
#include "lexer.c"
#include "lexer2.c"
#include "ast.c"
#include "checker.h"
#include "header.c"
#include "parser.c"
#include "parser2.c"
#include "checker.c"
#include "llvm.h"

#include "compiler.c"

#ifndef TEST
int main(int argc, const char **argv) {
    InitCompiler(&compiler, argc, argv);
    return !Compile(&compiler);
}
#endif
