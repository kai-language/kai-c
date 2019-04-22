
#define UNITY 1

#include "all.h"
#include "profiler.h"
#include "src/os.c"

#include "src/arena.c"
#include "src/queue.c"
#include "src/string.c"

#include "src/package.c"
#include "src/compiler.c"
#include "src/utf.c"
#include "src/lexer.c"
#include "src/parser.c"
#include "src/ast.c"
#include "src/types.c"
#include "src/checker.c"

#ifdef TEST
#define TEST 1
#endif

#include "src/main.c"

#if TEST
#include "src/tests/parser.c"
#include "src/tests/checker.c"
#endif
