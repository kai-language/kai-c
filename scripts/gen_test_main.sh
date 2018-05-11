#!/bin/bash

cat <<- EOF

#include "stdio.h"
#include "setjmp.h"
#include "signal.h"

int _fileTestsPassCount = 0;
int _totalTestsPassCount = 0;
int _currentTestAsserted = 0;
int _shouldNoteTestLog = 0;
const char *_currentTestCaseName;

jmp_buf _returnToPerformTestCase;

void colorBold() {
#ifndef NO_COLOR
    printf("\033[1m");
#endif
}

void colorGreen() {
#ifndef NO_COLOR
    printf("\033[32m");
#endif
}

void colorRed() {
#ifndef NO_COLOR
    printf("\033[31m");
#endif
}

void colorGray() {
#ifndef NO_COLOR
    printf("\033[90m");
#endif
}

void colorReset() {
#ifndef NO_COLOR
    printf("\033[0m");
#endif
}

void handleSignal(int sig, siginfo_t *info, void *where) {
    longjmp(_returnToPerformTestCase, 1);
}

#define ASSERT_MSG_VA(cond, msg, ...) do { \\
        if (!(cond)) { \\
            assertHandler(__FILE__, (i32)__LINE__, msg, __VA_ARGS__); \\
            fprintf(stderr, "\n"); \\
            _currentTestAsserted = 1; \\
            _shouldNoteTestLog = 1; \\
            raise(SIGINT); \\
        } \\
    } while(0)

#define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, msg, 0)

#define ASSERT(cond) ASSERT_MSG_VA(cond, 0, 0)
#define PANIC(msg) ASSERT_MSG_VA(0, msg, 0)
#define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented", 0);

#define TEST_ASSERT(cond) \\
if (!(cond)) { \\
    fprintf(stderr, "Assert failure: %s:%d: %s\n", __FILE__, (i32)__LINE__, "" #cond ""); \
    _currentTestAsserted = 1; \\
    raise(SIGINT); \\
    return; \\
}

#include "src/main.c"

void setSignalHandlerCheckingError(int sig) {
    struct sigaction sa_new = {0};

    sa_new.sa_sigaction = handleSignal;
    sa_new.sa_flags = SA_SIGINFO;
    if (sigaction(sig, &sa_new, NULL) == -1) {
        perror("failed to set handler for signal");
        exit(1);
    }
}

void _performTestCaseReportingResults(void (*testCase)(), const char *name) {
    _currentTestCaseName = name;
    if (setjmp(_returnToPerformTestCase) == 0)
        testCase();

    int success = _currentTestAsserted ? 0 : 1;

    size_t width = strlen(name);

    int approxWidth = 60;

    if (!success) approxWidth -= 4;

    // Pad if it's shorter than desired.

    // Then output the actual thing.

    printf("  %s", name);

    putchar(' ');

    while (width++ <= approxWidth)
        putchar('.');

    putchar(' ');

    success ? colorGreen() : colorRed();
    printf("%s\n", success ? "OK" : "FAILED");

    _fileTestsPassCount += _currentTestAsserted ? 0 : 1;
    _totalTestsPassCount += _currentTestAsserted ? 0 : 1;
    _currentTestAsserted = 0;
}

int main() {
    setSignalHandlerCheckingError(SIGINT);
    setSignalHandlerCheckingError(SIGABRT);
    setSignalHandlerCheckingError(SIGILL);
    setSignalHandlerCheckingError(SIGSEGV);
    setSignalHandlerCheckingError(SIGFPE);
    setSignalHandlerCheckingError(SIGBUS);
    setSignalHandlerCheckingError(SIGPIPE);

EOF

totalTests=0
for file in src/*; do
    nTests=$(grep -c '#if TEST' $file)
    matches=$(awk '/^#if TEST/ {getline; print}' $file)
    if [ -z "$matches" ]; then
        continue
    fi
cat <<- EOF
    printf("$file:\\n");
EOF
    for testCase in $matches; do
        if [[ "$testCase" = "void" ]] || [[ "$testCase" = "{" ]]; then
            continue
        fi
        testCase=$(echo "$testCase" | awk -F"({|void|\\\()" '{if($1) print $1}')
cat <<- EOF
    _performTestCaseReportingResults($testCase, "$testCase");
EOF
    done

    let "totalTests+=nTests"

cat <<- EOF
    colorGray();
    printf("%3.1f%% success (%d out of $nTests)\\n\\n", (double) _fileTestsPassCount / $nTests.f * 100, _fileTestsPassCount);
    colorReset();
    _fileTestsPassCount = 0;

EOF
done

cat <<- EOF

    // All tests finished executing

    colorGray();
    if (_shouldNoteTestLog)
        printf("Note: Some errors occured in called functions for details on these see tests.log\n");
    colorReset();

    colorBold();
    printf("%3.1f%% success (%d out of $totalTests)\n", (double) _totalTestsPassCount / $totalTests.f * 100, _totalTestsPassCount);
    colorReset();

    return !(_totalTestsPassCount == $totalTests);
}

EOF

