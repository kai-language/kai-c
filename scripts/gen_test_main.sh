#!/bin/bash

cat <<- EOF

#include "stdio.h"
#include "setjmp.h"
#include "signal.h"

int _fileTestsPassCount = 0;
int _totalTestsPassCount = 0;
int _currentTestAsserted = 0;
int _shouldNoteTestLog = 0;

jmp_buf returnToTestMain;

void handleSignal(int sig, siginfo_t *info, void *where) {
    longjmp(returnToTestMain, 1);
}

#define ASSERT_MSG_VA(cond, msg, ...) do { \
        if (!(cond)) { \
            assertHandler(__FILE__, (i32)__LINE__, msg, __VA_ARGS__); \
            fprintf(stderr, "\n"); \
            _currentTestAsserted = 1; \
            _shouldNoteTestLog = 1; \
            raise(SIGINT); \
        } \
    } while(0)

    #define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, msg, 0)

    #define ASSERT(cond) ASSERT_MSG_VA(cond, 0, 0)
    #define PANIC(msg) ASSERT_MSG_VA(0, msg, 0)
    #define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented", 0);

#define TEST_ASSERT(cond) \
if (!cond) { \
    _currentTestAsserted = 1; \
    raise(SIGINT); \
    return; \
}

#include "src/main.cpp"

void printTest(size_t sz, int padch, const char* name, int success) {
    size_t width = strlen(name);

    if (!success) sz -= 4;

    // Pad if it's shorter than desired.

    // Then output the actual thing.

    printf("  %s", name);

    putchar(' ');

    while (width++ <= sz)
        putchar(padch);

    putchar(' ');

    printf("%s\n", success ? "OK" : "FAILED");
}

void setSignalHandlerCheckingError(int sig) {
    struct sigaction sa_new = {0};

    sa_new.sa_sigaction = handleSignal;
    sa_new.sa_flags = SA_SIGINFO;
    if (sigaction(sig, &sa_new, NULL) == -1) {
        perror("failed to set handler for signal");
        exit(1);
    }
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
        cat <<- EOF
    if (setjmp(returnToTestMain) == 0)
EOF
        echo "$testCase" | awk -F"({|void|\\\()" '{if($1) print "        " $1 "();"}'
        cat <<- EOF

    printTest(60, '.', "$testCase", _currentTestAsserted ? 0 : 1);
    _fileTestsPassCount += _currentTestAsserted ? 0 : 1;
    _totalTestsPassCount += _currentTestAsserted ? 0 : 1;
    _currentTestAsserted = 0;

    // ------
    // End of $testCase


EOF
    done

    let "totalTests+=nTests"

    cat <<- EOF
    printf("%3.1f%% success (%d out of $nTests)\\n", (double) _fileTestsPassCount / $nTests.f * 100, _fileTestsPassCount);
    _fileTestsPassCount = 0;
EOF
done

cat <<- EOF

    // All tests finished executing

    printf("Executed $totalTests tests, with %d failures\n", $totalTests - _totalTestsPassCount);
    if (_shouldNoteTestLog)
        printf("\nNote: Some errors occured in called functions for details on these see tests.log\n");

    return 0;
}

EOF

