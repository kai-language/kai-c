
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>

#define DEBUG 1
#define TEST  1

int _fileTestsPassCount = 0;
int _totalTestsPassCount = 0;
int _currentTestAsserted = 0;
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

#define ASSERT_MSG_VA(cond, msg, ...) \
do { \
    if (!(cond)) { \
        assertHandler(__FILE__, (i32)__LINE__, "(" #cond ") " msg, __VA_ARGS__); \
        fprintf(stderr, "\n"); \
        _currentTestAsserted = 1; \
        longjmp(_returnToPerformTestCase, 1); \
    } \
} while(0)

#define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, msg, 0)

#define ASSERT(cond) ASSERT_MSG_VA(cond, "", 0)
#define PANIC(msg) ASSERT_MSG_VA(0, msg, 0)
#define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented", 0);

#include "unity.c"

void setSignalHandlerCheckingError(int sig) {
    struct sigaction sa_new = {0};

    sa_new.sa_sigaction = handleSignal;
    sa_new.sa_flags = SA_SIGINFO;
    if (sigaction(sig, &sa_new, NULL) == -1) {
        perror("failed to set handler for signal");
        exit(1);
    }
}

void _performTestCaseReportingResults(void (*testCase)(void), const char *name) {
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
    colorReset();

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

    printf("src/string.c:\n");
    _performTestCaseReportingResults(test_stringInterning, "test_stringInterning");
    colorGray();
    printf("%3.1f%% success (%d out of 1)\n\n", (double) _fileTestsPassCount / 1.f * 100, _fileTestsPassCount);
    colorReset();
    _fileTestsPassCount = 0;

    printf("src/queue.c:\n");
    _performTestCaseReportingResults(test_queue, "test_queue");
    colorGray();
    printf("%3.1f%% success (%d out of 1)\n\n", (double) _fileTestsPassCount / 1.f * 100, _fileTestsPassCount);
    colorReset();
    _fileTestsPassCount = 0;

    printf("src/compiler.c:\n");
    _performTestCaseReportingResults(test_flagParsingAndDefaults, "test_flagParsingAndDefaults");
    colorGray();
    printf("%3.1f%% success (%d out of 1)\n\n", (double) _fileTestsPassCount / 1.f * 100, _fileTestsPassCount);
    colorReset();
    _fileTestsPassCount = 0;

    // All tests finished

    colorBold();
    printf("%3.1f%% success (%d out of 3)\n", (double) _totalTestsPassCount / 3.f * 100, _totalTestsPassCount);
    colorReset();

    return !(_totalTestsPassCount == 3);
}
