
#define false 0
#define true !false

#define MAX 4096


#if defined(__unix__) || defined(__APPLE__)

//
//
// Prelude
//
char prelude[] =
R"(
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

)";

#else
#error "Platform not yet supported"
#endif


#if defined(__unix__) || defined(__APPLE__)
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <assert.h>

typedef struct DirectoryEntry {
    char name[MAX];
    int isFile;
    int isDirectory;
} DirectoryEntry;

typedef struct DirectoryIter {
    int valid;
    int error;
    DirectoryEntry current;
    void *handle;
} DirectoryIter;

void DirectoryIterClose(DirectoryIter *it) {
    if (it->valid) {
        it->valid = false;
        it->error = false;
        closedir((DIR *)it->handle);
    }
}

DirectoryEntry *DirectoryIterNext(DirectoryIter *it) {
    if (!it->valid) return NULL;
    do {
        struct dirent *entry = readdir((DIR *)it->handle);
        if (!entry) {
            DirectoryIterClose(it);
            return NULL;
        }
        strncpy(it->current.name, entry->d_name, MAX);
        it->current.name[MAX - 1] = 0;
        it->current.isFile = entry->d_type & DT_REG;
        it->current.isDirectory = entry->d_type & DT_DIR;
    } while (strncmp(".", it->current.name, 1) == 0 || strncmp("..", it->current.name, 2) == 0);

    return &it->current;
}

DirectoryIter DirectoryIterOpen(const char *path) {
    DirectoryIter it = {0};
    DIR *dir = opendir(path);
    if (!dir) {
        it.valid = false;
        it.error = true;
        return it;
    }
    it.handle = dir;
    strncpy(it.current.name, path, MAX);
    it.current.name[MAX - 1] = 0;
    it.valid = true;
    return it;
}

#else
#error "Platform not yet supported"
#endif

void eatSpace(char **stream) {
    while (isspace(**stream)) {
        (*stream)++;
    }
}

int totalTests = 0;

void discoverTests(const char *directoryPath) {

    char path[MAX];
    char *scratch = (char*)calloc(MAX, 1);

    size_t dirPathLen = strlen(directoryPath);
    strncpy(path, directoryPath, dirPathLen);
    if (path[dirPathLen - 1] != '/') {
        path[dirPathLen++] = '/';
    }
    path[dirPathLen] = '\0';

    DirectoryIter iter = DirectoryIterOpen(directoryPath);
    for (DirectoryEntry *entry; (entry = DirectoryIterNext(&iter));) {
        if (entry->isDirectory) continue;
        strcpy(path + dirPathLen, entry->name);

        int fileTests = 0;
        char *testNames[MAX];

        FILE *file = fopen(path, "r");
        assert(file);

        char *line = scratch;
        do {
            line = fgets(scratch, MAX, file);
            if (!line) continue;

            eatSpace(&line);
            if (strncmp(line, "#if", 3) != 0) continue;

            line += 3;
            eatSpace(&line);
            if (strncmp(line, "TEST", 4) != 0) continue;

            int depth = 1;
            do {
                line = fgets(line, MAX, file);
                if (!line) break;

                eatSpace(&line);

                if (strncmp(line, "#if", 3) == 0) {
                    depth++;
                    continue;
                } else if (strncmp(line, "#endif", 6) == 0) {
                    depth--;
                    continue;
                }

                if (strncmp(line, "void", 4) != 0) continue;

                line += 4;
                eatSpace(&line);
                if (strncmp(line, "test", 4) != 0) continue;

                char *start = line;
                while (*line != '(') {
                    line++;
                }
                char *name = (char*) malloc(line - start + 1);
                strncpy(name, start, line - start);
                name[line - start] = 0;
                testNames[fileTests++] = name;
                totalTests++;

            } while (depth > 0 && line != NULL);

        } while (line);

        fclose(file);
        if (fileTests < 1) continue;

        // Templating
        printf("    printf(\"%s:\\n\");\n", path);
        for (int i = 0; i < fileTests; i++) {
            printf("    _performTestCaseReportingResults(%s, \"%s\");\n", testNames[i], testNames[i]);
        }
        printf("    colorGray();\n");
        printf("    printf(\"%%3.1f%%%% success (%%d out of %d)\\n\\n\", (double) _fileTestsPassCount / %d.f * 100, _fileTestsPassCount);\n", fileTests, fileTests);
        printf("    colorReset();\n");
        printf("    _fileTestsPassCount = 0;\n");
        printf("\n");


    }
    DirectoryIterClose(&iter);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Expected filename(s) or directory(s) as input");
        exit(1);
    }

    printf("%s", prelude);

    for (int i = 1; i < argc; i++)
        discoverTests(argv[i]);

    printf("    // All tests finished\n");
    printf("\n");
    printf("    colorBold();\n");
    printf("    printf(\"%%3.1f%%%% success (%%d out of %d)\\n\", (double) _totalTestsPassCount / %d.f * 100, _totalTestsPassCount);\n", totalTests, totalTests);
    printf("    colorReset();\n");
    printf("\n");
    printf("    return !(_totalTestsPassCount == %d);\n", totalTests);
    printf("}\n");

    return 0;
}
