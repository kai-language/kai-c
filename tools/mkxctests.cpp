
#define false 0
#define true !false

#define MAX_PATH 4096

//
//
// Prelude
//
char prelude[] =
R"(
#import <XCTest/XCTest.h>

// NOTE: The XCTest Assertions expect to have `self` defined, so we do that for them and set self for each test case
XCTestCase *current_test_case;

#define ASSERT_MSG_VA(cond, ...) _XCTPrimitiveAssertTrue(current_test_case, cond, @#cond, __VA_ARGS__)
#define ASSERT_MSG(cond, msg) ASSERT_MSG_VA((cond), "(" #cond ") " msg)
#define ASSERT(cond) ASSERT_MSG_VA((cond))
#define PANIC(msg) ASSERT_MSG_VA(0, msg)
#define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented");

#pragma clang diagnostic ignored "-Wmacro-redefined" // Allow redefinition of helper macros

#define PROFILING_ENABLED 0
#include "unity.c"
)";

// End of Prelude

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <assert.h>
#include <libgen.h>
#include <string.h>
#include <sys/stat.h>

char *GetFileName(const char *path, char *res, char **dir) {
    size_t len = strlen(path);
    memcpy(res, path, len);
    res[len] = '\0';
    char *index = strrchr(res, '/');

    if (dir) {
        *index = '\0';
        *dir = res;
    }

    return index ? index+1 : res;
}

char *RemoveFileExtension(char *filename) {
    char *dot = strrchr(filename, '.');
    if (!dot) return filename;
    *dot = '\0';
    return filename;
}

void path_normalize(char *path) {
    char *ptr;
    for (ptr = path; *ptr; ptr++) {
        if (*ptr == '\\') {
            *ptr = '/';
        }
    }
    if (ptr != path && ptr[-1] == '/') {
        ptr[-1] = 0;
    }
}

void path_copy(char path[MAX_PATH], const char *src) {
    strncpy(path, src, MAX_PATH);
    path[MAX_PATH - 1] = 0;
    path_normalize(path);
}

typedef struct DirectoryIter DirectoryIter;
struct DirectoryIter {
    int valid;
    int error;

    char base[MAX_PATH];
    char name[MAX_PATH];
    bool isDirectory;

    void *handle;
};

bool dir_iter_skip(DirectoryIter *it) {
    char *ext = strrchr(it->name, '.');
    return strcmp(it->name, ".") == 0 || strcmp(it->name, "..") == 0 ||
        (ext && strcmp(ext, ".h") == 0);
}

void dir_iter_close(DirectoryIter *it) {
    if (it->valid && it->handle) {
        it->valid = false;
        it->error = false;
        closedir((DIR *)it->handle);
    }
}

void dir_iter_next(DirectoryIter *it) {
    if (!it->valid) return;
    if (!it->handle) { // Single file
        it->valid = false;
        it->error = false;
        return;
    }

    do {
        struct dirent *entry = readdir((DIR *) it->handle);
        if (!entry) {
            dir_iter_close(it);
            return;
        }
        strncpy(it->name, entry->d_name, MAX_PATH - 1);
        it->name[MAX_PATH - 1] = '\0';
        // NOTE: d_type is a speed optimization to save on lstat(2) calls. It may not be supported by the filesystem.
        it->isDirectory = entry->d_type & DT_DIR;
    } while (it->valid && dir_iter_skip(it));
}

static
mode_t file_mode(const char *path) {
    struct stat path_stat;
    stat(path, &path_stat);
    return path_stat.st_mode;
}

void dir_iter_open(DirectoryIter *it, const char *path) {
    memset(it, 0, sizeof *it);
    it->valid = true;
    mode_t mode = file_mode(path);
    if (S_ISREG(mode)) {
        path_copy(it->name, path);
        return;
    } else {
        DIR *dir = opendir(path);
        if (!dir) {
            it->valid = false;
            it->error = true;
            return;
        }
        it->handle = dir;
    }
    path_copy(it->base, path);
    dir_iter_next(it);
}

void eatSpace(char **stream) {
    while (isspace(**stream)) {
        (*stream)++;
    }
}

int totalTests = 0;

void discoverTests(const char *directoryPath) {

    char path[MAX_PATH];
    char *scratch = (char*)malloc(MAX_PATH);

    size_t dirPathLen = strlen(directoryPath);
    strncpy(path, directoryPath, dirPathLen);
    if (path[dirPathLen - 1] != '/') {
        path[dirPathLen++] = '/';
    }
    path[dirPathLen] = '\0';

    DirectoryIter iter;
    for (dir_iter_open(&iter, directoryPath); iter.valid; dir_iter_next(&iter)) {
        if (iter.isDirectory) continue;
        strcpy(path + dirPathLen, iter.name);

        int fileTests = 0;
        char *testNames[MAX_PATH];

        FILE *file = fopen(path, "r");
        assert(file);

        char *line = scratch;
        do {
            line = fgets(scratch, MAX_PATH, file);
            if (!line) continue;

            eatSpace(&line);
            if (strncmp(line, "#if", 3) != 0) continue;

            line += 3;
            eatSpace(&line);
            if (strncmp(line, "TEST", 4) != 0) continue;

            int depth = 1;
            do {
                line = fgets(line, MAX_PATH, file);
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

        static char name[MAX_PATH];
        printf("\n");
        printf("//\n");
        printf("// Tests found in file %s\n", path);
        printf("@interface %s_tests : XCTestCase\n@end\n\n", RemoveFileExtension(GetFileName(path, name, NULL)));
        printf("@implementation %s_tests\n\n", RemoveFileExtension(GetFileName(path, name, NULL)));
        printf("- (void) setUp {\n");
        printf("    current_test_case = self;\n");
        printf("    [self setContinueAfterFailure: false];\n");
        printf("}\n");
        printf("\n");
        for (int i = 0; i < fileTests; i++) {
            printf("- (void)%s {\n", testNames[i]);
            printf("    %s();\n", testNames[i]);
            printf("}\n\n");
        }

        printf("@end\n");
    }
    dir_iter_close(&iter);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Expected filename or directory as input");
        exit(1);
    }

    printf("%s", prelude);

    for (int i = 1; i < argc; i++)
        discoverTests(argv[i]);

    return 0;
}
