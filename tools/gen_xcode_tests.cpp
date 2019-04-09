
#define false 0
#define true !false

#define MAX 4096

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
#include "../src/main.c"
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

void eatSpace(char **stream) {
    while (isspace(**stream)) {
        (*stream)++;
    }
}

int totalTests = 0;

void discoverTests(const char *directoryPath) {

    char path[MAX];
    char *scratch = (char*)malloc(MAX);

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

        static char name[MAX];
        printf("\n");
        printf("//\n");
        printf("// Tests found in file %s\n", path);
        printf("@interface %s : XCTestCase\n@end\n\n", RemoveFileExtension(GetFileName(path, name, NULL)));
        printf("@implementation %s\n\n", RemoveFileExtension(GetFileName(path, name, NULL)));
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
    DirectoryIterClose(&iter);
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
