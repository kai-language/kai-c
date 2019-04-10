
typedef struct SysInfo SysInfo;
struct SysInfo {
    const char *name;
    const char *arch;
};

typedef struct DirectoryIter DirectoryIter;
struct DirectoryIter {
    int valid;
    int error;

    char base[MAX_PATH];
    char name[MAX_PATH];
    bool isDirectory;

    void *handle;
};

char *path_absolute(char path[MAX_PATH]);
mode_t file_mode(const char *path);

void DirectoryIterClose(DirectoryIter *it);
void DirectoryIterNext(DirectoryIter *it);
void DirectoryIterOpen(DirectoryIter *it, const char *path);

SysInfo get_current_sysinfo(void);
const char *ReadEntireFile(const char *path, u64 *len);

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

void path_join(char path[MAX_PATH], const char *src) {
    if (*path == '\0') { // If the lhs is an empty string don't prepend the '/'
        path_copy(path, src);
        return;
    }
    char *ptr = path + strlen(path);
    if (ptr != path && ptr[-1] == '/') {
        ptr--;
    }
    if (*src == '/') {
        src++;
    }
    snprintf(ptr, path + MAX_PATH - ptr, "/%s", src);
}

char *path_file(char path[MAX_PATH]) {
    path_normalize(path);
    for (char *ptr = path + strlen(path); ptr != path; ptr--) {
        if (ptr[-1] == '/') {
            return ptr;
        }
    }
    return path;
}

char *path_ext(char path[MAX_PATH]) {
    for (char *ptr = path + strlen(path); ptr != path; ptr--) {
        if (ptr[-1] == '.') {
            return ptr;
        }
    }
    return path;
}

bool DirectoryIterSkip(DirectoryIter *it) {
    return strcmp(it->name, ".") == 0 || strcmp(it->name, "..") == 0;
}

SysInfo CurrentSystem = {0};

bool HaveInitializedDetailsForCurrentSystem = false;
void InitDetailsForCurrentSystem() {

}

#if defined(_POSIX_VERSION)
#include "os_unix.c"
#elif defined(_WIN32) || defined(_WIN64)
#include "os_win32.c"
#else
#include "os_unknown.c"
#endif

