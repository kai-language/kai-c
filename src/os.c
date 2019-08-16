
#include "all.h"
#include "os.h"

#if INTERFACE
struct SysInfo {
    const char *name;
    const char *arch;
};

struct DirectoryIter {
    int valid;
    int error;

    char base[MAX_PATH];
    char name[MAX_PATH];
    bool isDirectory;

    void *handle;
};

FileMode file_mode(const char *path);
char *path_absolute(char path[MAX_PATH]);
void dir_iter_open(DirectoryIter *it, const char *path);
void dir_iter_next(DirectoryIter *it);
void dir_iter_close(DirectoryIter *it);
const char *ReadEntireFile(const char *path, u64 *len);
SysInfo get_current_sysinfo(void);
#endif

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

void path_append(char path[MAX_PATH], const char *src) {
    if (*path == '\0') {
        path_copy(path, src);
        return;
    }
    char *end = path + strlen(path);
    snprintf(end, path + MAX_PATH - end, "%s", src);
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

// returns path if no '.' occurs
const char *path_ext(const char path[MAX_PATH]) {
    for (const char *ptr = path + strlen(path); ptr != path; ptr--) {
        if (ptr[-1] == '.') {
            return ptr;
        }
    }
    return path;
}

bool dir_iter_skip(DirectoryIter *it) {
    return strcmp(it->name, ".") == 0 || strcmp(it->name, "..") == 0;
}

SysInfo CurrentSystem = {0};

bool HaveInitializedDetailsForCurrentSystem = false;
void InitDetailsForCurrentSystem(void) {

}

void *xcalloc(size_t num_bytes) {
    void *ptr = calloc(num_bytes, 1);
    if (!ptr) {
        perror("calloc failed");
        exit(1);
    }
    return ptr;
}

void *xrealloc(void *ptr, size_t num_bytes) {
    ptr = realloc(ptr, num_bytes);
    if (!ptr) {
        perror("realloc failed");
        exit(1);
    }
    return ptr;
}

void *xmalloc(size_t num_bytes) {
    void *ptr = malloc(num_bytes);
    if (!ptr) {
        perror("malloc failed");
        exit(1);
    }
    return ptr;
}

#if defined(__unix__)
#include "os/os_unix.c"
#elif defined(_WIN32) || defined(_WIN64)
#include "os/os_win32.c"
#else
#warning "Unsupported platform"
#endif

