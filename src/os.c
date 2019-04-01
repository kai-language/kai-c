
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

typedef struct DirectoryIter {
    int valid;
    int error;

    char base[MAX_PATH];
    char name[MAX_PATH];
    bool isDirectory;

    void *handle;
} DirectoryIter;

void DirectoryIterClose(DirectoryIter *it);
void DirectoryIterNext(DirectoryIter *it);
void DirectoryIterOpen(DirectoryIter *it, const char *path);

bool DirectoryIterSkip(DirectoryIter *it) {
    return strcmp(it->name, ".") == 0 || strcmp(it->name, "..") == 0;
}

char *AbsolutePath(const char *filename, char *resolved) {
#ifdef SYSTEM_POSIX
    return realpath(filename, resolved);
#else
    DWORD len = GetFullPathName(filename, MAX_PATH, resolved, NULL);
    if (len == 0 || len > MAX_PATH)
        return NULL;

    return resolved;
#endif
}

// FIXME: We are mmap()'ing this with no way to munmap it currently
char *ReadEntireFile(const char *path) {
    char *address = NULL;

#ifdef SYSTEM_POSIX
    i32 fd = open(path, O_RDONLY); // FIXME: No matching close
    if (fd == -1) return NULL;

    struct stat st;
    if (stat(path, &st) == -1) return NULL;
    size_t len = st.st_size;

    address = (char *) mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    if (address == MAP_FAILED) return NULL;
    close(fd);
#else
    FILE *fd = fopen(path, "rb");
    if (!fd) NULL;

    if (fseek(fd, 0, SEEK_END) == 0) {
        long size = ftell(fd);
        if (size == -1) NULL;
        address = (char *)malloc(size+1);
        if (fseek(fd, 0, SEEK_SET) != 0) NULL;
        size_t read = fread(address, 1, size, fd);
        if (read == 0) NULL;
        address[read] = '\0';
    }

    fclose(fd);
#endif

    return address;
}

typedef struct SysInfo SysInfo;
struct SysInfo {
    const char *name;
    const char *arch;
};

SysInfo CurrentSystem = {0};

SysInfo get_current_sysinfo(void);

bool HaveInitializedDetailsForCurrentSystem = false;
void InitDetailsForCurrentSystem() {

}

#if SYSTEM_POSIX
#include "os_unix.c"
#elif SYSTEM_WINDOWS
#include "os_win32.c"
#else
#include "os_unknown.c"
#endif
