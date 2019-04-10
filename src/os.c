
typedef struct DirectoryIter DirectoryIter;
typedef struct SysInfo SysInfo;
struct SysInfo {
    const char *name;
    const char *arch;
};

char *path_absolute(char path[MAX_PATH]);
void DirectoryIterClose(DirectoryIter *it);
void DirectoryIterNext(DirectoryIter *it);
mode_t file_mode(const char *path);
void DirectoryIterOpen(DirectoryIter *it,const char *path);
SysInfo get_current_sysinfo(void);

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
char *ReadEntireFile(const char *path, u64 *len) {
    char *address = NULL;

#ifdef SYSTEM_POSIX
    i32 fd = open(path, O_RDONLY); // FIXME: No matching close
    if (fd == -1) return NULL;

    // MMAP with nul term
    // https://gist.github.com/pervognsen/1179630
    off_t pos = lseek(fd, 0, SEEK_CUR);
    off_t size = lseek(fd, 0, SEEK_END);
    lseek(fd, pos, SEEK_SET);
    if (size < 0) PANIC("Fseek returned -1");
    if (len) *len = size;

    char *ptr;
    int pagesize = getpagesize();
    if (size % pagesize != 0) {
        // Load npages + 1 so we get the os to null terminate the last page
        ptr = mmap(NULL, size + 1, PROT_READ, 0, fd, 0);
    } else {
        //
        size_t fullsize = size + pagesize;
        ptr = mmap(NULL, fullsize, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
        ptr = mmap(ptr, fullsize, PROT_READ, MAP_FIXED, fd, 0);
    }
    ASSERT(ptr[size] == 0);

    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    if (address == MAP_FAILED) return NULL;
    close(fd);
#else
    FILE *fd = fopen(path, "rb");
    if (!fd) return NULL;

    if (fseek(fd, 0, SEEK_END) == 0) {
        long size = ftell(fd);
        if (size == -1) return NULL;
        address = (char *)malloc(size+1);
        if (fseek(fd, 0, SEEK_SET) != 0) NULL;
        size_t read = fread(address, 1, size, fd);
        if (read == 0) return NULL;
        address[read] = '\0';
        if (len) *len = read;
    }

    fclose(fd);
#endif

    return address;
}

SysInfo CurrentSystem = {0};

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
