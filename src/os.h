#pragma once

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
    char name[MAX_NAME];
    bool isDirectory;

    void *handle;
};

void *xmalloc(size_t num_bytes);
void *xcalloc(size_t num_bytes);
void *xrealloc(void *ptr, size_t num_bytes);

void InitDetailsForCurrentSystem(void);
SysInfo get_current_sysinfo(void);
const char *ReadEntireFile(const char *path, u64 *len);
const char *path_ext(const char path[MAX_PATH]);
char *path_file(char path[MAX_PATH]);
void path_join(char path[MAX_PATH], const char *src);
void path_copy(char path[MAX_PATH], const char *src);
void path_normalize(char *path);
char *path_absolute(char path[MAX_PATH]);
void dir_iter_close(DirectoryIter *it);
void dir_iter_next(DirectoryIter *it);
void dir_iter_open(DirectoryIter *it, const char *path);
bool dir_iter_skip(DirectoryIter *it);
mode_t file_mode(const char *path);
