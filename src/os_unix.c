
#include <dirent.h>
#include <stdlib.h>

void path_absolute(char path[MAX_PATH]) {
    char rel_path[MAX_PATH];
    path_copy(rel_path, path);
    realpath(rel_path, path);
}

void DirectoryIterClose(DirectoryIter *it) {
    if (it->valid && it->handle) {
        it->valid = false;
        it->error = false;
        closedir((DIR *)it->handle);
    }
}

void DirectoryIterNext(DirectoryIter *it) {
    if (!it->valid) return;
    if (!it->handle) { // Single file
        it->valid = false;
        it->error = false;
        return;
    }

    do {
        struct dirent *entry = readdir((DIR *) it->handle);
        if (!entry) {
            DirectoryIterClose(it);
            return;
        }
        strncpy(it->name, entry->d_name, MAX_PATH - 1);
        it->name[MAX_PATH - 1] = '\0';
        // NOTE: d_type is a speed optimization to save on lstat(2) calls. It may not be supported by the filesystem.
        it->isDirectory = entry->d_type & DT_DIR;
    } while (it->valid && DirectoryIterSkip(it));
}

mode_t file_mode(const char *path) {
    struct stat path_stat;
    stat(path, &path_stat);
    return path_stat.st_mode;
}

void DirectoryIterOpen(DirectoryIter *it, const char *path) {
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
    DirectoryIterNext(it);
}
