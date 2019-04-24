
#include "all.h"

char *path_absolute(char path[MAX_PATH]) {
    char rel_path[MAX_PATH];
    path_copy(rel_path, path);
    return realpath(rel_path, path);
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
        strncpy(it->name, entry->d_name, MAX_NAME - 1);
        it->name[MAX_NAME - 1] = '\0';
        // NOTE: d_type is a speed optimization to save on lstat(2) calls. It may not be supported by the filesystem.
        it->isDirectory = entry->d_type & DT_DIR;
    } while (it->valid && dir_iter_skip(it));
}

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
        strncpy(it->name, path, MAX_NAME);
        it->name[MAX_NAME - 1] = '\0';
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

// FIXME: We are mmap()'ing this with no way to munmap it currently
const char *ReadEntireFile(const char *path, u64 *len) {
    i32 fd = open(path, O_RDONLY); // FIXME: No matching close
    if (fd == -1) return NULL;
    // MMAP with nul term
    // https://gist.github.com/pervognsen/1179630
    off_t pos = lseek(fd, 0, SEEK_CUR);
    off_t size = lseek(fd, 0, SEEK_END);
    lseek(fd, pos, SEEK_SET);
    if (size < 0) fatal("Fseek returned -1");
    if (len) *len = size;

    char *ptr;
    int pagesize = getpagesize();
    if (size % pagesize != 0) {
        // Load npages + 1 so we get the os to null terminate the last page
        ptr = mmap(NULL, size + 1, PROT_READ, MAP_PRIVATE, fd, 0);
    } else {
        //
        size_t fullsize = size + pagesize;
        ptr = mmap(NULL, fullsize, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
        ptr = mmap(ptr, fullsize, PROT_READ, MAP_FIXED, fd, 0);
    }
    if (ptr == MAP_FAILED) perror("mmapping file failed");
    ASSERT(ptr[size] == 0);
    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    return ptr;
}

SysInfo get_current_sysinfo(void) {
    struct utsname *uts = xmalloc(sizeof(struct utsname));
    int res = uname(uts);
    if (res != 0) {
        perror("uname");
        exit(1);
    }
    SysInfo sysinfo = {uts->sysname, uts->machine};
    return sysinfo;
}

