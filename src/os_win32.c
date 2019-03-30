
#include <io.h>
#include <errno.h>

void DirectoryIterClose(DirectoryIter *it) {
    if (it->valid) {
        it->valid = false;
        it->error = false;
        _findclose((intptr_t) it->handle);
    }
}

void _directoryIterUpdate(DirectoryIter *it, bool done, struct _finddata_t *fileinfo) {
    it->valid = !done;
    it->error = done && errno != ENOENT;
    if (!done) {
        strncpy(it->current.name, fileinfo->name, sizeof(it->current.name) - 1);
        it->current.name[MAX_PATH - 1] = 0;
        it->current.isDirectory = fileinfo->attrib & _A_SUBDIR;
        it->current.isFile = !it->current.isDirectory;
    }
}

DirectoryEntry *DirectoryIterNext(DirectoryIter *it) {
    if (!it->valid) return NULL;
    do {
        struct _finddata_t fileinfo;
        int result = _findnext((intptr_t) it->handle, &fileinfo);
        _directoryIterUpdate(it, result != 0, &fileinfo);
        if (result != 0) {
            DirectoryIterClose(it);
            return NULL;
        }
    } while (it->valid && DirectoryExcluded(iter));
}

DirectoryIter DirectoryOpen(const char *path) {
    DirectoryIter it = {0};
    struct _filedata_t fileinfo;
    intptr_t handle = _findfirst(path, &fileinfo);
    it.handle = (void *) handle;
    _directoryIterUpdate(&it, handle == -1, &fileinfo);
    if (DirectoryExcluded(&it)) {
        DirectoryIterNext(&it);
    }
}
