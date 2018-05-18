
char *AbsolutePath(const char *filename, char *resolved) {
    return realpath(filename, resolved);
}

char *RemoveKaiExtension(char *filename) {
    char *dot = rindex(filename, '.');
    if (!dot) return filename;

    if (strcmp(dot, ".kai") == 0) {
        *dot = '\0';
    }
    return filename;
}

// FIXME: We are mmap()'ing this with no way to munmap it currently
char *ReadFile(const char *path) {
    i32 fd = open(path, O_RDONLY);
    if (fd == -1) return NullWithLoggedReason("failed to open file %s", path);

    struct stat st;
    if (stat(path, &st) == -1) return NullWithLoggedReason("Failed to stat already opened file %s with file descriptor %d", path, fd);
    size_t len = st.st_size;

    char *address = (char*) mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    if (address == MAP_FAILED) return NullWithLoggedReason("Failed to mmap opened file %s", path);

    return address;
}