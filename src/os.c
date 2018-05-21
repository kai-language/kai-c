
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

char *RemoveKaiExtension(char *filename) {
    char *dot = strrchr(filename, '.');
    if (!dot) return filename;

    if (strcmp(dot, ".kai") == 0) {
        *dot = '\0';
    }
    return filename;
}

// FIXME: We are mmap()'ing this with no way to munmap it currently
char *ReadEntireFile(const char *path) {
    char *address = NULL;

#ifdef SYSTEM_POSIX
    i32 fd = open(path, O_RDONLY);
    if (fd == -1) return NullWithLoggedReason("failed to open file %s", path);

    struct stat st;
    if (stat(path, &st) == -1) return NullWithLoggedReason("Failed to stat already opened file %s with file descriptor %d", path, fd);
    size_t len = st.st_size;

    address = (char *) mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    if (address == MAP_FAILED) return NullWithLoggedReason("Failed to mmap opened file %s", path);
#else
    FILE *fd = fopen(path, "rb");
    if (!fd) return (char *)NullWithLoggedReason("failed to  open file %s", path);

    if (fseek(fd, 0, SEEK_END) == 0) {
        long size = ftell(fd);
        if (size == -1) return (char *)NullWithLoggedReason("Failed to get file size");
        address = (char *)malloc(size+1);
        if (fseek(fd, 0, SEEK_SET) != 0) return (char *)NullWithLoggedReason("Failed to reset file cursor");
        size_t read = fread(address, 1, size, fd);
        if (read == 0) return (char *)NullWithLoggedReason("Failed to read file");
        address[++read] = NULL;
    }

    fclose(fd);
#endif

    return address;
}

