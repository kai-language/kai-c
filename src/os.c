
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

typedef struct FileData {
    const char *path;
    const char *code;
    size_t len;
} FileData;

FileData *files;

// FIXME: We are mmap()'ing this with no way to munmap it currently
char *ReadEntireFile(const char *path) {
    char *address = NULL;

#ifdef SYSTEM_POSIX
    i32 fd = open(path, O_RDONLY); // FIXME: No matching close
    if (fd == -1) return NullWithLoggedReason("failed to open file %s", path);

    struct stat st;
    if (stat(path, &st) == -1) return NullWithLoggedReason("Failed to stat already opened file %s with file descriptor %d", path, fd);
    size_t len = st.st_size;

    address = (char *) mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (close(fd) == -1) perror("close was interupted"); // intentionally continue despite the failure, just keep the file open
    if (address == MAP_FAILED) return NullWithLoggedReason("Failed to mmap opened file %s", path);
    close(fd);
    FileData data = {path, address, len};
    ArrayPush(files, data);
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
        address[read] = '\0';
    }

    fclose(fd);

    FileData data = {path, address, len};
    ArrayPush(files, data);
#endif

    return address;
}

typedef struct SysInfo SysInfo;
struct SysInfo {
    const char *name;
    const char *machine;
};

SysInfo CurrentSystem = {0};

bool HaveInitializedDetailsForCurrentSystem = false;
void InitDetailsForCurrentSystem() {
    if (HaveInitializedDetailsForCurrentSystem) return;
#if SYSTEM_POSIX
    struct utsname *sysinfo = malloc(sizeof(struct utsname));
    int res = uname(sysinfo);
    if (res != 0) {
        perror("uname");
        exit(1);
    }
    CurrentSystem.name = sysinfo->sysname;
    CurrentSystem.machine = sysinfo->machine;
#elif SYSTEM_WINDOWS
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
    CurrentSystem.name = OsNames[Os_Windows];
	switch (sysinfo.wProcessorArchitecture) {
        case PROCESSOR_ARCHITECTURE_INTEL: CurrentSystem.machine = ArchNames[Arch_x86];
        case PROCESSOR_ARCHITECTURE_AMD64: CurrentSystem.machine = ArchNames[Arch_x86_64];
        case PROCESSOR_ARCHITECTURE_ARM:   CurrentSystem.machine = ArchNames[Arch_arm];
        case 12:						   CurrentSystem.machine = ArchNames[Arch_arm64]; // MSDN has 12 as PROCESSOR_ARCHITECTURE_ARM64 but it isn't in our headers
        default:                           CurrentSystem.machine = ArchNames[Arch_Current]; // should we have an Arch_Unknown?
	}
#else
    CurrentSystem.name = "unknown";
    CurrentSystem.machine = "unknown";
#endif

    HaveInitializedDetailsForCurrentSystem = true;
}

