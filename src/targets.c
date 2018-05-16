
typedef enum Os Os;
enum Os {
    Os_Current,
    Os_Linux,
    Os_Darwin,
    NUM_OSES,
};

const char *OsNames[NUM_OSES] = {
    [Os_Current] = "current",
    [Os_Linux] = "Linux",
    [Os_Darwin] = "Darwin",
};

typedef enum Arch Arch;
enum Arch {
    Arch_Current,
    Arch_x86_x64,
    NUM_ARCHES,
};

const char *ArchNames[NUM_ARCHES] = {
    [Arch_Current] = "current",
    [Arch_x86_x64] = "x86-x64",
};

Os OsForName(const char *name) {
    for (int i = 0; i < NUM_OSES; i++) {
        if (OsNames[i] != NULL && strcmp(OsNames[i], name) == 0) return i;
    }
    return -1;
}

Arch ArchForName(const char *name) {
    for (int i = 0; i < NUM_ARCHES; i++) {
        if (ArchNames[i] != NULL && strcmp(ArchNames[i], name) == 0) return i;
    }
    return -1;
}
