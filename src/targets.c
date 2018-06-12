
typedef enum Os Os;
enum Os {
    Os_Current,
    Os_Linux,
    Os_Darwin,
    Os_Windows,
    NUM_OSES,
};

const char *OsNames[NUM_OSES] = {
    [Os_Current] = "current",
    [Os_Linux] = "Linux",
    [Os_Darwin] = "Darwin",
    [Os_Windows] = "Windows"
};

typedef enum Arch Arch;
enum Arch {
    Arch_Current,
    Arch_x86_64,
    Arch_x86,
    Arch_arm,
    Arch_arm64,
    NUM_ARCHES,
};

const char *ArchNames[NUM_ARCHES] = {
    [Arch_Current] = "current",
    [Arch_x86_64]  = "x86_64",
    [Arch_x86]     = "x86",
    [Arch_arm]     = "arm",
    [Arch_arm64]   = "arm64",
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


// Type details

typedef struct TargetMetrics {
    u32 Width;
    u32 Align;
} TargetMetrics;

enum {
    TargetMetrics_Pointer,
};

TargetMetrics metrics_32bit[2] = {
    [TargetMetrics_Pointer] = { .Width = 32, .Align = 32 },
};

TargetMetrics metrics_64bit[2] = {
    [TargetMetrics_Pointer] = { .Width = 64, .Align = 64 },
};

TargetMetrics *Os_Linux_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = metrics_64bit,
};

TargetMetrics *Os_Darwin_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = metrics_64bit,
};

TargetMetrics *Os_Windows_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = metrics_64bit,
    [Arch_x86] = metrics_32bit,
};
