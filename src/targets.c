
#include "targets.h"

const char *OsNames[NUM_OSES] = {
    [Os_Current] = "current",
    [Os_Linux] = "Linux",
    [Os_Darwin] = "Darwin",
    [Os_Windows] = "Windows"
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


TargetPointerMetrics pointers_32bit = { .Width = 32, .Align = 32 };
TargetPointerMetrics pointers_64bit = { .Width = 64, .Align = 64 };

TargetPointerMetrics *Os_Linux_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = &pointers_64bit,
};

TargetPointerMetrics *Os_Darwin_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = &pointers_64bit,
};

TargetPointerMetrics *Os_Windows_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = &pointers_64bit,
    [Arch_x86] = &pointers_32bit,
};
