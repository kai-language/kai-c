
#include "targets.h"


const char *OsNames[NUM_OSES] = {
    [Os_Unknown] = "unknown",
    [Os_Linux] = "Linux",
    [Os_Darwin] = "Darwin",
    [Os_Windows] = "Windows"
};

const char *ArchNames[NUM_ARCHES] = {
    [Arch_Unknown] = "unknown",
    [Arch_x86_64]  = "x86_64",
    [Arch_x86]     = "x86",
    [Arch_arm]     = "arm",
    [Arch_arm64]   = "arm64",
};

Os OsForName(const char *name) {
    for (int i = 0; i < NUM_OSES; i++) {
        if (OsNames[i] != NULL && strcmp(OsNames[i], name) == 0)
            return (Os) i;
    }
    return Os_Unknown;
}

Arch ArchForName(const char *name) {
    for (int i = 0; i < NUM_ARCHES; i++) {
        if (ArchNames[i] != NULL && strcmp(ArchNames[i], name) == 0)
            return (Arch) i;
    }
    return Arch_Unknown;
}

// Type details

TargetMetrics Os_Linux_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = { .Width = 64, .Align = 64 },
};

TargetMetrics Os_Darwin_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = { .Width = 64, .Align = 64 },
};

TargetMetrics Os_Windows_ArchSupport[NUM_ARCHES] = {
    [Arch_x86_64] = { .Width = 64, .Align = 64 },
    [Arch_x86]    = { .Width = 32, .Align = 32 },
};
