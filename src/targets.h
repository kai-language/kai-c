
typedef enum Os {
    Os_Unknown,
    Os_Linux,
    Os_Darwin,
    Os_Windows,

    NUM_OSES,
} Os;

typedef enum Arch {
    Arch_Unknown,
    Arch_x86_64,
    Arch_x86,
    Arch_arm,
    Arch_arm64,
    NUM_ARCHES,
} Arch;

typedef struct TargetMetrics {
    u32 Width;
    u32 Align;
} TargetMetrics;
