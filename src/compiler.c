#include "compiler.h"

void InitBuiltinTypes();

void InitCompiler() {
    InitKeywords();
    InitBuiltinTypes();
    InitDetailsForCurrentSystem();
    InitUnsetFlagsToDefaults();
}

Map packageMap;
DynamicArray(Package*) packages;

Queue parsingQueue;
Queue checkingQueue;

void addPackage(Package *package) {
    Package *old = MapGet(&packageMap, package->path);
    if (old != package) {
        ASSERT(!old);
        MapSet(&packageMap, package->path, package);
        ArrayPush(packages, package);
    }
}

Package *ImportPackage(const char *path) {
    path = StrIntern(path);
    Package *package = MapGet(&packageMap, path);
    if (!package) { // First time we have seen this package
        package = Calloc(DefaultAllocator, 1, sizeof(Package));
        package->path = path;
        if (FlagVerbose) printf("Importing %s\n", path);

        char fullPath[MAX_PATH];
        if (!AbsolutePath(path, fullPath)) return NullWithLoggedReason("Failed to resolve absolute path for %s", path); // Unable to resolve file path

        strcpy(package->fullPath, fullPath);
        addPackage(package);
        QueueEnqueue(&parsingQueue, package);
    }
    return package;
}
