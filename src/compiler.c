
#include "compiler.h"

void InitTarget(void);
void InitBuiltins(void);

Package builtinPackage;

void InitCompiler() {
    InitUnsetFlagsToDefaults();
    InitKeywords();
    InitBuiltins();
}

Map packageMap;
DynamicArray(Package*) packages;

Queue parsingQueue;
Queue checkingQueue;

Scope *pushScope(Package *pkg, Scope *parent);

void addPackage(Package *package) {
    Package *old = MapGet(&packageMap, package->fullpath);
    if (old != package) {
        ASSERT(!old);
        MapSet(&packageMap, package->fullpath, package);
        ArrayPush(packages, package);
    }
}

Package *ImportPackage(const char *path, Package *importer) {

    char pathRelativeToImporter[MAX_PATH];
    if (importer) {
        strcpy(pathRelativeToImporter, importer->path);
        char *lastSlash = strrchr(pathRelativeToImporter, '/') + 1;
        strcpy(lastSlash, path);
    } else {
        strcpy(pathRelativeToImporter, path);
    }

    char resolvedPath[MAX_PATH];
    char *result = AbsolutePath(pathRelativeToImporter, resolvedPath);
    if (!result) {
        if (FlagVerbose) printf("Failed to resolve absolure path for %s\n", path);
        return NULL;
    }

    const char *fullpath = StrIntern(resolvedPath);
    Package *package = MapGet(&packageMap, fullpath);
    if (!package) { // First time we have seen this package
        package = Calloc(DefaultAllocator, 1, sizeof(Package));
        package->path = StrIntern(pathRelativeToImporter);
        package->fullpath = fullpath;
        package->scope = pushScope(package, builtinPackage.scope);
        if (FlagVerbose) printf("Importing %s\n", package->path);

        addPackage(package);
        QueuePushBack(&parsingQueue, package);
    }
    return package;
}
