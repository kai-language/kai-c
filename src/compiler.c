
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
DynamicArray(Package *) packages;

Map libraryMap;
DynamicArray(Library *) libraries;

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

void addLibrary(Library *library) {
    Library *old = MapGet(&libraryMap, library->fullpath);
    if (old != library) {
        ASSERT(!old);
        MapSet(&libraryMap, library->fullpath, library);
        ArrayPush(libraries, library);
    }
}

Package *ImportPackage(const char *path, Package *importer) {

    char pathRelativeToImporter[MAX_PATH];
    if (importer && path[0] != '/') {
        strcpy(pathRelativeToImporter, importer->path);
        char *lastSlash = strrchr(pathRelativeToImporter, '/') + 1;
        strcpy(lastSlash, path);
    } else {
        strcpy(pathRelativeToImporter, path);
    }

    char resolvedPath[MAX_PATH];
    char *result = AbsolutePath(pathRelativeToImporter, resolvedPath);
    if (!result) {
        if (FlagVerbose) printf("Failed to resolve absolute path for %s\n", path);
        return NULL;
    }

    const char *fullpath = StrIntern(result);
    Package *package = MapGet(&packageMap, fullpath);
    if (!package) { // First time we have seen this package
        package = Calloc(DefaultAllocator, 1, sizeof *package);
        package->path = StrIntern(pathRelativeToImporter);
        package->fullpath = fullpath;
        package->scope = pushScope(package, builtinPackage.scope);
        if (FlagVerbose) printf("Importing %s\n", package->path);

        addPackage(package);
        QueuePushBack(&parsingQueue, package);
    }
    return package;
}

Library *LinkToPackage(const char *path, Package *linkTo) {
    // TODO: Decide on rules for search paths for libraries.
    // Current thoughts are that we resolve relevant to the file with the #link directive
    //  we then look in the root directory where a build.kai kind of file is then fall back to system locations
    // We should allow the compile time build program to provide their own search paths

    char pathRelativeToImporter[MAX_PATH];
    if (linkTo && path[0] != '/') {
        strcpy(pathRelativeToImporter, linkTo->path);
        char *lastSlash = strrchr(pathRelativeToImporter, '/') + 1;
        strcpy(lastSlash, path);
    } else {
        strcpy(pathRelativeToImporter, path);
    }

    char resolvedPath[MAX_PATH];
    char *result = AbsolutePath(pathRelativeToImporter, resolvedPath);
    if (!result) {
        if (FlagVerbose) printf("Failed to resolve absolute path for %s\n", path);
        return NULL;
    }

    const char *fullpath = StrIntern(result);
    Library *library = MapGet(&libraryMap, fullpath);
    if (!library) {
        library = Calloc(DefaultAllocator, 1, sizeof *library);
        library->path = StrIntern(pathRelativeToImporter);
        library->fullpath = fullpath;
        if (FlagVerbose) printf("Will link %s\n", library->path);

        addLibrary(library);
    }
    return library;
}
