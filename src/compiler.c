
#include "compiler.h"

void InitTarget(void);
void InitBuiltins(void);

Package builtinPackage;

Map packageMap;
DynamicArray(Package*) packages;

Queue parsingQueue;
Queue checkingQueue;

Scope *pushScope(Package *pkg, Scope *parent);


enum { MAX_GLOBAL_SEARCH_PATHS = 256 };
const char *static_global_search_paths[MAX_GLOBAL_SEARCH_PATHS];
const char **global_search_paths = static_global_search_paths;
int num_global_search_paths;

bool is_package_path(const char *package_path, const char *search_path) {
    char path[MAX_PATH];
    path_copy(path, search_path);
    path_join(path, package_path);
    DirectoryIter it;
    for (DirectoryIterOpen(&it, path); it.valid; DirectoryIterNext(&it)) {
        const char *ext = path_ext(it.name);
        if (ext != it.name && strcmp(ext, "kai") == 0) {
            DirectoryIterClose(&it);
            return true;
        }
    }
    return false;
}

bool resolve_package_path(char dest[MAX_PATH], const char *path, Package *importer) {
    if (importer) {
        if (is_package_path(path, importer->searchPath)) {
            path_copy(dest, importer->searchPath);
            path_join(dest, path);
            return true;
        }
    }

    // Check the global package search path
    for (int i = 0; i < num_global_search_paths; i++) {
        if (is_package_path(path, global_search_paths[i])) {
            path_copy(dest, global_search_paths[i]);
            path_join(dest, path);
            return true;
        }
    }

    return false;
}

void package_search_path(char dest[MAX_PATH], const char *path) {
    path_copy(dest, path);
    const char *ext = path_ext(dest);
    if (strcmp(ext, "kai") == 0) {
        char *lastSlash = strrchr(dest, '/');
        if (lastSlash) *lastSlash = '\0';
    }
}

void add_global_search_path(const char *path) {
    if (FlagVerbose) printf("Adding global search path %s\n", path);
    global_search_paths[num_global_search_paths++] = StrIntern(path);
}

void InitGlobalSearchPaths() {
    add_global_search_path(".");
    add_global_search_path("vendor");
    add_global_search_path("packages");

    const char *kaipath_env = getenv("KAIPATH");
    if (kaipath_env) {
        char path[MAX_PATH];
        path_copy(path, kaipath_env);
        path_join(path, "packages");
        add_global_search_path(path);
    } else if (FlagVerbose) {
        printf("No KAIPATH environment variable set\n");
    }
}

Package *ImportPackage(const char *path, Package *importer) {

    char package_path[MAX_PATH];
    if (!resolve_package_path(package_path, path, importer)) {
        if (FlagVerbose) {
            printf("Failed to resolve package path for %s\n", path);
        }
        return NULL;
    }

    char absolutePath[MAX_PATH];
    char *result = AbsolutePath(package_path, absolutePath);
    if (!result) {
        if (FlagVerbose) printf("Failed to resolve absolute path for %s\n", path);
        return NULL;
    }

    const char *fullpath = StrIntern(absolutePath);
    Package *package = MapGet(&packageMap, fullpath);
    if (!package) { // First time we have seen this package
        package = Calloc(DefaultAllocator, 1, sizeof(Package));
        package->path = StrIntern(package_path);
        package->fullpath = fullpath;
        package->scope = pushScope(package, builtinPackage.scope);

        package->searchPath = StrIntern(package_path);
        char searchPath[MAX_PATH];
        package_search_path(searchPath, package_path);
        package->searchPath = StrIntern(searchPath);
        if (FlagVerbose) {
            printf("Importing %s\n", package->path);
            printf("  Search Path for package is %s\n", package->searchPath);
        }

        // Add the package to the package map.
        MapSet(&packageMap, package->fullpath, package);
        ArrayPush(packages, package);

        DynamicArray(SourceFile) files = NULL;

        // Discover all files
        DirectoryIter it;
        for (DirectoryIterOpen(&it, fullpath); it.valid; DirectoryIterNext(&it)) {
            SourceFile file = {0};
            char name[MAX_PATH];
            path_copy(name, it.name);
            char *ext = path_ext(name);
            if (ext == name || strcmp(ext, "kai") != 0) {
                continue;
            }

            char path[MAX_PATH];
            path_copy(path, it.base);
            path_join(path, it.name);
            file.path = StrIntern(path);

            path_absolute(path);
            file.fullpath = StrIntern(path);

            file.package = package;
            file.parsed = false;
            ArrayPush(files, file);
        }

        package->numFiles = ArrayLen(files);
        package->files = ArenaAlloc(&package->arena, sizeof *files * package->numFiles);
        memcpy(package->files, files, sizeof *files * package->numFiles);

        ArrayFree(files);

        // Queue all files
        for (u64 i = 0; i < package->numFiles; i++) {
            QueuePushBack(&parsingQueue, &package->files[i]);
        }
    }
    return package;
}

