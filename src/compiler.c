
#include "compiler.h"
#include "flags.h"

Compiler compiler;

Scope *pushScope(Package *pkg, Scope *parent);

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
    DirectoryIterClose(&it);
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
    for (int i = 0; i < compiler.num_global_search_paths; i++) {
        if (is_package_path(path, compiler.global_search_paths[i])) {
            path_copy(dest, compiler.global_search_paths[i]);
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

void add_global_search_path(Compiler *compiler, const char *path) {
    if (compiler->flags.verbose) printf("Adding global search path %s\n", path);
    compiler->global_search_paths[compiler->num_global_search_paths++] = StrIntern(path);
}

void InitGlobalSearchPaths(Compiler *compiler) {
    add_global_search_path(compiler, ".");
    add_global_search_path(compiler, "vendor");
    add_global_search_path(compiler, "packages");
    const char *kaipath_env = getenv("KAIPATH");
    if (kaipath_env) {
        char path[MAX_PATH];
        path_copy(path, kaipath_env);
        path_join(path, "packages");
        add_global_search_path(compiler, path);
    } else if (compiler->flags.verbose) {
        printf("No KAIPATH environment variable set\n");
    }
}

void read_package_source_files(Package *pkg) {
    DirectoryIter iter;
    for (DirectoryIterOpen(&iter, pkg->fullpath); iter.valid; DirectoryIterNext(&iter)) {
        if (iter.isDirectory || iter.name[0] == '.') continue;
        char name[MAX_PATH];
        path_copy(name, iter.name);
        char *ext = path_ext(name);
        if (ext == name || strcmp(ext, "kai") != 0) continue;
        ext[-1] = '\0';
        Source *source = ArenaAlloc(&pkg->arena, sizeof *source);
        path_copy(source->name, name);

        path_copy(source->path, iter.base);
        path_join(source->path, iter.name);
        path_absolute(source->path);

        source->code = ReadEntireFile(source->path);
        if (!source->code) {
            ReportError(pkg, FatalError, (SourceRange){ source->path }, "Failed to read source file");
            return;
        }
        source_memory_usage += strlen(source->code);
        parseAllStmts(pkg, source->code);
    }
    DirectoryIterClose(&iter);
}

Package *import_package(const char *path, Package *importer) {
    char package_path[MAX_PATH];
    path_copy(package_path, path);
    if (!resolve_package_path(package_path, path, importer)) {
        if (compiler.flags.verbose) printf("Failed to resolve package path for %s\n", path);
        return NULL;
    }
    if (!path_absolute(package_path)) {
        if (compiler.flags.verbose) printf("Failed to resolve absolute path for %s\n", path);
        return NULL;
    }
    const char *fullpath = StrIntern(package_path);
    Package *package = MapGet(&compiler.package_map, fullpath);
    if (!package) { // first time seeing this package
        package = Calloc(DefaultAllocator, 1, sizeof *package);
        package->path = path;
        package->fullpath = fullpath;
        package->scope = pushScope(package, compiler.builtin_package.scope);

        char search_path[MAX_PATH];
        package_search_path(search_path, fullpath);
        package->searchPath = StrIntern(search_path);
        if (compiler.flags.verbose) printf("Importing %s\n", package->path);

        MapSet(&compiler.package_map, fullpath, package);
        ArrayPush(compiler.packages, package);

        QueuePushBack(&compiler.parsing_queue, package);
    }
    return package;
}

void ConfigureDefaults(Compiler *compiler) {
    if (!strlen(compiler->output_name)) {
        char path[MAX_PATH];
        path_copy(path, compiler->input_name);
        char *file = path_file(path);
        file = RemoveKaiExtension(file);
        path_copy(compiler->output_name, file);
    }

    SysInfo info = get_current_sysinfo();
    if (compiler->target_os == Os_Unknown) {
        compiler->target_os = OsForName(info.name);
    }
    if (compiler->target_arch == Arch_Unknown) {
        compiler->target_arch = ArchForName(info.arch);
    }

    if (compiler->target_os == Os_Unknown || compiler->target_arch == Arch_Unknown) {
        printf("Unsupported Os or Arch: %s %s\n",
               OsNames[compiler->target_os], ArchNames[compiler->target_arch]);
        exit(1);
    }

    switch (compiler->target_os) {
        case Os_Linux:
            compiler->target_metrics = Os_Linux_ArchSupport[compiler->target_arch];
            break;
        case Os_Darwin:
            compiler->target_metrics = Os_Darwin_ArchSupport[compiler->target_arch];
            break;
        case Os_Windows:
            compiler->target_metrics = Os_Windows_ArchSupport[compiler->target_arch];
            break;

        default:
            break;
    }
}

void outputVersionAndBuildInfo() {
    printf("%s\n\n", VERSION);

    bool debug = false;

#if DEBUG
    debug = true;
#endif

    const char *y = "✔";
    const char *n = "✘";

    printf("-DDEBUG %s\n", debug ? y : n);
}

void InitCompiler(Compiler *compiler, int argc, const char **argv) {
    const char *prog_name = argv[0];
    ParseFlags(compiler, &argc, &argv);
    if (compiler->flags.version) {
        outputVersionAndBuildInfo();
        exit(0);
    }
    if (argc != 1 || compiler->flags.help) {
        printf("Usage: %s [flags] <input>\n", prog_name);
        PrintUsage(argv[0]);
        exit(1);
    }
    ConfigureDefaults(compiler);
    InitBuiltinTypes(compiler);
    InitGlobalSearchPaths(compiler);
    InitKeywords();
}

bool Compile(Compiler *compiler) {
    Package *builtins = import_package("builtin", NULL);
    if (!builtins) printf("warning: Failed to compile builtin package\n");

    Package *mainPackage = import_package(compiler->input_name, NULL);
    if (!mainPackage) {
        printf("error: Failed to compile '%s'\n", compiler->input_name);
        exit(1);
    }

    for (;;) {
        Package *pkg = QueuePopFront(&compiler->parsing_queue);
        if (pkg) {
            parse_package(pkg);
            continue;
        }

        CheckerWork *work = QueuePopFront(&compiler->checking_queue);
        if (work) {
            if (compiler->flags.verbose) printf("Checking package %s\n", work->package->path);
            CheckerContext ctx = { .scope = work->package->scope };
            checkStmt(work->stmt, &ctx, work->package);
            if (ctx.mode == ExprMode_Unresolved) {
                QueuePushBack(&compiler->checking_queue, work);
            }
            continue;
        }

        break;
    }

    b32 sawErrors = false;
    size_t numPackages = ArrayLen(compiler->packages);
    for (size_t i = 0; i < numPackages; i++) {
        if (HasErrors(compiler->packages[i])) {
            OutputReportedErrors(compiler->packages[i]);
            sawErrors = true;
        }
    }
    if (sawErrors) return false;

#if !TEST
    CodegenLLVM(mainPackage);
#endif

    if (compiler->flags.verbose)
        printf("Source memory usage = %.2f kB\n", (float) source_memory_usage / 1024.f);

    if (compiler->target_output != OutputType_Exec || compiler->flags.emitHeader)
        CodegenCHeader(mainPackage);

    return true;
}

#if TEST
void InitTestCompiler(Compiler *compiler, const char *flags) {
    *compiler = (Compiler){0};
    char *str = "";
    if (flags) {
        str = malloc(strlen(flags) + 1);
        strcpy(str, flags);
    }

    DynamicArray(const char *) args = NULL;
    ArrayPush(args, "kai");

    char *arg = strtok(str, " ");
    while(arg) {
        ArrayPush(args, arg);
        arg = strtok(NULL, " ");
    }

    ArrayPush(args, "test.kai");

    int argc = (int) ArrayLen(args);
    InitCompiler(compiler, argc, args);
}
#endif
