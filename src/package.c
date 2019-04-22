
#include "all.h"
#include "os.h"
#include "arena.h"
#include "queue.h"
#include "package.h"
#include "ast.h"
#include "compiler.h"
#include "checker.h"
#include "string.h"
#include "lexer.h"

extern u64 source_memory_usage;

bool is_package_path(const char *package_path, const char *search_path) {
    TRACE2(IMPORT, STR("package_path", package_path), STR("search_path", search_path));
    char path[MAX_PATH];
    path_copy(path, search_path);
    path_join(path, package_path);
    mode_t stat = file_mode(path);
    if (stat & S_IFDIR) return true; // Directories are always packages
    if ((stat & (S_IFREG | S_IFLNK)) == 0) return false; // Must be a file or symlink
    const char *ext = path_ext(path);
    return strcmp(ext, "kai") == 0;
}

void package_search_path(char dest[MAX_PATH], const char *path) {
    TRACE(IMPORT);
    path_copy(dest, path);
    const char *ext = path_ext(dest);
    if (strcmp(ext, "kai") == 0) {
        char *lastSlash = strrchr(dest, '/');
        if (lastSlash) *lastSlash = '\0';
    }
}

bool resolve_package_path(char dest[MAX_PATH], const char *path, Package *importer) {
    TRACE1(IMPORT, STR("path", path));
    if (importer) {
        if (is_package_path(path, importer->search_path)) {
            path_copy(dest, importer->search_path);
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

Package *import_package(const char *path, Package *importer) {
    TRACE(IMPORT);
    char package_path[MAX_PATH];
    path_copy(package_path, path);
    if (!resolve_package_path(package_path, path, importer)) {
        verbose("Failed to resolve package path for %s", path);
        return NULL;
    }
    if (!path_absolute(package_path)) {
        verbose("Failed to resolve absolute path for %s", path);
        return NULL;
    }
    const char *fullpath = str_intern(package_path);
    Package *package = hmget(compiler.packages, fullpath);
    if (!package) { // first time seeing this package
        package = arena_calloc(&compiler.arena, sizeof *package);
        memset(package, 0, sizeof *package);
        package->path = path;
        package->fullpath = fullpath;
        package->scope = scope_push(package, compiler.builtin_package.scope);
        char search_path[MAX_PATH];
        package_search_path(search_path, fullpath);
        package->search_path = str_intern(search_path);
        verbose("Importing %s", package->path);
        hmput(compiler.packages, fullpath, package);
        COUNTER1(IMPORT, "num_packages", INT("num", (int) hmlen(compiler.packages)));
        queue_push_back(&compiler.parsing_queue, package);
        COUNTER1(IMPORT, "parsing_queue", INT("length", (int) compiler.parsing_queue.size));
    }
    return package;
}

void package_read_source_files(Package *package) {
    TRACE1(IMPORT, STR("path", package->path));
    DirectoryIter iter;
    for (dir_iter_open(&iter, package->fullpath); iter.valid; dir_iter_next(&iter)) {
        if (iter.isDirectory || iter.name[0] == '.') continue;
        char name[MAX_PATH];
        path_copy(name, iter.name);
        char *ext = path_ext(name);
        if (ext == name || strcmp(ext, "kai") != 0) continue;
        ext[-1] = '\0';
        Source _source = {.start = package->total_sources_size};
        arrput(package->sources, _source);
        Source *source = &arrlast(package->sources);
        path_copy(source->name, name);
        path_copy(source->path, iter.base);
        path_join(source->path, iter.name);
        path_absolute(source->path);
        u64 len;
        BEGIN1(IO, "readfile", STR("path", source->path));
        source->code = ReadEntireFile(source->path, &len);
        END(IO, "readfile");
        if (!source->code) {
            Range range = {source->start, source->start};
            package->total_sources_size += 1;
            add_error(package, range, "Failed to read source file");
            return;
        }
        if (len + package->total_sources_size > UINT32_MAX)
            fatal("Packages with over 4GB of source code are unsupported.");
        source->len = (u32) len;
        package->total_sources_size += (u32) len;
        source_memory_usage += len;
    }
    dir_iter_close(&iter);
}

void package_posinfo_online(PosInfo *info, u32 offset) {
    Source *source = info->source;
    arrput(source->line_offsets, offset);
    info->line += 1;
}

PosInfo package_posinfo(Package *package, u32 pos) {
    TRACE(LEXING);
    Source *source;
    for (int i = 0; i < arrlen(package->sources); i++) {
        source = &package->sources[i];
        u32 end = source->start + source->len;
        if (pos > source->start && pos < end) goto found;
    }
    return (PosInfo){0};

found:;
    u32 offset = pos - source->start;
    u32 start_of_line = 0;
    if (source->line_offsets) {
        for (int i = 0; i < arrlen(source->line_offsets); i++) {
            u32 line_offset = source->line_offsets[i];
            if (line_offset > offset) {
                start_of_line = source->line_offsets[i - 1];
                break;
            }
        }
    }
    Lexer lexer;
    PosInfo info = {source, .line = 1};
    lexer_init(&lexer, source->code + offset);
    lexer.client.data = &info;
    lexer.client.online = (void *) package_posinfo_online;
    lexer_next_token(&lexer);
//    while (lexer.tok.offset_start <= offset) lexer_next_token(&lexer);
//    ASSERT(lexer.tok.offset_start == offset);
//    u32 start_of_line = arrlast(source->line_offsets);
    info.column = lexer.tok.offset_start - start_of_line;
    info.offset = lexer.tok.offset_start;
    return info;
}

void output_error(SourceError *error) {
    PosInfo location = error->location;
    fprintf(stderr, "error(%s:%u:%u) %s\n", location.source->name, location.line, location.column, error->msg);
    if (compiler.flags.error_source)
        fprintf(stderr, "%s", error->code_block);

    for (SourceNote *note = error->note; note; note = note->next) {
        location = note->location;
        fprintf(stderr, "note(%s:%u:%u) %s\n", location.source->name, location.line, location.column, note->msg);
    }
}

char *find_code_block_and_highlight_range(Package *package, Range range) {
    return NULL;
}

void add_error(Package *package, Range range, const char *fmt, ...) {
    va_list args;
    char msg[4096];
    va_start(args, fmt);
    int len = vsnprintf(msg, sizeof msg, fmt, args);
    va_end(args);
    if (len < 0) {
        perror("Encountered error while constructing compiler errors");
        len = 0;
    }
    PosInfo info = package_posinfo(package, range.start);
    SourceError error = {info};
    error.msg = arena_alloc(&package->arena, len + 1);
    memcpy(error.msg, msg, len + 1);
    if (compiler.flags.error_source)
        error.code_block = find_code_block_and_highlight_range(package, range);
    arrput(package->errors, error);
}

void add_note(Package *package, Range range, const char *fmt, ...) {
    ASSERT(package->errors);
    char msg[4096];
    va_list args;
    va_start(args, fmt);
    int len = vsnprintf(msg, sizeof msg, fmt, args);
    va_end(args);
    if (len < 0) {
        perror("Encountered error while constructing compiler errors");
        len = 0;
    }
    PosInfo info = package_posinfo(package, range.start);
    SourceNote note = {info};
    note.msg = arena_alloc(&package->arena, len + 1);
    memcpy(note.msg, msg, len + 1);
    arrput(package->notes, note);

    SourceError *error = &arrlast(package->errors);

    SourceNote **indirect = &error->note;
    while ((*indirect) != NULL)
        indirect = &(*indirect)->next;
    *indirect = &arrlast(package->notes);
}
