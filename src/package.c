
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

bool is_package_path(const char *path) {
    TRACE1(IMPORT, STR("path", path));
    mode_t mode = file_mode(path);
    if (S_ISDIR(mode)) return true;
    if (!S_ISREG(mode)) return false;
    const char *ext = path_ext(path);
    return strcmp(ext, "kai") == 0;
}

void package_search_path(char search_path[MAX_PATH], const char *path) {
    TRACE(IMPORT);
    path_copy(search_path, path);
    const char *ext = path_ext(search_path); // TODO: use filemode instead of assuming ext means file?
    if (strcmp(ext, "kai") == 0) {
        char *lastSlash = strrchr(search_path, '/');
        if (lastSlash) *lastSlash = '\0';
    }
}

bool resolve_package_path(char package_path[MAX_PATH], const char *path, Package *importer) {
    TRACE1(IMPORT, STR("path", path));
    if (importer) {
        package_search_path(package_path, importer->path);
        path_join(package_path, path);
        if (is_package_path(package_path))
            return true;
    } else if (is_package_path(path)) {
        path_copy(package_path, path);
        return true;
    }
    for (int i = 0; i < compiler.num_global_search_paths; i++) {
        path_copy(package_path, compiler.global_search_paths[i]);
        path_join(package_path, path);
        if (is_package_path(package_path))
            return true;
    }
    return false;
}

Package *import_package(const char *path, Package *importer) {
    TRACE(IMPORT);
    char package_path[MAX_PATH];
    if (!resolve_package_path(package_path, path, importer)) {
        verbose("Failed to resolve package path for %s", path);
        return NULL;
    }
    path = str_intern(package_path);
    Package *package = hmget(compiler.packages, path);
    if (!package) { // first time seeing this package
        package = arena_calloc(&compiler.arena, sizeof *package);
        package->path = path;
        package->scope = scope_push(package, compiler.global_scope);
        verbose("Importing %s", package->path);
        hmput(compiler.packages, path, package);
        COUNTER1(IMPORT, "num_packages", INT("num", (int) hmlen(compiler.packages)));
        queue_push_back(&compiler.parsing_queue, package);
        COUNTER1(IMPORT, "parsing_queue", INT("length", (int) compiler.parsing_queue.size));
    }
    return package;
}

void package_read_source_files(Package *package) {
    TRACE1(IMPORT, STR("path", package->path));
    DirectoryIter iter;
    for (dir_iter_open(&iter, package->path); iter.valid; dir_iter_next(&iter)) {
        if (iter.isDirectory || iter.name[0] == '.') continue;
        Source source = {
            .filename = str_intern(iter.name),
            .start = package->total_sources_size
        };
        char filepath[MAX_PATH];
        path_copy(filepath, package->path);
        if (strcmp(package->path, iter.name) != 0) // @Hack supports file as package
            path_join(filepath, iter.name);
        u64 len;
        BEGIN1(IO, "readfile", STR("path", filepath));
        source.code = ReadEntireFile(filepath, &len);
        END(IO, "readfile");
        bool read_success = source.code != NULL;
        if (!read_success) {
            len = 1;
            source.code = xcalloc(len);
        }
        if (len + package->total_sources_size > UINT32_MAX)
            fatal("Packages with over 4GB of source code are unsupported.");
        source.len = (u32) len;
        package->total_sources_size += (u32) len;
        arrput(package->sources, source);
        if (!read_success) {
            Range range = {source.start, source.start};
            add_error(package, range, "Failed to read source file");
        }
        source_memory_usage += len;
    }
    dir_iter_close(&iter);
}

void package_posinfo_online(PosInfo *info, u32 offset) {
    Source *source = info->source;
    arrput(source->line_offsets, offset);
    info->line += 1;
}

Source *package_source(Package *package, u32 pos) {
    TRACE(LEXING);
    Source *source;
    for (int i = 0; i < arrlen(package->sources); i++) {
        source = &package->sources[i];
        u32 end = source->start + source->len;
        if (pos >= source->start && pos < end) return source;
    }
    return NULL;
}

PosInfo package_posinfo(Package *package, u32 pos) {
    TRACE(LEXING);
    Source *source = package_source(package, pos);
    if (!source) return (PosInfo){0};
    u32 offset = pos - source->start;
    u32 start_of_line = 0;
    u32 lineno = 0;
    if (source->line_offsets) {
        for (int i = 0; i < arrlen(source->line_offsets); i++) {
            lineno++;
            start_of_line = source->line_offsets[i];
            if (start_of_line >= offset) {
                start_of_line = source->line_offsets[i - 1];  // FIXME: Results in oob on first char error.
                break;
            }
        }
    }
    Lexer lexer;
    PosInfo info = {source, .line = lineno};
    lexer_init(&lexer, source->code);
    lexer.str += offset;
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

void output_error(Package *package, SourceError error) {
    PosInfo location = error.location;
    char filepath[MAX_PATH];
    path_copy(filepath, package->path);
    if (strcmp(package->path, location.source->filename) != 0) // @Hack supports file as package
        path_join(filepath, location.source->filename);
    fprintf(stderr, "error(%s:%u:%u) %s\n", filepath, location.line, location.column, error.msg);
    if (compiler.flags.error_source && error.code_block)
        fprintf(stderr, "%s", error.code_block);

    for (SourceNote *note = error.note; note; note = note->next) {
        location = note->location;
        fprintf(stderr, "note(%s:%u:%u) %s\n", filepath, location.line, location.column, note->msg);
    }
}

void output_errors(Package *package) {
    for (i64 i = 0; i < arrlen(package->errors); i++)
        output_error(package, package->errors[i]);
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
