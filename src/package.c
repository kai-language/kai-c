
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

void package_search_path(char search_path[MAX_PATH], const char *path) {
    TRACE(IMPORT);
    path_copy(search_path, path);
    const char *ext = path_ext(search_path); // TODO: use filemode instead of assuming ext means file?
    if (strcmp(ext, "kai") == 0) {
        char *lastSlash = strrchr(search_path, '/');
        if (lastSlash) *lastSlash = '\0';
    }
}

typedef enum PathType {
    PATH_INVALID,
    PATH_PACKAGE,
    PATH_FILE,
} PathType;

PathType resolve_import_path(char import_path[MAX_PATH], const char *path, Package *importer) {
    TRACE1(IMPORT, STR("path", path));

    if (!importer && path == compiler.input_name) { // Search from cwd
        path_copy(import_path, path);
        FileMode mode = file_mode(import_path);
        switch (mode) {
            case FILE_REGULAR:   return PATH_FILE;
            case FILE_DIRECTORY: return PATH_PACKAGE;
            default:
                warn("Expected a regular file or directory at path %s", import_path);
                return PATH_INVALID;
        }
    }

    if (importer) { // search relative to the package importing this
        package_search_path(import_path, importer->path);
        path_join(import_path, path);
        FileMode mode = file_mode(path);
        switch (mode) {
            case FILE_REGULAR:   return PATH_FILE;
            case FILE_DIRECTORY: return PATH_PACKAGE;
            case FILE_OTHER:
                warn("Expected a regular file or directory at path %s", import_path);
                return PATH_INVALID;
            case FILE_INVALID:
                break;
        }
    }

    for (int i = 0; i < compiler.num_import_search_paths; i++) {
        path_copy(import_path, compiler.import_search_paths[i]);
        path_join(import_path, path);
        FileMode mode = file_mode(import_path);
        switch (mode) {
        case FILE_REGULAR:   return PATH_FILE;
        case FILE_DIRECTORY: return PATH_PACKAGE;
        case FILE_OTHER:
            warn("Expected a regular file or directory at path %s", import_path);
            return PATH_INVALID;
        case FILE_INVALID:
            continue;
        }
    }
    return PATH_INVALID;
}

void package_add_file(Package *package, const char *path, const char *name) {
    TRACE(IMPORT);
    Source source = {
        .filename = str_intern(name),
        .start = package->total_sources_size
    };
    char filepath[MAX_PATH];
    path_copy(filepath, package->path);
    path_join(filepath, name);
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

Package *package_create(const char *path, bool is_dir) {
    Package *package = arena_calloc(&compiler.arena, sizeof *package);
    package->path = str_intern(path);
    package->scope = scope_push(package, compiler.global_scope);
    hmput(compiler.packages, path, package);
    verbose("Importing package %s %s", is_dir ? "dir" : "file", package->path);
    if (is_dir) package_read_source_files(package);
    COUNTER1(IMPORT, "num_packages", INT("num", (int) hmlen(compiler.packages)));
    queue_push_back(&compiler.parsing_queue, package);
    COUNTER1(IMPORT, "parsing_queue", INT("length", (int) compiler.parsing_queue.size));
    return package;
}

Package *import_path(const char *path, Package *importer) {
    TRACE(IMPORT);
    char import_path[MAX_PATH];
    PathType path_type = resolve_import_path(import_path, path, importer);
    switch (path_type) {
        case PATH_FILE: { // add file to importing package
            path = str_intern(import_path);
            verbose("Importing file %s", path);
            Package *package = importer;
            if (!package) {
                char *directory = import_path;
                char *filename = path_file(import_path);
                if (filename != directory) filename[-1] = '\0';
                else directory = ".";
                path = filename;
                package = package_create(directory, false);
                verbose("Importing file package %s", package->path);
            }
            package_add_file(package, package->path, path);
            return package;
        }
        case PATH_PACKAGE: {
            path = str_intern(import_path);
            Package *package = hmget(compiler.packages, path);
            if (!package) { // first time seeing this package
                package = package_create(path, true);
            }
            return package;
        }
        case PATH_INVALID:
            verbose("Failed to resolve package path for %s", path);
            return NULL;
    }
    fatal("Unhandled case above");
}

void package_read_source_files(Package *package) {
    TRACE1(IMPORT, STR("path", package->path));
    DirectoryIter iter;
    for (dir_iter_open(&iter, package->path); iter.valid; dir_iter_next(&iter)) {
        if (iter.isDirectory || iter.name[0] == '.') continue;
        char filepath[MAX_PATH];
        path_copy(filepath, package->path);
        if (strcmp(package->path, iter.name) != 0) // @Hack supports file as package
            path_join(filepath, iter.name);
        package_add_file(package, filepath, iter.name);
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
    u32 lineno = 1;
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

void package_object_path(Package *package, char object_name[MAX_PATH]) {
    strncpy(object_name, package->path, MAX_PATH);
    object_name[MAX_PATH - 1] = 0;
    char *ext = strrchr(object_name, '.');
    if (!ext) {
        char *end = object_name + strlen(object_name);
        *(end) = '.';
        *(end+1) = 'o';
        *(end+2) = '\0';
    } else {
        *(ext+1) = 'o';
        *(ext+2) = '\0';
    }
}

void output_error(Package *package, SourceError error) {
    PosInfo location = error.location;
    char filepath[MAX_PATH];
    path_copy(filepath, package->path);
    if (strcmp(package->path, location.source->filename) != 0) // @Hack supports file as package
        path_join(filepath, location.source->filename);
    fprintf(stderr, "error(%s:%u:%u) %s\n", filepath, location.line, location.column, error.msg);
    if (compiler.flags.error_source && error.code_block)
        fprintf(stderr, "%s\n", error.code_block);

    for (SourceNote *note = error.note; note; note = note->next) {
        location = note->location;
        fprintf(stderr, "note(%s:%u:%u) %s\n", filepath, location.line, location.column, note->msg);
    }
}

void output_errors(Package *package) {
    if (compiler.flags.developer) return;
    for (i64 i = 0; i < arrlen(package->errors); i++)
        output_error(package, package->errors[i]);
}

char *highlight_line(
    char *buf, const char *buf_end,
    const char *line, const char *line_end,
    const char *start, const char *end)
{
    ASSERT(line <= start);
    ASSERT(line_end >= end);
    int len_req = 0;

    // Print line up until start into buf
    len_req = snprintf(buf, buf_end - buf, "%.*s", (int)(start - line), line);
    if (len_req > buf_end - buf) return NULL;
    buf += len_req;
    line += len_req;

    if (compiler.flags.error_colors) {
        // Print highlight codes
        len_req = snprintf(buf, buf_end - buf, "\x1B[31m");
        if (len_req > buf_end - buf) return NULL;
        buf += len_req;
    }

    ASSERT(line == start);

    // Print error range
    len_req = snprintf(buf, buf_end - buf, "%.*s", (int)(end - start), line);
    if (len_req > buf_end - buf) return NULL;
    buf += len_req;
    line += len_req;

    if (compiler.flags.error_colors) {
        // Print reset code
        len_req = snprintf(buf, buf_end - buf, "\x1B[0m");
        if (len_req > buf_end - buf) return NULL;
        buf += len_req;
    }

    // Print the remaining line
    len_req = snprintf(buf, buf_end - buf, "%.*s\n", (int)(line_end - line), line);
    if (len_req > buf_end - buf) return NULL;
    buf += len_req;
    line += len_req;

    // + 1 because we append a newline
    ASSERT(line == line_end + 1);

    return buf;
}

// FIXME: This can run past the start if the error is on the first line for example.
char *package_highlighted_range(Package *package, Range range) {
    PosInfo pos = package_posinfo(package, range.start);

#define MAX_LINES 3
#define MAX_LINE_LENGTH 512

    // We add 1 so we have a buffer incase we have color turned off and need to add ^^^^^^ pointers
    char lines[MAX_LINES + 1][MAX_LINE_LENGTH];
    char no_color_highlight[MAX_LINE_LENGTH];

    const char *cursor = pos.source->code + pos.offset;

    i64 nbytes = 0;
    i64 nlines = 0;
    i64 column = INT64_MAX;
    // first find a common column offset to remove indentation with
    for (i64 line = 0; line < MAX_LINES; line++) {
        const char *code_start = cursor;
        while (*cursor != '\n') {
            if (!isspace(*cursor)) code_start = cursor;
            cursor--;
        }
        // cursor is the end of the previous line (+1 is start of this line)
        column = MIN(column, code_start - (cursor + 1));
        cursor--;
        // we only print contiguious code. Break on empty lines
        if (*cursor == '\n' || cursor < pos.source->code) break;
    }
    // reset cursor
    cursor = pos.source->code + pos.offset;

    for (i64 line = 0; line < MAX_LINES; line++) {
        const char *start = cursor;
        const char *end = cursor;
        while (*cursor != '\n' && cursor >= pos.source->code) {
            cursor--;
        }
        // cursor is the end of the previous line (+1 is start of this line)
        start = (cursor + 1) + column;
        while (*end != '\n') end++;

        // FIXME: Handle ranges that span multiple lines
        if (line != 0) {
            nbytes += snprintf(lines[line], sizeof lines[line], "%.*s\n", (int)(end - start), start);
        } else {
            char *buf = lines[line];
            u32 line_offset = (u32) (start - pos.source->code);
            u32 start_offset = pos.offset - line_offset;
            u32 end_offset = start_offset + range.end - range.start;

            char *result = highlight_line(
                buf, buf + MAX_LINE_LENGTH, start, end, start + start_offset, start + end_offset);
            if (!result) return NULL;
            nbytes += strlen(buf);
            if (!compiler.flags.error_colors) {
                memset(no_color_highlight, ' ', end - start + 1);
                memset(no_color_highlight + start_offset, '^', range.end - range.start);
                no_color_highlight[end - start] = '\0';
            }
        }

        nlines++;
        cursor--;
        // only print contiguious code. Break on empty lines
        if (*cursor == '\n' || cursor < pos.source->code) break;
    }

    nbytes += 1 + 1; // 1 for indentation (\t) 1 for nul termination.
    if (!compiler.flags.error_colors) nbytes += strlen(lines[nlines - 1]) + 1;
    char *results = arena_alloc(&package->arena, nbytes + 1 + 40);
    char *line_cursor = results;
    for (i64 line = nlines - 1; line >= 0; line--) {
        line_cursor += sprintf(line_cursor, "\t%s", lines[line]);
    }
    if (!compiler.flags.error_colors)
        line_cursor += sprintf(line_cursor, "\t%s", no_color_highlight);
    *line_cursor = '\0';
    return results;
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
        error.code_block = package_highlighted_range(package, range);
    arrput(package->errors, error);

    if (compiler.flags.developer) output_error(package, error);
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
