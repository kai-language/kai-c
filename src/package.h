#pragma once

// requires
// arena.h

// checker.h
typedef struct Scope Scope;
typedef struct Sym Sym;

// ast.h
typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Range Range;

typedef struct Source Source;
struct Source {
    char name[MAX_NAME];
    char path[MAX_PATH];
    const char *code;
    u32 start;
    u32 len;

    u32 *line_offsets; // arr
};

typedef struct PosInfo PosInfo;
struct PosInfo {
    Source *source;
    u32 offset;
    u32 line;
    u32 column;
};

typedef struct SourceNote SourceNote;
struct SourceNote {
    PosInfo location;
    char *msg;
    SourceNote *next;
};

typedef struct SourceError SourceError;
struct SourceError {
    PosInfo location;
    char *msg;
    char *code_block;
    SourceNote *note;
};

typedef struct ImportMapEntry ImportMapEntry;
struct ImportMapEntry {
    Decl *key;
    Sym *value;
};

typedef struct OperandMapEntry OperandMapEntry;

typedef struct Package Package;
struct Package {
    const char *path;
    const char *fullpath;
    const char *search_path;

    Source *sources; // arr
    u32 total_sources_size;

    Arena arena;

    Stmt **stmts;

    ImportMapEntry *imports; // hm
    Scope *scope;

    OperandMapEntry *operands; // hm

    SourceError *errors; // arr
    SourceNote *notes;   // arr

    void *userdata;
};

typedef struct PackageMapEntry PackageMapEntry;
struct PackageMapEntry {
    const char *key;
    Package *value;
};

void add_error(Package *package, Range range, const char *fmt, ...);
void add_note(Package *package, Range range, const char *fmt, ...);
void package_read_source_files(Package *package);
Package *import_package(const char *path, Package *importer);
PosInfo package_posinfo(Package *package, u32 pos);
