#pragma once

// requires
// arena.h

// checker.h
typedef struct Scope Scope;
typedef struct Sym Sym;
typedef struct OperandMapEntry OperandMapEntry;

// ast.h
typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Range Range;

// llvm.h
typedef struct Emitter Emitter;

typedef struct Source Source;
struct Source {
    const char *filename;
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

typedef struct SymMapEntry SymMapEntry;
struct SymMapEntry {
    const void *key;
    Sym *value;
};

typedef struct Package Package;
struct Package {
    const char *path;

    Source *sources; // arr
    u32 total_sources_size;

    Arena arena;

    Stmt **stmts;

    ImportMapEntry *imports; // hm
    Scope *scope;

    OperandMapEntry *operands; // hm
    SymMapEntry *symbols; // hm

    SourceError *errors; // arr
    SourceNote *notes;   // arr

    void *userdata;
};

typedef struct PackageMapEntry PackageMapEntry;
struct PackageMapEntry {
    const char *key;
    Package *value;
};

void output_errors(Package *package);
void add_error(Package *package, Range range, const char *fmt, ...);
void add_note(Package *package, Range range, const char *fmt, ...);
void package_read_source_files(Package *package);
Package *import_path(const char *path, Package *importer);
Source *package_source(Package *package, u32 pos);
PosInfo package_posinfo(Package *package, u32 pos);
