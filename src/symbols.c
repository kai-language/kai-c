#include "symbols.h"

void DeclarePackageSymbol(Package *package, const char *name, Symbol *sym) {
    Symbol *old = MapGet(&package->symbolMap, name);
    if (old) {
        Position pos = sym->decl ? sym->decl->start : (Position){0};
        ReportError(package, RedefinitionError, pos, "Duplicate definition of global symbol '%s'", name);
        if (old->decl) {
            ReportNote(package, old->decl->start, "Previous definition of '%s'", name);
        }
    }

    // TODO(Brett, vdka): Should we be hashing the string here or does the intern
    // system cover that for us? This function uses the pointer and not the
    // content of the string
    MapSetU64(&package->symbolMap, HashBytes(name, strlen(name)), (u64)sym);
    ArrayPush(package->symbols, sym);
}
