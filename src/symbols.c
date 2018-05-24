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

    MapSet(&package->symbolMap, name, sym);
    ArrayPush(package->symbols, sym);
}
