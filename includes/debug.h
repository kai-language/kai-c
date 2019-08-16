
#ifdef DEBUG_IMPLEMENTATION

void warn(const char *fmt, ...) {
    char buf[4096];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, 4096, fmt, args);
    va_end(args);
    fprintf(stderr, "warning: %s\n", buf);
}

_Noreturn
void fatal(const char *fmt, ...) {
    char buf[4096];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, 4096, fmt, args);
    va_end(args);
    Backtrace();
    fprintf(stderr, "fatal error: %s\n", buf);
    fflush(stderr);
#if DEBUG
    DEBUG_TRAP();
#endif
    exit(1);
}

INLINE
void verbose(const char *fmt, ...) {
    if (!compiler.flags.verbose) return;
    char buf[4096];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, 4096, fmt, args);
    va_end(args);
    printf("%s\n", buf);
}

void Backtrace(void) {
#define BACKTRACE_MAX_STACK_DEPTH 50
#if defined(__unix__)
    void* callstack[BACKTRACE_MAX_STACK_DEPTH];
    int i, frames = backtrace(callstack, BACKTRACE_MAX_STACK_DEPTH);
    char** strs = backtrace_symbols(callstack, frames);
    for (i = 0; i < frames; ++i) {
        fprintf(stderr, "%s\n", strs[i]);
    }
    free(strs);
#else
    // NOTE: Windows doesn't have a backtrace implementation that's not super tedious to use
#endif
}

void assertHandler(char const *file, i32 line, char const *msg, ...) {
    va_list args;
    va_start(args, msg);
    Backtrace();

    if (msg) {
        fprintf(stderr, "Assert failure: %s:%d: ", file, line);
        vfprintf(stderr, msg, args);
        fprintf(stderr, "\n");
    } else {
        fprintf(stderr, "Assert failure: %s:%d\n", file, line);
    }
    va_end(args);

    fflush(stderr);
}

#if DEBUG
// NOTE: For hash maps call arrheader(p - 1) ... p[-1] is the default value for the hm
stbds_array_header *arrheader(void *p) {
    if (!p) return p;
    return stbds_header(p);
}
#endif

#endif
