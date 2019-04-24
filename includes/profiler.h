
#ifndef PROFILING_ENABLED
#define PROFILING_ENABLED 1
#endif

typedef struct SPDR_Context SPDR_Context;

extern struct SPDR_Context *spdr;
void profiler_init(void);

#if PROFILING_ENABLED
#include "spdr/spdr.h"

// Categories
#define GENERAL "general"
#define INIT "initialization"
#define IMPORT "import"
#define LEXING "lexing"
#define PARSING "parsing"
#define CHECKING "checking"
#define INTERN "interning"
#define IO "io"

// Arguments
#define INT(key, value) SPDR_INT(key, value)
#define FLOAT(key, value) SPDR_FLOAT(key, value)
#define STR(key, value) SPDR_STR(key, value)

// Instant events
#define EVENT(category, name) \
    SPDR_EVENT(spdr, category, name)

#define EVENT1(category, name, arg0) \
    SPDR_EVENT1(spdr, category, name, arg0)

#define EVENT2(category, name, arg0, arg1) \
    SPDR_EVENT2(spdr, category, name, arg0, arg1)

#define EVENT3(category, name, arg0, arg1, arg2) \
    SPDR_EVENT3(spdr, category, name, arg0, arg1, arg2)

// Work slices

#define BEGIN(category, name) \
    SPDR_BEGIN(spdr, category, name)

#define BEGIN1(category, name, arg0) \
    SPDR_BEGIN1(spdr, category, name, arg0)

#define BEGIN2(category, name, arg0, arg1) \
    SPDR_BEGIN2(spdr, category, name, arg0, arg1)

#define BEGIN3(category, name, arg0, arg1, arg2) \
    SPDR_BEGIN3(spdr, category, name, arg0, arg1, arg2)

#define END(category, name) \
    SPDR_END(spdr, category, name)

// Scoped work slices (only works on GCC & Clang)

#define TRACE(category) \
    SPDR_SCOPE(spdr, category, __FUNCTION__)

#define TRACE1(category, arg0) \
    SPDR_SCOPE1(spdr, category, __FUNCTION__, arg0)

#define TRACE2(category, arg0, arg1) \
    SPDR_SCOPE2(spdr, category, __FUNCTION__, arg0, arg1)

#define TRACE3(category, arg0, arg1, arg2) \
    SPDR_SCOPE3(spdr, category, __FUNCTION__, arg0, arg1, arg2)

// Counters

#define COUNTER1(category, name, arg0) \
    SPDR_COUNTER1(spdr, category, name, arg0)

#define COUNTER2(category, name, arg0, arg1) \
    SPDR_COUNTER2(spdr, category, name, arg0, arg1)

#define COUNTER3(category, name, arg0, arg1, arg2) \
    SPDR_COUNTER3(spdr, category, name, arg0, arg1, arg2)

// Async Events

#define ASYNC_EVENT_BEGIN(category, name, id) \
    SPDR_ASYNC_EVENT_BEGIN(spdr, category, name, SPDR_INT("id", id))

#define ASYNC_EVENT_BEGIN1(category, name, id, arg0) \
    SPDR_ASYNC_EVENT_BEGIN1(spdr, category, name, SPDR_INT("id", id), arg0)

#define ASYNC_EVENT_BEGIN2(category, name, id, arg0, arg1) \
    SPDR_ASYNC_EVENT_BEGIN2(spdr, category, name, SPDR_INT("id", id), arg0, arg1)

#define ASYNC_EVENT_END(category, name, id) \
    SPDR_ASYNC_EVENT_END(spdr, category, name, SPDR_INT("id", id))

// Metadata

#define METADATA1(name, arg0) \
    SPDR_METADATA1(spdr, name, arg0)

#endif

#ifdef PROFILER_IMPLEMENTATION
#if defined(__APPLE__)
#include "../deps/uu.spdr/src/spdr_osx_unit.c"
#elif defined(__unix__)
#include "../deps/uu.spdr/src/spdr_linux_unit.c"
#elif defined(_WIN64)
#include "../deps/uu.spdr/src/spdr_win64_unit.c"
#elif defined(_WIN32)
#include "../deps/uu.spdr/src/spdr_win32_unit.c"
#endif

struct SPDR_Context *spdr;
static FILE *tracefile;

static void trace(const char *line, void *_) {
        char buffer[512] = "";
        strncat(buffer, line, sizeof buffer - 2);
        strncat(buffer, "\n", sizeof buffer - 2);
        fputs(buffer, tracefile);
}

// 10MB
#define TRACE_MEMORY 10 * 1024 * 1024
static char spdr_buffer[TRACE_MEMORY];

void profiler_init(void) {
    tracefile = fopen("trace.json", "w");
    if (!tracefile) perror("Failed to open trace.json");

    spdr_init(&spdr, spdr_buffer, TRACE_MEMORY);
    spdr_enable_trace(spdr, PROFILING_ENABLED);
    spdr_set_log_fn(spdr, trace, NULL);

    METADATA1("thread_name", STR("name", "Main_thread"));
}

void profiler_output(void) {
    tracefile = fopen("chrome_trace.json", "wb");
    if (tracefile) {
        spdr_report(spdr, SPDR_CHROME_REPORT, trace, NULL);
        fclose(tracefile);
    }
}
#endif

#if !PROFILING_ENABLED

// Categories
#define GENERAL
#define INIT
#define IMPORT

// Arguments
#define INT(key, value)
#define FLOAT(key, value)
#define STR(key, value)

// Instant events
#define EVENT(category, name)

#define EVENT1(category, name, arg0)

#define EVENT2(category, name, arg0, arg1)

#define EVENT3(category, name, arg0, arg1, arg2)

// Work slices

#define BEGIN(category, name)

#define BEGIN1(category, arg0, name)

#define BEGIN2(category, arg0, arg1, name)

#define BEGIN3(category, arg0, arg1, arg2, name)

#define END(category, name)

// Scoped work slices (only works on GCC & Clang)

#define TRACE(category)

#define TRACE1(category, arg0)

#define TRACE2(category, arg0, arg1)

#define TRACE3(category, arg0, arg1, arg2)

// Counters

#define COUNTER1(category, name, arg0)

#define COUNTER2(category, name, arg0, arg1)

#define COUNTER3(category, name, arg0, arg1, arg2)

// Async Events

#define ASYNC_EVENT_BEGIN(category, name, id)

#define ASYNC_EVENT_BEGIN1(category, name, id, arg0)

#define ASYNC_EVENT_BEGIN2(category, name, id, arg0, arg1)

#define ASYNC_EVENT_END(category, name, id)

// Metadata

#define METADATA1(name, arg0)

#endif

