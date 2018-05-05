#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <strings.h>

#if defined(_WIN32) || defined(_WIN64)
	#ifndef SYSTEM_WINDOWS
	#define SYSTEM_WINDOWS 1
	#endif
#elif defined(__APPLE__) && defined(__MACH__)
    #ifndef SYSTEM_POSIX
    #define SYSTEM_POSIX 1
    #endif
	
    #ifndef SYSTEM_OSX
	#define SYSTEM_OSX 1
	#endif
#elif defined(__unix__)
	#ifndef SYSTEM_POSIX
    #define SYSTEM_POSIX 1
    #endif

    #ifndef SYSTEM_UNIX
	#define SYSTEM_UNIX 1
	#endif

	#if defined(__linux__)
		#ifndef SYSTEM_LINUX
		#define SYSTEM_LINUX 1
		#endif
	#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
		#ifndef SYSTEM_FREEBSD
		#define SYSTEM_FREEBSD 1
		#endif
	#else
		#error This UNIX operating system is not supported
	#endif
#else
	#error This operating system is not supported
#endif

#if SYSTEM_POSIX
    #include <fcntl.h>
    #include <sys/stat.h>
    #include <sys/mman.h>
    #include <execinfo.h>
#elif SYSTEM_WINDOWS

#endif

#define ArrayCount(Array) (sizeof(Array) / sizeof((Array)[0]))
#define KB(x) (  (x)*1024LL)
#define MB(x) (KB(x)*1024LL)
#define GB(x) (MB(x)*1024LL)
#define TB(x) (GB(x)*1024LL)

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;

typedef float    f32;
typedef double   f64;

typedef i8       b8;
typedef i32      b32;


#if defined(_MSC_VER)
    #if _MSC_VER < 1300
    #define DEBUG_TRAP() __asm int 3
    #else
    #define DEBUG_TRAP() __debugbreak()
    #endif
#else
    #define DEBUG_TRAP() __builtin_trap()
#endif

#if !defined(RELEASE) && !defined(ASSERTS)
    #define ASSERT_MSG_VA(cond, msg, ...) do { \
        if (!(cond)) { \
            assertHandler(__FILE__, (i64)__LINE__, msg, __VA_ARGS__); \
            DEBUG_TRAP(); \
        } \
    } while(0)

    #define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, msg, 0)

    #define ASSERT(cond) ASSERT_MSG_VA(cond, 0, 0)
    #define PANIC(msg) ASSERT_MSG_VA(0, msg, 0)
    #define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented", 0);
#else
    #define ASSERT_MSG_VA(cond, msg, ...)
    #define ASSERT_MSG(cond, msg)
    #define ASSERT(cond)
    #define PANIC(msg)
    #define UNIMPLEMENTED()
#endif

#if !defined(Inline)
	#if defined(_MSC_VER)
		#if _MSC_VER < 1300
		#define Inline
		#else
		#define Inline __forceinline
		#endif
	#else
		#define Inline __attribute__ ((__always_inline__))
	#endif
#endif

void assertHandler(char const *file, i32 line, char const *msg, ...);

struct Allocator {

};

#include "string.cpp"
#include "utf.cpp"
#include "array.cpp"
#include "hash.cpp"
#include "map.cpp"

b32 ReadFile(String *data, String path) {
    i32 file;
    if ((file = open((const char *)path.data, O_RDONLY)) < -1) {
        return false;
    }

    struct stat buf;
    if (fstat(file, &buf) < 0 ) {
        return false;
    }

    u64 len = buf.st_size;
    void *address = mmap(NULL, len, PROT_READ, MAP_PRIVATE, file, 0);
    if (address == MAP_FAILED)
        return false;

    *data = MakeString((u8 *) address, (u32)len);

    return true;
}
