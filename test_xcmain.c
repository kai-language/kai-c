
#import <XCTest/XCTest.h>

// NOTE: The XCTest Assertions expect to have `self` defined, so we do that for them and set self for each test case
XCTestCase *current_test_case;

#define ASSERT_MSG_VA(cond, ...) _XCTPrimitiveAssertTrue(current_test_case, cond, @#cond, __VA_ARGS__)
#define ASSERT_MSG(cond, msg) ASSERT_MSG_VA((cond), "(" #cond ") " msg)
#define ASSERT(cond) ASSERT_MSG_VA((cond))
#define PANIC(msg) ASSERT_MSG_VA(0, msg)
#define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented");

#pragma clang diagnostic ignored "-Wmacro-redefined" // Allow redefinition of helper macros

#define PROFILING_ENABLED 0
#include "unity.c"

//
// Tests found in file src/string.c
@interface string_tests : XCTestCase
@end

@implementation string_tests

- (void) setUp {
    current_test_case = self;
    [self setContinueAfterFailure: false];
}

- (void)test_stringInterning {
    test_stringInterning();
}

@end

//
// Tests found in file src/queue.c
@interface queue_tests : XCTestCase
@end

@implementation queue_tests

- (void) setUp {
    current_test_case = self;
    [self setContinueAfterFailure: false];
}

- (void)test_queue {
    test_queue();
}

@end

//
// Tests found in file src/compiler.c
@interface compiler_tests : XCTestCase
@end

@implementation compiler_tests

- (void) setUp {
    current_test_case = self;
    [self setContinueAfterFailure: false];
}

- (void)test_flagParsingAndDefaults {
    test_flagParsingAndDefaults();
}

@end
