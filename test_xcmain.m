
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

//
// Tests found in file src/tests/bytecode.c
@interface bytecode_tests : XCTestCase
@end

@implementation bytecode_tests

- (void) setUp {
    current_test_case = self;
    [self setContinueAfterFailure: false];
}

- (void)test_bytecode_integer_arithmetic {
    test_bytecode_integer_arithmetic();
}

- (void)test_bytecode_floating_point_arithmetic {
    test_bytecode_floating_point_arithmetic();
}

- (void)test_bytecode_load_and_stores_move {
    test_bytecode_load_and_stores_move();
}

- (void)test_bytecode_conversions {
    test_bytecode_conversions();
}

- (void)test_bytecode_cmp {
    test_bytecode_cmp();
}

@end

//
// Tests found in file src/tests/parser.c
@interface parser_tests : XCTestCase
@end

@implementation parser_tests

- (void) setUp {
    current_test_case = self;
    [self setContinueAfterFailure: false];
}

- (void)test_parse_atoms {
    test_parse_atoms();
}

- (void)test_parse_types {
    test_parse_types();
}

- (void)test_parse_aggregate {
    test_parse_aggregate();
}

- (void)test_parse_enums {
    test_parse_enums();
}

- (void)test_parse_functypes {
    test_parse_functypes();
}

- (void)test_parse_func {
    test_parse_func();
}

- (void)test_parse_slice {
    test_parse_slice();
}

- (void)test_parse_field {
    test_parse_field();
}

- (void)test_parse_calls {
    test_parse_calls();
}

- (void)test_parse_compound {
    test_parse_compound();
}

- (void)test_parse_unary {
    test_parse_unary();
}

- (void)test_parse_binary {
    test_parse_binary();
}

- (void)test_parse_ternary {
    test_parse_ternary();
}

- (void)test_parse_val {
    test_parse_val();
}

- (void)test_parse_var {
    test_parse_var();
}

- (void)test_parse_foreign {
    test_parse_foreign();
}

- (void)test_parse_foreignblock {
    test_parse_foreignblock();
}

- (void)test_parse_label {
    test_parse_label();
}

- (void)test_parse_assign {
    test_parse_assign();
}

- (void)test_parse_return {
    test_parse_return();
}

- (void)test_parse_defer {
    test_parse_defer();
}

- (void)test_parse_goto {
    test_parse_goto();
}

- (void)test_parse_block {
    test_parse_block();
}

- (void)test_parse_if {
    test_parse_if();
}

- (void)test_parse_for {
    test_parse_for();
}

- (void)test_parse_switch {
    test_parse_switch();
}

@end
