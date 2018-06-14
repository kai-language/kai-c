
#import <XCTest/XCTest.h>

// NOTE: The XCTest Assertions expect to have `self` defined, so we do that for them and set self for each test case
XCTestCase *self;

void setSelfForTestCase(XCTestCase *testCase) {
    self = testCase;
}

#define ASSERT_MSG_VA(cond, ...) XCTAssert(cond, __VA_ARGS__)
#define ASSERT_MSG(cond, msg) ASSERT_MSG_VA(cond, "(" #cond ") " msg)
#define ASSERT(cond) ASSERT_MSG_VA(cond)
#define PANIC(msg) ASSERT_MSG_VA(0, msg)
#define UNIMPLEMENTED() ASSERT_MSG_VA(0, "unimplemented");
#include "../src/main.c"

//
// Tests found in file src/ast.c
@interface ast : XCTestCase
@end

@implementation ast

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_isExpr_and_isDecl {
    test_isExpr_and_isDecl();
}

- (void)test_doesExprAllocate {
    test_doesExprAllocate();
}

@end

//
// Tests found in file src/common.c
@interface common : XCTestCase
@end

@implementation common

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_arena {
    test_arena();
}

- (void)test_GetFileName {
    test_GetFileName();
}

@end

//
// Tests found in file src/checker.c
@interface checker : XCTestCase
@end

@implementation checker

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_canCoerce {
    test_canCoerce();
}

- (void)test_checkConstantDeclarations {
    test_checkConstantDeclarations();
}

- (void)test_checkConstantUnaryExpressions {
    test_checkConstantUnaryExpressions();
}

- (void)test_checkConstantBinaryExpressions {
    test_checkConstantBinaryExpressions();
}

@end

//
// Tests found in file src/array.c
@interface array : XCTestCase
@end

@implementation array

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_array {
    test_array();
}

@end

//
// Tests found in file src/lexer.c
@interface lexer : XCTestCase
@end

@implementation lexer

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_keywords {
    test_keywords();
}

- (void)test_lexer {
    test_lexer();
}

@end

//
// Tests found in file src/string.c
@interface string : XCTestCase
@end

@implementation string

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_stringInterning {
    test_stringInterning();
}

@end

//
// Tests found in file src/queue.c
@interface queue : XCTestCase
@end

@implementation queue

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_queue {
    test_queue();
}

@end

//
// Tests found in file src/error.c
@interface error : XCTestCase
@end

@implementation error

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_errorReporting {
    test_errorReporting();
}

@end

//
// Tests found in file src/types.c
@interface types : XCTestCase
@end

@implementation types

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_TypeIntern {
    test_TypeIntern();
}

@end

//
// Tests found in file src/map.c
@interface map : XCTestCase
@end

@implementation map

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_map {
    test_map();
}

- (void)test_mapCopy {
    test_mapCopy();
}

@end

//
// Tests found in file src/flags.c
@interface flags : XCTestCase
@end

@implementation flags

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_flagParsingAndDefaults {
    test_flagParsingAndDefaults();
}

@end

//
// Tests found in file src/parser.c
@interface parser : XCTestCase
@end

@implementation parser

- (void) setUp {
    setSelfForTestCase(self);
    [self setContinueAfterFailure: false];
}

- (void)test_parseExprAtom {
    test_parseExprAtom();
}

- (void)test_parseExprPrimary {
    test_parseExprPrimary();
}

- (void)test_parseExprUnary {
    test_parseExprUnary();
}

- (void)test_parseExprBinary {
    test_parseExprBinary();
}

- (void)test_parseExprTernary {
    test_parseExprTernary();
}

- (void)test_parseSimpleStmt {
    test_parseSimpleStmt();
}

- (void)test_parseStmt {
    test_parseStmt();
}

- (void)test_parseStruct {
    test_parseStruct();
}

- (void)test_parseUnion {
    test_parseUnion();
}

@end
