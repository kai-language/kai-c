#include "../src/common.cpp"
#include "../src/lexer.cpp"

// forward declarations
#include "../src/parser.hpp"
#include "../src/checker.hpp"

#include "../src/parser.cpp"
#include "../src/checker.cpp"


typedef struct TestData {
    i32 num;
} TestData;


i32 main(void) {

    printf("sizeof(TestData) = %lu\n\n", sizeof(TestData));

    ArrayArena<TestData> aa;

    initArrayArena(&aa, 10, 7);

    for (u32 i=0; i<12; ++i) {
        Array<TestData> ar;
        initArray(&aa, &ar);

        printf("ar.data = %p\n", ar.data);
        printf("ar.len  = %u\n", ar.len);
        printf("ar.cap  = %u\n", ar.cap);
    }

    return 0;
}


