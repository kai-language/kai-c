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
    printf("sizeof(Array<TestData>) = %lu\n\n", sizeof(Array<TestData>));
    printf("sizeof(ArrayArena<TestData>) = %lu\n\n", sizeof(ArrayArena<TestData>));


    ArrayArena<TestData> aa;

    initArrayArena(&aa, 10, 7);

    Array<TestData> ar;

    for (u32 i=0; i<14; ++i) {
        initArray(&aa, &ar);


        dumpArray(ar);
    }

    printf("ar is a slice : %d\n", (bool) ARRAY_IS_SLICE((&ar)));

    reallocArray(aa, &ar, 15);

    printf("ar is a slice : %d\n", (bool) ARRAY_IS_SLICE((&ar)));

    reallocArray(aa, &ar, 20);

    return 0;
}


