#include "stdio.h"
#include "stdlib.h"


int testFunc(void) {

    int *ptr2 = malloc(N * sizeof(int));
    
    defer { if(ptr2) free(ptr2); }


    return 0;
}


int main(int argnum, char **args) {

    int N = 100;

    int *ptr = malloc(N * sizeof(int));

    defer { if(ptr) free(ptr); }
    defer { printf("deferred\n"); }

    
    if (argnum <  2) {
        return 0;
    }


    if (argnum >= 2) {
        return 0;
    } 

    testFunc();

    return 0;
}


