#include "stdio.h"
#include "stdlib.h"


int main(int argnum, char **args) {

    int N = 100;

    int *ptr = (int *) malloc(N * sizeof(int));

    defer { if(ptr) free(ptr); }
    defer { printf("deferred\n"); }

    
    if (argnum <  2) {
        return 0;
    }


    if (argnum >= 2) {
        return 0;
    } 


    return 0;
}


