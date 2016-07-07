#include "common.h"

#define SIZEOF(type) printf(#type ": %li\n", sizeof(type))

void dec_to_bin(int n)
{
    int c, k;
    for (c = 31; c >= 0; c--) {
        k = n >> c;
        if (k & 1)
            printf("1");
        else
            printf("0");
    }
    printf("\n");
}

int main()
{
    int nums[3][3] = {
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
    };
    foreach(i, 3) {
        foreach(j, 3) {
            printf("[%i][%i] -> %i\n", i, j, nums[i][j]);
            dec_to_bin(nums[i][j]);
        }
    }
    SIZEOF(int);
    SIZEOF(int*);
    SIZEOF(float);
    SIZEOF(float*);
    SIZEOF(double);
    SIZEOF(double*);
    SIZEOF(char);
    SIZEOF(char*);
}