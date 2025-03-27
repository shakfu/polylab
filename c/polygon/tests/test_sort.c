#include <stdio.h>
#include <stdlib.h>

int compare(const void* pa, const void* pb)
{
    const int* a = *(const int**)pa;
    const int* b = *(const int**)pb;
    if (a[0] == b[0])
        return a[1] - b[1];
    else
        return a[0] - b[0];
}
/*
#define NUMCMP(x,y) (((x) < (y)) ? -1 : ((x) > (y)) ? 1 : 0)

int compare ( const void *pa, const void *pb ) {
    const int (*a)[2] = *(const int (**)[2])pa;
    const int (*b)[2] = *(const int (**)[2])pb;
    int tmp;
    if((tmp=NUMCMP((*a)[0], (*b)[0]))==0)
        return NUMCMP((*a)[1], (*b)[1]);
    else
        return tmp;
}
*/
int main(void)
{
    int** array;
    int number = 10;
    int i;

    array = malloc(number * sizeof(int*));
    for (i = 0; i < number; i++) {
        array[i] = malloc(2 * sizeof(int));
        array[i][0] = rand() % 20;
        array[i][1] = rand() % 20;
    }
    for (i = 0; i < number; ++i)
        printf("%2d, %2d\n", array[i][0], array[i][1]);

    printf("\n");

    qsort(array, number, sizeof array[0], compare);

    for (i = 0; i < number; ++i)
        printf("%2d, %2d\n", array[i][0], array[i][1]);

    return 0;
}