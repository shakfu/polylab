#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int compare(const void *pa, const void *pb) {
    const double *a = *(const double **)pa;
    const double *b = *(const double **)pb;    
    return (a[0] > b[0]) - (a[0] < b[0]);

}

int main(void){
    double **array;
    int number = 10;
    int i;

    // seed the random generator
    srand((unsigned)time(NULL));

    array = malloc(number * sizeof(double*));
    for (i = 0; i < number; i++){
        array[i] = malloc(2 * sizeof(double));
        array[i][0] = ((double)rand()/(double)RAND_MAX);
        array[i][1] = ((double)rand()/(double)RAND_MAX);
    }
    for(i = 0;i < number;++i)
        printf("%f, %f\n", array[i][0], array[i][1]);

    printf("\n");
    printf("sorted:\n");

    qsort(array, number, sizeof(double), compare);

    for(i = 0;i < number;++i)
        printf("%f, %f\n", array[i][0], array[i][1]);

    return 0;
}