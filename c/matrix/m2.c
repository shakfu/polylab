/*

2) Using an array of pointers 

We can create an array of pointers of size r. Note that from C99, C language allows 
variable sized arrays. After creating an array of pointers, we can dynamically allocate
memory for every row.

*/

#include <stdio.h>
#include <stdlib.h>

int main()
{
    int n_rows = 3, n_cols = 4, i, j, count;

    int* arr[n_rows];
    for (i = 0; i < n_rows; i++)
        arr[i] = (int*)malloc(n_cols * sizeof(int));

    // Note that arr[i][j] is same as *(*(arr+i)+j)
    count = 0;
    for (i = 0; i < n_rows; i++)
        for (j = 0; j < n_cols; j++)
            arr[i][j] = ++count; // Or *(*(arr+i)+j) = ++count

    for (i = 0; i < n_rows; i++)
        for (j = 0; j < n_cols; j++)
            printf("%d ", arr[i][j]);

    /* Code for further processing and free the
      dynamically allocated memory */

    for (int i = 0; i < n_rows; i++)
        free(arr[i]);

    return 0;
}

