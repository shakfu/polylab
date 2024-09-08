/*

3) Using pointer to a pointer

We can create an array of pointers also dynamically using a double pointer.

Once we have an array pointers allocated dynamically, we can dynamically allocate
memory and for every row like method 2. 

*/


#include <stdio.h>
#include <stdlib.h>

int main()
{
    int n_rows = 3, n_cols = 4, len = 0;
    int *ptr, **arr;
    int count = 0, i, j;

    len = sizeof(int*) * n_rows + sizeof(int) * n_cols * n_rows;
    arr = (int**)malloc(len);

    // ptr is now pointing to the first element of the 2D
    // array
    // it points to the memory just after the row pointers
    // allocated memory
    ptr = (int*)(arr + n_rows);

    // for loop to point rows pointer to appropriate
    // location in 2D array
    for (i = 0; i < n_rows; i++)
        arr[i] = (ptr + n_cols * i);

    for (i = 0; i < n_rows; i++)
        for (j = 0; j < n_cols; j++)
            arr[i][j] = ++count; // OR *(*(arr+i)+j) = ++count

    for (i = 0; i < n_rows; i++)
        for (j = 0; j < n_cols; j++)
            printf("%d ", arr[i][j]);

    return 0;
}
