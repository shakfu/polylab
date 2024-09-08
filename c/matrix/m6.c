/*

6) Using a pointer to the first row of VLA

Similar to 5 but allows arr[i][j] syntax.

*/


#include <stdio.h>
#include <stdlib.h>

int main()
{
    int row = 3, col = 4, i, j, count;

    int (*arr)[col] = calloc(row, sizeof *arr);
    
    count = 0;
    for (i = 0; i < row; i++)
        for (j = 0; j < col; j++)
            arr[i][j] = ++count;

    for (i = 0; i < row; i++)
        for (j = 0; j < col; j++)
            printf("%d ", arr[i][j]);

    free(arr);
    
    return 0;
}

