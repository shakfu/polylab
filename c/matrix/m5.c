/*

5) Using a pointer to Variable Length Array.

The dimensions of VLA are bound to the type of the variable.

Therefore one form a pointer to an array with run-time defined shape.
The pointer has to be dereferenced before subscripting with syntax (*arr)[i][j].

*/

#include <stdio.h>
#include <stdlib.h>

int main()
{
    int row = 3, col = 4, i, j, count;

    int (*arr)[row][col] = malloc(sizeof *arr);
    
    count = 0;
    for (i = 0; i < row; i++)
        for (j = 0; j < col; j++)
            (*arr)[i][j] = ++count;

    for (i = 0; i < row; i++)
        for (j = 0; j < col; j++)
            printf("%d ", (*arr)[i][j]);

    free(arr);
    
    return 0;
}
