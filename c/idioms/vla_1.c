
#include <stdio.h>
#include <stdlib.h>

void print_array(int n, int m, float (*p)[n][m])
{
    if (!p) {
        printf("empty array\n");
        return;
    }
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            printf("[%d][%d] = %f\n", i, j, (*p)[i][j]);
}

int func1(int n, int m)
{
    float(*p)[n][m] = malloc(sizeof *p);
    if (!p)
        return -1;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            (*p)[i][j] = i + j;
    print_array(n, m, p);
    free(p);
    return 1;
}

int func2(int n, int m)
{
    // Caution: checks should be made to ensure N*M*sizeof(float) does NOT
    // exceed limitations for auto VLAs and is within available size of stack.
    float p[n][m]; // auto VLA is held on the stack, and sized when the
                   // function is invoked
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            p[i][j] = i + j;
    print_array(n, m, (float (*)[n][m])p);
    // no need to free(p) since it will disappear when the function exits,
    // along with the rest of the stack frame
    return 1;
}


int main(void)
{
    printf("func1:\n");
    func1(5, 5);
    printf("func2:\n");
    func2(5, 5);
    printf("DONE\n");
}
