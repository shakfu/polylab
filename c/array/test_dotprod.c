#include <math.h>
#include <stdio.h>

/* This function computes the dot product of two floating-point arrays ('x'
   and `y') of length `size'.  The product is communicated back in `z'. */

void dot_product(float x[], float y[], float z[], int size)
{
    int i;
    for (i = 0; i < size; i = i + 1) {
        z[i] = x[i] * y[i];
    }
}


/*  The test function exercises dot_product. */

int main(int argc, char* argv[])
{

    int i;
    float a[5], b[5], c[5];

    for (i = 0; i < 5; i = i + 1) {
        a[i] = i;
        b[i] = 5;
    }

    printf("A     = %g %g %g %g %g\n", a[0], a[1], a[2], a[3], a[4]);
    printf("B     = %g %g %g %g %g\n", b[0], b[1], b[2], b[3], b[4]);
    dot_product(a, b, c, 5);
    printf("A * B = %g %g %g %g %g\n", c[0], c[1], c[2], c[3], c[4]);
    return 0;
}
