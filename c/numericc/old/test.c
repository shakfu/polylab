#include <stdio.h>
#include "nr.h"
#include "nrutil.h"

void main() {
    
    // declarations
    int i, a, b;
    float **m = matrix(1, 2, 1, 2);
    float *v = vector(1,7);
    
    // populate vector
    v[1] = 1.1;
    v[2] = 2.1;
    v[3] = 3.1;
    v[4] = 4.1;
    v[5] = 5.1;
    v[6] = 6.1;
    v[7] = 7.1;
    
    for (i=1; i<8; i++)
        v[i] = (float)i + 0.5;
    
    // do something with vector
    i = 1;
    printf("vector[%d]: %f\n", i, v[i] );
    
    // populate matrix
    m[1][1] = 1.0;
    m[1][2] = 2.0;
    m[2][1] = 3.0;
    m[2][2] = 4.0;
    
    // do something with matrix
    a = 1;
    b = 1;
    m[a][b] = SQR(m[2][2] + m[2][1]);
    printf("matrix[%d][%d]: %f\n", a, b, m[a][b] );
    
    
    // free variables
    free_vector(v, 1, 7);
    free_matrix(m, 1, 2 , 1, 2);
    printf("ok");
}
