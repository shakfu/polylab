#include <stdio.h>
#include "nr.h"
#include "nrutil.h"

extern float trapzd(float (*func)(float), float a, float b, int n);

float func(float x) {
    return x+2.0;
}

void main() {
    
    // declarations
    printf("trapzd(func, 1.0, 2.0, 1) = %f\n", trapzd(func, 1.0, 2.0, 1));
    
}
