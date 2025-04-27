// from: https://c-faq.com/misc/multretval.html

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void polar_to_rectangular(double rho, double theta, double *xp, double *yp)
{
    *xp = rho * cos(theta);
    *yp = rho * sin(theta);
}

int main(void)
{
    double x, y;
    polar_to_rectangular(1., 3.14, &x, &y);
    printf("x: %f\n", x);
    printf("y: %f\n", y);
    return EXIT_SUCCESS;
}
