#include "nrutil.h"

float amotry(float** p, float y[], float psum[], int ndim,
             float (*funk)(float[]), int ihi, float fac)
/*
     Extrapolates by a factor fac through the face of the simplex across from
       the high point, tries it,
       and replaces the high point if the new point is better.
*/
{
    int j;
    float fac1, fac2, ytry, *ptry;
    ptry = vector(1, ndim);
    fac1 = (1.0 - fac) / ndim;
    fac2 = fac1 - fac;
    for (j = 1; j <= ndim; j++)
        ptry[j] = psum[j] * fac1 - p[ihi][j] * fac2;
    ytry = (*funk)(ptry);
    // Evaluate the function at the trial point.
    if (ytry < y[ihi]) { // If it's better than the highest, then replace the
                         // highest.
        y[ihi] = ytry;
        for (j = 1; j <= ndim; j++) {
            psum[j] += ptry[j] - p[ihi][j];
            p[ihi][j] = ptry[j];
        }
    }
    free_vector(ptry, 1, ndim);
    return ytry;
}
