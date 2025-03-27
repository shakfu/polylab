
/*---------------------------------------------------------------------------*/
/*
Overview
  The mathematical model of a business problem is the system of equations
  and related mathematical expressions that describe the essense of the
  problem.

Decision Variables
  If there are n related quantifiable decisions to be made, they are
  represented as decision variables (x1, x2, ..., xn) whose respective
  values are to be determined.

The Objective Function
  The appropriate measure of performance is then expressed as a mathematical
  function of these decision variables (e.g. P = 3x1 + 2x2 + ... + 5xn)

Constraints
  Any restrictions on the values that can be assigned to these decision
  variables are also expressed mathematically, typically by inequalities
  or equations.

Parameters
  The constants (namely, the coefficients and right hand sides) in the
  constraints and the objective function are called the parameter of the
  model.
*/


/*---------------------------------------------------------------------------*/

#include "nrutil.h"
#include <math.h>
#define TINY 1.0e-10 // A small number.
#define NMAX 5000    // Maximum allowed number of function evaluations.
#define GET_PSUM                                                              \
    for (j = 1; j <= ndim; j++) {                                             \
        for (sum = 0.0, i = 1; i <= mpts; i++)                                \
            sum += p[i][j];                                                   \
        psum[j] = sum;                                                        \
    }
#define SWAP(a, b)                                                            \
    {                                                                         \
        swap = (a);                                                           \
        (a) = (b);                                                            \
        (b) = swap;                                                           \
    }

void amoeba(float** p, float y[], int ndim, float ftol, float (*funk)(float[]),
            int* nfunk)
/*
Multidimensional minimization of the function funk(x) where x[1..ndim] is a
vector in ndim dimensions, by the downhill simplex method of Nelder and Mead.
The matrix p[1..ndim+1] [1..ndim] is input. Its ndim+1 rows are
ndim-dimensional vectors which are the vertices of the starting simplex. Also
input is the vector y[1..ndim+1], whose components must be preinitialized to
the values of funk evaluated at the ndim+1 vertices (rows) of p; and ftol the
fractional convergence tolerance to be achieved in the function value (n.b.!).
On output, p and y will have been reset to ndim+1 new points all within ftol of
a minimum function value, and nfunk gives the number of function evaluations
taken.
*/
{
    float amotry(float** p, float y[], float psum[], int ndim,
                 float (*funk)(float[]), int ihi, float fac);
    int i, ihi, ilo, inhi, j, mpts = ndim + 1;
    float rtol, sum, swap, ysave, ytry, *psum;

    psum = vector(1, ndim);
    *nfunk = 0;
    GET_PSUM for (;;)
    {
        ilo = 1;
        // First we must determine which point is the highest (worst),
        // next-highest, and lowest (best), by looping over the points in the
        // simplex.
        ihi = y[1] > y[2] ? (inhi = 2, 1) : (inhi = 1, 2);
        for (i = 1; i <= mpts; i++) {
            if (y[i] <= y[ilo])
                ilo = i;
            if (y[i] > y[ihi]) {
                inhi = ihi;
                ihi = i;
            } else if (y[i] > y[inhi] && i != ihi)
                inhi = i;
        }
        rtol = 2.0 * fabs(y[ihi] - y[ilo])
            / (fabs(y[ihi]) + fabs(y[ilo]) + TINY);
        // Compute the fractional range from highest to lowest and return if
        // satisfactory.
        if (rtol < ftol) { // If returning, put best point and value in slot 1.
            SWAP(y[1], y[ilo])
            for (i = 1; i <= ndim; i++) SWAP(p[1][i], p[ilo][i]) break;
        }
        if (*nfunk >= NMAX)
            nrerror("NMAX exceeded");
        *nfunk += 2;

        // Begin a new iteration. First extrapolate by a factor -1 through the
        // face of the simplex across from the high point, i.e., reflect the
        // simplex from the high point.
        ytry = amotry(p, y, psum, ndim, funk, ihi, -1.0);
        if (ytry <= y[ilo])
            // Gives a result better than the best point, so try an additional
            // extrapolation by a factor 2.
            ytry = amotry(p, y, psum, ndim, funk, ihi, 2.0);
        else if (ytry >= y[inhi]) {
            // The reflected point is worse than the second-highest, so look
            // for an intermediate lower point, i.e., do a one-dimensional
            // contraction.
            ysave = y[ihi];
            ytry = amotry(p, y, psum, ndim, funk, ihi, 0.5);
            if (ytry
                >= ysave) { // Can't seem to get rid of that high point. Better
                            // contract around the lowest (best) point.
                for (i = 1; i <= mpts; i++) {
                    if (i != ilo) {
                        for (j = 1; j <= ndim; j++)
                            p[i][j] = psum[j] = 0.5 * (p[i][j] + p[ilo][j]);
                        y[i] = (*funk)(psum);
                    }
                }
                *nfunk += ndim; // Keep track of function evaluations.
                GET_PSUM        // Recompute psum.
            }
        } else
            --(*nfunk); // Correct the evaluation count.
    } // Go back for the test of doneness and the next iteration.
    free_vector(psum, 1, ndim);
}
