#define FUNC(x) ((*func)(x))

float trapzd(float (*func)(float), float a, float b, int n)
/*
    This routine computes the nth stage of refinement of an extended
    trapezoidal rule. func is input as a pointer to the function to be
    integrated between limits a and b, also input. When called with
    n=1, the routine returns the crudest estimate of integral b a f(x)dx.
    Subsequent calls with n=2,3,... (in that sequential order) will
    improve the accuracy by adding 2n-2 additional interior points.
*/

{
    float x, tnm, sum, del;
    static float s;
    int it, j;

    if (n == 1) {
        return (s = 0.5 * (b - a) * (FUNC(a) + FUNC(b)));
    } else {
        for (it = 1, j = 1; j < n - 1; j++)
            it <<= 1;
        tnm = it;
        del = (b - a) / tnm; // This is the spacing of the points to be added.
        x = a + 0.5 * del;
        for (sum = 0.0, j = 1; j <= it; j++, x += del)
            sum += FUNC(x);
        s = 0.5
            * (s
               + (b - a) * sum / tnm); // This replaces s by its refined value.
        return s;
    }
}
