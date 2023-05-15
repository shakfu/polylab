// see: https://www.dsprelated.com/freebooks/pasp/Variable_Delay_Lines.html

#include <stdio.h>

#define N 10

static double A[N];
static double *rptr = A; // read ptr
static double *wptr = A; // write ptr

void setdelay(int M) {
    rptr = wptr - M;
    while (rptr < A) {
        rptr += N;
    }
}


double delayline(double x) {
    double y;
    // A[wptr++] = x;
    // y = A[rptr++];
    *wptr++ = x;
    y = *rptr++;
    if ((wptr - A) >= N) {
        wptr -= N;
    }
    if ((rptr - A) >= N) {
        rptr -= N;
    }
    return y;
}


int main()
{
    setdelay(3);
    double y;
    double x = 0.0;
    for (int i = 0; i < 10; i++) {
        x += i;
        y = delayline(x);
        printf("x: %f -> y: %f\n", x, y);
    }
    
    return 0;
}