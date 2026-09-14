#include <stdio.h>

#include "finlib.h"

int main(void)
{
    const double cashflows[] = {-100.0, 60.0, 60.0, 60.0};

    printf("fv(0.05, 10, 100)       = %f\n", fv(0.05, 10, 100));
    printf("pv(0.05, 10, 100)       = %f\n", pv(0.05, 10, 100));
    printf("npvb(0.1, cashflows)    = %f\n", npvb(0.1, cashflows, 4));
    printf("irr(cashflows)          = %f\n", irr(cashflows, 4));
    printf("pmt(0.05, 12, 1000, 0)  = %f\n", pmt(0.05, 12, 1000, 0));
    printf("nper(0.05, 100, 1000)   = %f\n", nper(0.05, 100, 1000));
    printf("ddb(10000, 1000, 5, 5)  = %f\n", ddb(10000, 1000, 5, 5));
    return 0;
}
