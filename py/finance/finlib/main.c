#include <stdio.h>
#include <math.h>
#include "finlib.h"

struct funclib {
    int age;
    float (*npv)(float, float*, int);
    float (*irr)(float*, int);
    float (*payback)(float*, int);
};

void test_funcs(void) 
{
    float cashflows[] = {-100.0, 60.0, 60.0, 60.0};
    int len = sizeof(cashflows) / sizeof(float);
    struct funclib fl;
    
    fl.age = 22;
    fl.npv = npv;
    fl.irr = irr;
    fl.payback = payback;

    printf("len: %d\n", len);
    printf("npv: %f\n", fl.npv(0.1, cashflows, len));
    printf("irr: %f\n", fl.irr(cashflows, len));
    printf("payback: %f\n", fl.payback(cashflows, len));

} 


int main(int argc, char *argv[])
{
    if (argc <= 2) {
        printf("usage: %s function [values]\n", argv[0]); 
    }
    test_funcs();
    return 0;
}

