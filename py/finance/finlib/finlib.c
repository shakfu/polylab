#include <stdio.h>
#include <math.h>

float npv(float rate, float cashflows[], int len)
{
    float total = 0.0;
    int i;

    for (i=0; i < len; i++) {
        total += (cashflows[i] / pow(1.0 + rate, i));
    }

    return total;

}

float irr(float cashflows[], int len)
{
    float rate = 1.0;
    float investment = cashflows[0];
    int i;

    for (i=0; i < len; i++) {
        rate *= (1 - npv(rate, cashflows, len) / investment);
    }
    return rate;
}

float payback_i(float investment, float cashflows[], int len)
{
    float total = 0.0;
    int years = 0;
    float cumulative[len];
    float A,B,C;
    int i;

    for (i=0; i < len; i++) {
        total += cashflows[i];
        if (total < investment) {
            years += 1;
        }
        cumulative[i] = total;
    }
    A = years;
    B = investment - cumulative[years-1];
    C = cumulative[years] - cumulative[years-1];
    return A + (B/C);
}

float payback(float cashflows[], int len)
{
    float investment = cashflows[0];
    float cashflows_1[len-1];
    int i;

    for (i=1; i < len; i++) {
        cashflows_1[i] = cashflows[i];
    }
    
    if (investment < 0) {
        investment = -investment;
    }
    return payback_i(investment, cashflows_1, len-1);
}



