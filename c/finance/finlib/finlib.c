#include <math.h>

#include "finlib.h"

/* (1 + rate)^n - 1; expm1/log1p keep precision when rate is small. */
static double growth(double rate, double n)
{
    return expm1(n * log1p(rate));
}

/* The rate == 0 branches take the limit of a removable 0/0 singularity. */

double fv(double rate, double n, double payment)
{
    return rate == 0 ? payment * n : payment * growth(rate, n) / rate;
}

double fvb(double rate, double n, double payment)
{
    return fv(rate, n, payment) * (1 + rate);
}

double fvl(double rate, double n, double principal)
{
    return principal * exp(n * log1p(rate));
}

double pv(double rate, double n, double payment)
{
    return rate == 0 ? payment * n : payment * -growth(rate, -n) / rate;
}

double pvb(double rate, double n, double payment)
{
    return pv(rate, n, payment) * (1 + rate);
}

double pvl(double rate, double n, double future)
{
    return future * exp(-n * log1p(rate));
}

double npvb(double rate, const double *cashflows, int len)
{
    /* No fused multiply-add: rounding the product first lets exact IRRs
       such as 0.5 give NPV exactly 0, as in finance.py. */
    #pragma STDC FP_CONTRACT OFF
    double discount = 1 / (1 + rate);
    double total = 0;

    /* Horner form: avoids computing large powers. */
    for (int i = len - 1; i >= 0; i--)
        total = total * discount + cashflows[i];
    return total;
}

double npv(double rate, const double *cashflows, int len)
{
    return npvb(rate, cashflows, len) / (1 + rate);
}

/* Above the IRR, NPV takes the sign of the first nonzero cash flow. */
static int above_root(double rate, const double *cashflows, int len,
                      int first_positive)
{
    return (npvb(rate, cashflows, len) > 0) == first_positive;
}

double irr(const double *cashflows, int len)
{
    int changes = 0, first_positive = 0, prev_positive = 0, seen = 0;
    double lo, hi, mid, value;

    for (int i = 0; i < len; i++) {
        if (!isfinite(cashflows[i]))
            return NAN;
        if (cashflows[i] == 0)
            continue;
        int positive = cashflows[i] > 0;
        if (!seen)
            first_positive = positive;
        else if (positive != prev_positive)
            changes++;
        seen = 1;
        prev_positive = positive;
    }
    /* One sign change gives one root in (-1, inf) (Descartes' rule of signs). */
    if (changes != 1)
        return NAN;

    if (above_root(0, cashflows, len, first_positive)) {
        lo = -0.5;
        hi = 0;
        while (lo > -1 && above_root(lo, cashflows, len, first_positive))
            lo = (lo - 1) / 2;
        /* Root lies closer to -1 than any double; finance.py raises here. */
        if (lo <= -1)
            return NAN;
    } else {
        lo = 0;
        hi = 1;
        while (!above_root(hi, cashflows, len, first_positive)) {
            lo = hi;
            hi *= 2;
        }
    }

    /* NPV rounds to exactly 0 across a band of floats around the root. Return
       the first one visited, so exact roots such as 0.0 or 0.5 come back exact. */
    if (npvb(lo, cashflows, len) == 0)
        return lo;
    if (npvb(hi, cashflows, len) == 0)
        return hi;

    /* Bisect until the midpoint is no longer representable between lo and hi. */
    mid = (lo + hi) / 2;
    while (lo < mid && mid < hi) {
        value = npvb(mid, cashflows, len);
        if (value == 0)
            return mid;
        if ((value > 0) == first_positive)
            hi = mid;
        else
            lo = mid;
        mid = (lo + hi) / 2;
    }
    return mid;
}

double pmt(double rate, double n, double loan, double balloon)
{
    if (rate == 0)
        return (loan - balloon) / n;
    return (loan - balloon * exp(-n * log1p(rate))) * rate / -growth(rate, -n);
}

double pmtb(double rate, double n, double loan, double balloon)
{
    return pmt(rate, n, loan, balloon) / (1 + rate);
}

double nper(double rate, double payment, double loan)
{
    if (rate == 0)
        return loan / payment;
    return -log1p(-loan * rate / payment) / log1p(rate);
}

double nperb(double rate, double payment, double loan)
{
    if (rate == 0)
        return loan / payment;
    return -log1p(-loan * rate / (payment * (1 + rate))) / log1p(rate);
}

double nperl(double rate, double target, double principal)
{
    return -log(principal / target) / log1p(rate);
}

double ratel(double n, double target, double principal)
{
    /* The spec's target^(1/n) / principal^(1/n) is NaN for negative inputs
       and overflows for n < 1; the ratio form does neither. */
    return pow(target / principal, 1 / n) - 1;
}

double sln(double cost, double salvage, double life)
{
    return (cost - salvage) / life;
}

double syd(double cost, double salvage, double life, int period)
{
    if (period < 1 || period > life)
        return 0;
    return (cost - salvage) * (life - period + 1) / (life * (life + 1) / 2);
}

double ddb(double cost, double salvage, double life, int period)
{
    double book = cost;
    double dep = 0;

    if (period < 1 || period > life)
        return 0;
    /* Book value may not fall below salvage (as in Excel's DDB). */
    for (int i = 1; i <= period; i++) {
        dep = fmax(0, fmin(book * 2 / life, book - salvage));
        book -= dep;
    }
    return dep;
}
