/* Expected values come from direct period-by-period sums or textbook
   schedules, not from the closed forms under test. */
#include <math.h>
#include <stdio.h>

#include "finlib.h"

static int failures;

#define CHECK_CLOSE(got, want, tol) check_close(__FILE__, __LINE__, #got, got, want, tol)
#define CHECK(cond) check(__FILE__, __LINE__, #cond, cond)

static void check_close(const char *file, int line, const char *expr,
                        double got, double want, double tol)
{
    if (!(fabs(got - want) <= tol * fmax(1, fabs(want)))) {
        printf("%s:%d: %s = %.17g, want %.17g\n", file, line, expr, got, want);
        failures++;
    }
}

static void check(const char *file, int line, const char *expr, int cond)
{
    if (!cond) {
        printf("%s:%d: %s is false\n", file, line, expr);
        failures++;
    }
}

/* Present value of n payments q plus balloon x; due = pay at period start.
   Equals the loan amount iff the payments amortise it to x. Summing in
   present-value terms avoids cancelling terms of size amt * (1+rate)^n. */
static double loan_pv(double rate, int n, double q, double x, int due)
{
    double total = x * pow(1 + rate, -n);

    for (int k = 1; k <= n; k++)
        total += q * pow(1 + rate, due ? 1 - k : -k);
    return total;
}

static void test_future_value(void)
{
    CHECK_CLOSE(fv(0.05, 10, 100), 1257.789253554883, 1e-12);
    CHECK_CLOSE(fvb(0.05, 10, 100), 1320.6787162326273, 1e-12);
    CHECK_CLOSE(fvl(0.05, 10, 100), 162.8894626777442, 1e-12);
}

static void test_present_value(void)
{
    CHECK_CLOSE(pv(0.05, 10, 100), 772.1734929184811, 1e-12);
    CHECK_CLOSE(pvb(0.05, 10, 100), 810.7821675644052, 1e-12);
    CHECK_CLOSE(pvl(0.05, 10, 100), 61.39132535407592, 1e-12);
}

static void test_npv(void)
{
    const double cf[] = {-100, 60, 60, 60};

    CHECK_CLOSE(npv(0.1, cf, 4), 44.737381326412105, 1e-12);
    CHECK_CLOSE(npvb(0.1, cf, 4), 49.21111945905332, 1e-12);
    CHECK_CLOSE(npvb(0.1, cf, 0), 0, 0);
    CHECK_CLOSE(npvb(0.1, cf, 1), -100, 0);
}

/* Cases and expected values from ../test_finance.py. */
static void test_irr_is_npv_root(void)
{
    const double a[] = {-100, 60, 60, 60};
    const double b[] = {-100, 10, 10, 10};
    const double c[] = {0, -100, 150};
    double d[201] = {-1};
    const struct { const double *cf; int len; double want; } cases[] = {
        {a, 4, 0.36309653947517},
        {b, 4, -0.42441744383163},
        {c, 3, 0.5},
        {d, 201, 1e6},
    };

    for (int i = 1; i < 201; i++)
        d[i] = 1e6;
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        double rate = irr(cases[i].cf, cases[i].len);
        CHECK_CLOSE(rate, cases[i].want, 1e-6);
        CHECK_CLOSE(npvb(rate, cases[i].cf, cases[i].len), 0, 1e-6);
    }
}

static void test_irr_exact_roots_are_exact(void)
{
    const double cases[][3] = {
        /* cf0, cf1, want */
        {-100, 100, 0}, {-100, 200, 1}, {-100, 300, 2}, {-100, 50, -0.5},
        {100, -200, 1}, {100, -50, -0.5}, {-100, 150, 0.5},
        {-100, 125, 0.25}, {-100, 75, -0.25},
    };
    const double gap[] = {-100, 0, 400};

    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++)
        CHECK_CLOSE(irr(cases[i], 2), cases[i][2], 0);
    CHECK_CLOSE(irr(gap, 3), 1, 0);
}

static void test_irr_rejects_undefined_or_multiple(void)
{
    const double one[] = {-100};
    const double lead_zero[] = {0, 60, 60};
    const double no_change[] = {100, 60, 60};
    const double two_changes[] = {-100, 200, -50};
    const double non_finite[] = {-100, NAN, 60};
    const double infinite[] = {-100, INFINITY};

    CHECK(isnan(irr(one, 0)));
    CHECK(isnan(irr(one, 1)));
    CHECK(isnan(irr(lead_zero, 3)));
    CHECK(isnan(irr(no_change, 3)));
    CHECK(isnan(irr(two_changes, 3)));
    CHECK(isnan(irr(non_finite, 3)));
    CHECK(isnan(irr(infinite, 2)));
}

static void test_irr_root_below_double_resolution(void)
{
    /* True root is 1 + rate = 1e-600, closer to -1 than any double.
       The trailing 0 makes NPV at -1 NaN, which must not loop forever. */
    const double cf[] = {-1e300, 1e-300, 0};

    CHECK(isnan(irr(cf, 3)));
}

static void test_pmt_amortises_to_balloon(void)
{
    const double amt = 10000, x = 2500;

    for (double rate = 0; rate < 0.3; rate += 0.0125)
        for (int n = 1; n <= 60; n += 7) {
            CHECK_CLOSE(loan_pv(rate, n, pmt(rate, n, amt, x), x, 0), amt, 1e-12);
            CHECK_CLOSE(loan_pv(rate, n, pmtb(rate, n, amt, x), x, 1), amt, 1e-12);
        }
}

static void test_nper_inverts_annuity(void)
{
    CHECK_CLOSE(nper(0.05, pmt(0.05, 12, 1000, 0), 1000), 12, 1e-12);
    CHECK_CLOSE(nperb(0.05, pmtb(0.05, 12, 1000, 0), 1000), 12, 1e-12);
    /* 100 grows to 200 at 5%: n = ln 2 / ln 1.05 */
    CHECK_CLOSE(nperl(0.05, 200, 100), 14.206699082890461, 1e-12);
}

static void test_nper_without_solution(void)
{
    /* Payment 50 is below the 100 of interest on 1000 at 10%. */
    CHECK(isnan(nper(0.1, 50, 1000)));
    CHECK(isnan(nperb(0.1, 50, 1000)));
    /* Payment equals interest: the loan never amortises. */
    CHECK(isinf(nper(0.1, 100, 1000)));
}

static void test_ratel(void)
{
    CHECK_CLOSE(ratel(10, 162.8894626777442, 100), 0.05, 1e-12);
    CHECK_CLOSE(ratel(10, -162.8894626777442, -100), 0.05, 1e-12);
    CHECK_CLOSE(ratel(0.5, 1e300, 1e299), 99, 1e-12);
}

static void test_zero_rate_limits(void)
{
    CHECK_CLOSE(fv(0, 10, 100), 1000, 0);
    CHECK_CLOSE(fvb(0, 10, 100), 1000, 0);
    CHECK_CLOSE(pv(0, 10, 100), 1000, 0);
    CHECK_CLOSE(pvb(0, 10, 100), 1000, 0);
    CHECK_CLOSE(pmt(0, 10, 1000, 100), 90, 0);
    CHECK_CLOSE(pmtb(0, 10, 1000, 100), 90, 0);
    CHECK_CLOSE(nper(0, 100, 1000), 10, 0);
    CHECK_CLOSE(nperb(0, 100, 1000), 10, 0);
    /* The limit branches must agree with the general form near 0. */
    CHECK_CLOSE(fv(1e-12, 10, 100), fv(0, 10, 100), 1e-9);
    CHECK_CLOSE(pv(1e-12, 10, 100), pv(0, 10, 100), 1e-9);
    CHECK_CLOSE(pmt(1e-12, 10, 1000, 100), pmt(0, 10, 1000, 100), 1e-9);
    CHECK_CLOSE(nper(1e-12, 100, 1000), nper(0, 100, 1000), 1e-6);
}

static void test_straight_line_and_syd(void)
{
    const double syd_want[] = {3000, 2400, 1800, 1200, 600};
    double total = 0;

    CHECK_CLOSE(sln(10000, 1000, 5), 1800, 0);
    for (int per = 1; per <= 5; per++) {
        CHECK_CLOSE(syd(10000, 1000, 5, per), syd_want[per - 1], 1e-12);
        total += syd(10000, 1000, 5, per);
    }
    CHECK_CLOSE(total, 9000, 1e-12);
    CHECK_CLOSE(syd(10000, 1000, 5, 0), 0, 0);
    CHECK_CLOSE(syd(10000, 1000, 5, 6), 0, 0);
}

static void test_ddb_stops_at_salvage(void)
{
    /* Period 5 unclamped would be 518.4, taking book to 777.6 < 1000. */
    const double low[] = {4000, 2400, 1440, 864, 296};
    const double high[] = {4000, 2400, 600, 0, 0};

    for (int per = 1; per <= 5; per++) {
        CHECK_CLOSE(ddb(10000, 1000, 5, per), low[per - 1], 1e-12);
        CHECK_CLOSE(ddb(10000, 3000, 5, per), high[per - 1], 1e-12);
    }
    CHECK_CLOSE(ddb(10000, 1000, 5, 0), 0, 0);
    CHECK_CLOSE(ddb(10000, 1000, 5, 6), 0, 0);
}

int main(void)
{
    test_future_value();
    test_present_value();
    test_npv();
    test_irr_is_npv_root();
    test_irr_exact_roots_are_exact();
    test_irr_rejects_undefined_or_multiple();
    test_irr_root_below_double_resolution();
    test_pmt_amortises_to_balloon();
    test_nper_inverts_annuity();
    test_nper_without_solution();
    test_ratel();
    test_zero_rate_limits();
    test_straight_line_and_syd();
    test_ddb_stops_at_salvage();

    if (failures)
        printf("%d check(s) failed\n", failures);
    else
        printf("all checks passed\n");
    return failures != 0;
}
