#ifndef FINLIB_H
#define FINLIB_H

/* Financial functions specified in finance_functions.md.
 *
 * Common arguments:
 *   rate  interest rate per period, as a fraction (0.05 = 5%)
 *   n     number of periods
 *
 * Suffixes: b = payments at the start of each period (annuity due),
 * l = a single lump sum. Without a suffix, payments fall at the end of
 * each period. Inputs with no solution return NaN or +-inf. */

/* Future value of a series of end-of-period payments.
 *   payment  amount paid each period
 * Returns the accumulated value after n periods. */
double fv(double rate, double n, double payment);

/* Future value of a series of start-of-period payments.
 *   payment  amount paid each period
 * Returns the accumulated value after n periods. */
double fvb(double rate, double n, double payment);

/* Future value of a single sum invested now.
 *   principal  amount invested now
 * Returns the value after n periods. */
double fvl(double rate, double n, double principal);

/* Present value of a series of end-of-period payments.
 *   payment  amount received each period
 * Returns the value today of all n payments. */
double pv(double rate, double n, double payment);

/* Present value of a series of start-of-period payments.
 *   payment  amount received each period
 * Returns the value today of all n payments. */
double pvb(double rate, double n, double payment);

/* Present value of a single sum received later.
 *   future  amount received after n periods
 * Returns the value today. */
double pvl(double rate, double n, double future);

/* Net present value of cash flows starting one period from now.
 *   cashflows  cashflows[i] falls at the end of period i + 1
 *   len        number of cash flows
 * Returns the sum of the discounted cash flows. */
double npv(double rate, const double *cashflows, int len);

/* Net present value of cash flows starting now.
 *   cashflows  cashflows[i] falls at the end of period i; [0] is undiscounted
 *   len        number of cash flows
 * Returns the sum of the discounted cash flows. */
double npvb(double rate, const double *cashflows, int len);

/* Internal rate of return: the rate at which npvb of the cash flows is 0.
 * Not in finance_functions.md; ported from ../finance.py.
 *   cashflows  cashflows[i] falls at the end of period i
 *   len        number of cash flows
 * Returns the rate per period. Returns NaN unless the nonzero cash flows
 * change sign exactly once, since the IRR is otherwise undefined or not
 * unique. Also NaN if any cash flow is NaN or inf, or if the IRR is too
 * close to -1 to represent. */
double irr(const double *cashflows, int len);

/* Payment that repays a loan with end-of-period payments.
 *   loan     amount borrowed
 *   balloon  balance left after n periods (0 for full repayment)
 * Returns the payment per period. */
double pmt(double rate, double n, double loan, double balloon);

/* Payment that repays a loan with start-of-period payments.
 *   loan     amount borrowed
 *   balloon  balance left after n periods (0 for full repayment)
 * Returns the payment per period. */
double pmtb(double rate, double n, double loan, double balloon);

/* Number of end-of-period payments needed to repay a loan.
 *   payment  amount paid each period
 *   loan     amount borrowed
 * Returns the periods needed, possibly fractional. Returns NaN if payment
 * is below the interest per period, and +inf if it equals it. */
double nper(double rate, double payment, double loan);

/* Number of start-of-period payments needed to repay a loan.
 *   payment  amount paid each period
 *   loan     amount borrowed
 * Returns the periods needed, possibly fractional. Returns NaN if payment
 * cannot cover the interest. */
double nperb(double rate, double payment, double loan);

/* Number of periods for a single sum to grow to a target.
 *   target     value to reach
 *   principal  amount invested now
 * Returns the periods needed, possibly fractional. */
double nperl(double rate, double target, double principal);

/* Interest rate at which a single sum grows to a target.
 *   n          number of periods
 *   target     value to reach
 *   principal  amount invested now
 * Returns the rate per period. */
double ratel(double n, double target, double principal);

/* Straight-line depreciation: the same charge every period.
 *   cost     purchase cost of the asset
 *   salvage  value at the end of its life
 *   life     useful life in periods
 * Returns the depreciation per period. */
double sln(double cost, double salvage, double life);

/* Sum-of-years'-digits depreciation: charges fall linearly each period.
 *   cost     purchase cost of the asset
 *   salvage  value at the end of its life
 *   life     useful life in periods
 *   period   period to evaluate, from 1
 * Returns the depreciation for period, or 0 outside [1, life]. */
double syd(double cost, double salvage, double life, int period);

/* Double-declining-balance depreciation: each period charges 2/life of
 * the remaining book value. Book value never falls below salvage.
 *   cost     purchase cost of the asset
 *   salvage  value at the end of its life
 *   life     useful life in periods
 *   period   period to evaluate, from 1
 * Returns the depreciation for period, or 0 outside [1, life]. */
double ddb(double cost, double salvage, double life, int period);

#endif
