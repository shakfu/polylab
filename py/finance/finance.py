#!/usr/bin/env python3
'''
A set of functions for quick financial analysis of an investment
opportunity and a series of projected cashflows.

For further details and pros/cons of each function please refer
to the respective wikipedia page:

    payback_period
        https://en.wikipedia.org/wiki/Payback_period

    net present value
        https://en.wikipedia.org/wiki/Net_present_value

    internal rate of return
        https://en.wikipedia.org/wiki/Internal_rate_of_return
'''

import argparse
import itertools
import sys


def payback_of_investment(investment: float, cashflows: list[float]) -> float:
    """The payback period refers to the length of time required
       for an investment to have its initial cost recovered.

       Returns the time at which cumulative cashflow first reaches the
       investment, interpolated linearly within that period. Later dips
       below the investment are ignored.

       >>> payback_of_investment(200.0, [60.0, 60.0, 70.0, 90.0])
       3.111111111111111
       >>> payback_of_investment(50.0, [60.0, 60.0])
       0.8333333333333334
       >>> payback_of_investment(100.0, [100.0, -50.0, 60.0, 10.0])
       1.0
       >>> payback_of_investment(100.0, [60.0, 30.0])
       Traceback (most recent call last):
       ...
       ValueError: insufficient cashflows: investment never recovered
    """
    if investment <= 0:
        raise ValueError("investment must be positive")
    cumulative = 0.0
    for period, cashflow in enumerate(cashflows):
        prev, cumulative = cumulative, cumulative + cashflow
        if cumulative >= investment:
            return period + (investment - prev) / cashflow
    raise ValueError("insufficient cashflows: investment never recovered")


def payback(cashflows: list[float]) -> float:
    """The payback period refers to the length of time required
       for an investment to have its initial cost recovered.

       (This version accepts a list of cashflows)

       >>> payback([-200.0, 60.0, 60.0, 70.0, 90.0])
       3.111111111111111
       >>> payback([200.0, 60.0])
       Traceback (most recent call last):
       ...
       ValueError: cashflow0 must be a negative investment
    """
    if not cashflows or cashflows[0] >= 0:
        raise ValueError("cashflow0 must be a negative investment")
    return payback_of_investment(-cashflows[0], cashflows[1:])


def npv(rate: float, cashflows: list[float]) -> float:
    """The total present value of a time series of cash flows.

        >>> npv(0.1, [-100.0, 60.0, 60.0, 60.0])
        49.21111945905332
        >>> npv(-1.0, [-100.0, 60.0])
        Traceback (most recent call last):
        ...
        ValueError: rate must be greater than -1
    """
    if rate <= -1:
        raise ValueError("rate must be greater than -1")
    # Horner form: large powers overflow to inf instead of raising OverflowError.
    discount = 1 / (1 + rate)
    total = 0.0
    for cashflow in reversed(cashflows):
        total = total * discount + cashflow
    return total


def irr(cashflows: list[float]) -> float:
    """The IRR or Internal Rate of Return is the annualized effective
       compounded return rate which can be earned on the invested
       capital, i.e., the yield on the investment.

       Requires exactly one sign change in the cashflows; otherwise the
       IRR is either undefined or not unique.

       >>> round(irr([-100.0, 60.0, 60.0, 60.0]), 12)
       0.363096539475
       >>> round(irr([-100.0, 10.0, 10.0, 10.0]), 12)
       -0.424417443832
       >>> irr([100.0, 60.0, 60.0])
       Traceback (most recent call last):
       ...
       ValueError: IRR needs exactly one sign change in cashflows
    """
    nonzero = [cashflow for cashflow in cashflows if cashflow]
    if sum((a > 0) != (b > 0) for a, b in itertools.pairwise(nonzero)) != 1:
        raise ValueError("IRR needs exactly one sign change in cashflows")

    # One sign change gives one root in (-1, inf) (Descartes' rule of signs).
    # Above the root, NPV takes the sign of the first nonzero cashflow.
    def above_root(rate: float) -> bool:
        return (npv(rate, cashflows) > 0) == (nonzero[0] > 0)

    if above_root(0.0):
        lo, hi = -0.5, 0.0
        while above_root(lo):
            lo = (lo - 1) / 2
    else:
        lo, hi = 0.0, 1.0
        while not above_root(hi):
            lo, hi = hi, hi * 2

    # NPV rounds to exactly 0 across a band of floats around the root. Return the
    # first one visited, so exact roots such as 0.0 or 0.5 come back exact.
    for rate in (lo, hi):
        if npv(rate, cashflows) == 0:
            return rate

    # Bisect until the midpoint is no longer representable between lo and hi.
    mid = (lo + hi) / 2
    while lo < mid < hi:
        value = npv(mid, cashflows)
        if value == 0:
            return mid
        if (value > 0) == (nonzero[0] > 0):
            hi = mid
        else:
            lo = mid
        mid = (lo + hi) / 2
    return mid


def format_currency(amount: float) -> str:
    """Format an amount with thousands separators and two decimals.

       >>> format_currency(-1234567.891)
       '-1,234,567.89'
    """
    return f"{amount:,.2f}"


def investment_analysis(discount_rate: float, cashflows: list[float]) -> dict:
    """Computes payback, IRR, NPV, and an approval verdict.

       Assumes that the first element of the list (i.e. at period 0)
       is the initial investment with a negative float value.
       Payback or IRR is None when it is undefined for the cashflows.
       payback_dips is True when cumulative cashflow falls back below the
       investment after first reaching it.

       >>> r = investment_analysis(0.05, [-100.0, 150.0, 50.0])
       >>> round(r["payback"], 4), r["payback_dips"], r["approve"]
       (0.6667, False, True)
    """
    if not cashflows or cashflows[0] >= 0:
        raise ValueError("cashflow0 must be a negative investment")
    npv_ = npv(discount_rate, cashflows)
    try:
        payback_ = payback(cashflows)
    except ValueError:
        payback_ = None
    investment = -cashflows[0]
    after_crossing = itertools.dropwhile(
        lambda total: total < investment, itertools.accumulate(cashflows[1:]))
    payback_dips = any(total < investment for total in after_crossing)
    try:
        irr_ = irr(cashflows)
    except ValueError:
        irr_ = None
    return {"payback": payback_, "payback_dips": payback_dips, "irr": irr_,
            "npv": npv_, "approve": npv_ > 0}


def format_report(discount_rate: float, cashflows: list[float], results: dict) -> str:
    """Renders the results of investment_analysis as a text report."""
    cells = [(str(year), format_currency(cf)) for year, cf in enumerate(cashflows)]
    columns = [("year", "cashflow"), *cells]
    payback_ = results["payback"]
    irr_ = results["irr"]
    lines = [
        "-" * 70,
        "  ".join(col[0].ljust(max(map(len, col))) for col in columns),
        "  ".join(col[1].ljust(max(map(len, col))) for col in columns),
        "",
        f"Discount Rate: {discount_rate * 100:.1f}%",
        "",
        "Payback: " + ("never" if payback_ is None else f"{payback_:.2f} years"),
        *(["         warning: cumulative cashflow later dips below the investment"]
          if results["payback_dips"] else []),
        "    IRR: " + ("undefined" if irr_ is None else f"{irr_ * 100:.2f}%"),
        f"    NPV: {format_currency(results['npv'])}",
        "",
        "==> {} investment of {}".format(
            "Approve" if results["approve"] else "Do Not Approve",
            format_currency(-cashflows[0])),
        "-" * 70,
    ]
    return "\n".join(line.rstrip() for line in lines)


def main(argv: list[str]) -> None:
    """commandline entry point
    """
    parser = argparse.ArgumentParser(
        prog="invest",
        description="Provides analysis of an investment and a series of cashflows.",
        epilog="example: invest 0.05 -10000 6000 6000 6000")
    parser.add_argument(
        "discount_rate", type=float,
        help="rate used to discount future cashflows to their present values")
    parser.add_argument(
        "cashflows", type=float, nargs="+", metavar="cashflow",
        help="cashflow0 is the investment (negative); later values are net "
             "inflows (positive) or net outflows (negative)")
    args = parser.parse_args(argv)
    try:
        results = investment_analysis(args.discount_rate, args.cashflows)
    except ValueError as e:
        parser.error(str(e))
    print(format_report(args.discount_rate, args.cashflows, results))


if __name__ == '__main__':
    main(sys.argv[1:])
