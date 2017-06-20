#!/usr/bin/env python
'''
A set of functions for quick financial analysis of an investment
opportunity and a series of projected cashflows.

For further details and pros and cons of each function please refer
to the respective wikipedia page:

    payback_period
        http://en.wikipedia.org/wiki/Payback_period

    net present value
        http://en.wikipedia.org/wiki/Net_present_value

    internal rate of return
        http://en.wikipedia.org/wiki/Internal_rate_of_return
'''

cpdef double payback_of_investment(double investment, list cashflows):
    """The payback period refers to the length of time required
       for an investment to have its initial cost recovered.

       >>> payback_of_investment(200.0, [60.0, 60.0, 70.0, 90.0])
       3.1111111111111112
    """
    cdef double total = 0.0
    cdef int years = 0
    cumulative = []
    if not cashflows or (sum(cashflows) < investment):
        raise Exception("insufficient cashflows")
    for cashflow in cashflows:
        total += cashflow
        if total < investment:
            years += 1
        cumulative.append(total)
    A = years
    B = investment - cumulative[years-1]
    C = cumulative[years] - cumulative[years-1]
    return A + (B/C)

cpdef double payback(list cashflows):
    """The payback period refers to the length of time required
       for an investment to have its initial cost recovered.

       >>> payback([-200.0, 60.0, 60.0, 70.0, 90.0])
       3.1111111111111112
    """
    investment, cashflows = cashflows[0], cashflows[1:]
    if investment < 0 : investment = -investment
    return payback_of_investment(investment, cashflows)

cpdef double npv(double rate, list cashflows):
    """The total present value of a time series of cash flows.

        >>> npv(0.1, [-100.0, 60.0, 60.0, 60.0])
        49.211119459053322
    """
    cdef double total = 0.0
    for i, cashflow in enumerate(cashflows):
        total += cashflow / (1 + rate)**i
    return total

cpdef irr(list cashflows, int iterations=100):
    """The IRR or Internal Rate of Return is the annualized effective
       compounded return rate which can be earned on the invested
       capital, i.e., the yield on the investment.

       >>> irr([-100.0, 60.0, 60.0, 60.0])
       0.36309653947517645
    """
    cdef double rate = 1.0
    investment = cashflows[0]
    for i in range(1, iterations+1):
        rate *= ( 1 - npv(rate, cashflows) / investment)
    return rate

cpdef investment_analysis(double discount_rate, list cashflows):
    """Provides summary investment analysis on a list of cashflows
       and a discount_rate.

       Assumes that the first element of the list (i.e. at period 0)
       is the initial investment with a negative float value.
    """
    _npv = npv(discount_rate, cashflows)
    ts = [('year', 'cashflow')] + [(str(x), str(y)) for (x,y) in zip(
           range(len(cashflows)), cashflows)]
    print "-" * 70
    for y,c in ts:
        print y + (len(c) - len(y) + 1)*' ',
    print
    for y,c in ts:
        print c + ' ',
    print
    print
    print "Discount Rate: %.1f%%" % (discount_rate * 100)
    print
    print "Payback: %.2f years" % payback(cashflows)
    print "    IRR: %.2f%%" % (irr(cashflows) * 100)
    print "    NPV: %s" % _npv
    print
    print "==> %s investment of %s" % (
        ("Approve" if _npv > 0 else "Do Not Approve"), -cashflows[0])
    print "-" * 70

