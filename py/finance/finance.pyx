#!/usr/bin/env python
'''
A set of functions for quick financial analysis of an investment
opportunity and a series of projected cashflows.

For further details and pros/cons of each function please refer
to the respective wikipedia page:

    payback_period 
        http://en.wikipedia.org/wiki/Payback_period
    
    net present value 
        http://en.wikipedia.org/wiki/Net_present_value
        
    internal rate of return
        http://en.wikipedia.org/wiki/Internal_rate_of_return
'''

def payback_of_investment(float investment, cashflows):
    """The payback period refers to the length of time required 
       for an investment to have its initial cost recovered.
       
       >>> payback_of_investment(200.0, [60.0, 60.0, 70.0, 90.0])
       3.1111111111111112
    """
    cdef float total = 0.0
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

def payback(cashflows):
    """The payback period refers to the length of time required
       for an investment to have its initial cost recovered.
       
       (This version accepts a list of cashflows)
       
       >>> payback([-200.0, 60.0, 60.0, 70.0, 90.0])
       3.1111111111111112
    """
    cdef double investment = cashflows[0]
    cashflows = cashflows[1:]
    if investment < 0 : investment = -investment
    return payback_of_investment(investment, cashflows)

def npv(float rate, cashflows):
    """The total present value of a time series of cash flows.
    
        >>> npv(0.1, [-100.0, 60.0, 60.0, 60.0])
        49.211119459053322
    """
    cdef float total = 0.0
    for i, cashflow in enumerate(cashflows):
        total += cashflow / (1 + rate)**i
    return total

def irr(cashflows):
    """The IRR or Internal Rate of Return is the annualized effective 
       compounded return rate which can be earned on the invested 
       capital, i.e., the yield on the investment.
       
       >>> irr([-100.0, 60.0, 60.0, 60.0])
       0.36309653947517645

    """
    cdef float rate = 1.0
    cdef int iterations = 100
    cdef float investment = cashflows[0]
    for i in range(1, iterations+1):
        rate *= (1 - npv(rate, cashflows) / investment)
    return rate




