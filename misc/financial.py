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

import sys, locale

def payback_of_investment(investment, cashflows):
    """The payback period refers to the length of time required 
       for an investment to have its initial cost recovered.
       
       >>> payback_of_investment(200.0, [60.0, 60.0, 70.0, 90.0])
       3.1111111111111112
    """
    total, years, cumulative = 0.0, 0, []
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
    investment, cashflows = cashflows[0], cashflows[1:]
    if investment < 0 : investment = -investment
    return payback_of_investment(investment, cashflows)

def npv(rate, cashflows):
    """The total present value of a time series of cash flows.
    
        >>> npv(0.1, [-100.0, 60.0, 60.0, 60.0])
        49.211119459053322
    """
    total = 0.0
    for i, cashflow in enumerate(cashflows):
        total += cashflow / (1 + rate)**i
    return total

def irr(cashflows, iterations=100):
    """The IRR or Internal Rate of Return is the annualized effective 
       compounded return rate which can be earned on the invested 
       capital, i.e., the yield on the investment.
       
       >>> irr([-100.0, 60.0, 60.0, 60.0])
       0.36309653947517645

    """
    rate = 1.0
    investment = cashflows[0]
    for i in range(1, iterations+1):
        rate *= ( 1 - npv(rate, cashflows) / investment)
    return rate

def lease_price(capital, lifetime, maintenance, operations, target_irr=0.1):
    annual_maint = maintenance * capital / lifetime
    annual_ops = operations * capital / lifetime
    depreciation = capital / lifetime
    annual_cost = -sum([annual_maint, annual_ops, depreciation])
    outflows = [-capital] + [annual_cost]*(lifetime-1)
    guess = capital / 5
    print 'guess:', guess/12 
    def estimate(x):
        inflows = [0] + [x] * (lifetime-1)
        cashflows = [x+y for x,y in zip(outflows,inflows)]
        return cashflows
    print 'irr:', irr(estimate(guess))
    while irr(estimate(guess)) < target_irr:
        guess += 100
    print irr(estimate(guess))
    return guess/12

def mirr(reinvest_rate, finance_rate, cashflows):
    """The modified internal rate of return assumes all positive cashflows
       are re-invested (usually at the WACC) for the remaining duration of 
       the project. All negative cash flows are inflated at the finance rate
       to take account of the cost of funds and included in the initial
       investment outlay.
       
       >>> mirr(0.05, 0.1, [-100.0, 60.0, 60.0, 60.0])
       0.256974347740387       
    """
    n = float(len(cashflows))
    positives = map((lambda x: x if x > 0 else 0.0), cashflows)
    negatives = map((lambda x: x if x < 0 else 0.0), cashflows)
    a = -npv(reinvest_rate, positives) * ((1 + reinvest_rate)**n) 
    b = npv(finance_rate, negatives) * (1 + finance_rate) 
    c = (1.0 / (n-1.0))
    return (a/b)**c - 1.0

# enable placing commas in thousands
locale.setlocale(locale.LC_ALL, "")
# convenience function to place commas in thousands
format = lambda x: locale.format('%d', x, True)

def investment_analysis(discount_rate, cashflows):
    """Provides summary investment analysis on a list of cashflows
       and a discount_rate.
       
       Assumes that the first element of the list (i.e. at period 0) 
       is the initial investment with a negative float value.
    """
    _npv = npv(discount_rate, cashflows)
    ts = [('year', 'cashflow')] + [(str(x), format(y)) for (x,y) in zip(
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
    print "    NPV: %s" % format(_npv)
    print 
    print "==> %s investment of %s" % (
        ("Approve" if _npv > 0 else "Do Not Approve"), format(-cashflows[0]))
    print "-" * 70

def main(inputs):
    """commandline entry point
    """
    
    usage = '''Provides analysis of an investment and a series of cashflows.
    
    usage: invest discount_rate [cashflow0, cashflow1, ..., cashflowN]
        where 
            discount_rate is the rate used to discount future cashflows 
                             to their present values
            cashflow0 is the investment amount (always a negative value)
            cashflow1 .. cashflowN values can be positive (net inflows)
                                                 or
                                                 negative (net outflows)
    for example:
        invest 0.05 -10000 6000 6000 6000
    '''
    
    try:
        rate, cashflows = inputs[0], inputs[1:]
        investment_analysis(float(rate), [float(c) for c in cashflows])
    except IndexError:
        print usage
        sys.exit()

if __name__ == '__main__':
    # print "true", 0.256974347740387
    # print "mirr:", mirr(0.05, 0.1, [-100.0, 60.0, 60.0, 60.0])
    print lease_price(100000, 5, 0.3, 0.1)
    # debug = True
    # if debug:
    #     import doctest
    #     doctest.testmod()
    # else:
    #     main(sys.argv[1:])
