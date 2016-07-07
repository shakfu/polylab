import pyximport; pyximport.install()
from finance import *
import numpy.lib.financial as numpy

def display(func, check):
    print "=>", func
    print "=>", check
    print

cashflows = [-100.0, 60.0, 60.0, 60.0] 

tests = [
    ('irr(cashflows)', "0.363096539475"),
    ('npv(0.1, cashflows)', "49.2111194591"),
    ('payback(cashflows)', "1.66666666667"),
]

for func, check in tests:
    print func
    result = eval(func)
    display(result, check)


