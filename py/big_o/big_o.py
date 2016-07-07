# big_o.py


from math import log, factorial
from random import random

def test(name, f):
    for n in range(1, 1000, 100):
        try:
            print "%s: f(%s) = %s" % (name, n, round(f(n),0))
        except (ValueError, OverflowError), e:
            print 'math domain or overflow error'

def logstar(n):
    if n <= 1: return 0
    if n > 1:
        return 1 + logstar(log(n))

test("O(log log n)", lambda n: log(log(n)))
test("O(log n)", lambda n: log(n))
test("O(n**c), 0<c<1", lambda n: n**0.5)
test("O(n)", lambda n: n)
test("O(n log* n)", logstar)
test("O(n log n)", lambda n: n * log(n))
test("O(n**2)", lambda n: n**2)
test("O(n**c), c>1", lambda n: n**3)
test("O(n!)", lambda n: factorial(n))