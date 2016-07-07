# functions
"""
a cython functionalist extravaganzum

"""

import math

def sum(long n):
    """a summing bird
    """
    cdef long i, s = 0
    for i in range(n):
        s += i
    return s

cdef class Function:
    cpdef double evaluate(self, double x) except *:
        return 0

cdef class SinOfSquareFunction(Function):
    cpdef double evaluate(self, double x) except *:
        return math.sin(x**2)

cdef class CosFunction(Function):
    cpdef double evaluate(self, double x) except *:
        return math.cos(x)

def integrate(Function f, double a, double b, int N):
    cdef int i
    cdef double s, dx
    if f is None:
        raise ValueError("f cannot be None")
    s = 0
    dx = (b-a)/N
    for i in range(N):
        s += f.evaluate(a+i*dx)
    return s * dx

def test():
    print(integrate(SinOfSquareFunction(), 0, 1, 10000))
    print(integrate(CosFunction(), 0, 1, 10000))


