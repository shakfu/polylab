

cdef extern from "math.h":
    double sin(double)

cdef inline int int_min(int a, int b):
    return b if b < a else a


