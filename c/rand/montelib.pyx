cdef extern from "monte.h":
    cdef extern float c_dice "dice" (int N, int ndice, int nsix)

def dice(int N, int ndice, int nsix):
    return c_dice(N, ndice, nsix)

