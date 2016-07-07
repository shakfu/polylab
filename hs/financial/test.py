from ctypes import *
lib = cdll.LoadLibrary("./financial.so")
lib.pmt.restype = c_double
lib.pmt.argtypes = (c_double, c_double)
print lib.pmt(1.0, 2.0)





