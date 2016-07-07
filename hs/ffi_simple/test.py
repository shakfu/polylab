from ctypes import *
lib = cdll.LoadLibrary("./simple.so")

def specify(func, restype, argtypes):
    func.restype = restype
    func.argtypes = argtypes
    return func

pmt = specify(lib.pmt, c_float, (c_float, c_float))

print pmt(1.0, 2.0)



