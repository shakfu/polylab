from ctypes import *

calc = CDLL('./libcalc.dylib')

calc.add.argtypes = [c_int, c_int]

calc.add.restype = c_int

add = calc.add

print(add(10,20))

