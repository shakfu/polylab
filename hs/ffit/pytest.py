from ctypes import *
lib = cdll.LoadLibrary("./Test.so")
print lib.hsfun(5)

