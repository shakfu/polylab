from ctypes import *
lib = cdll.LoadLibrary("./Test.so")
lib.hsfun(5)
