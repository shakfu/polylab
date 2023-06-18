# distutils: language = c++
from oscillators cimport Sine

def main():
  cdef Sine[float] sine
  sine.setFrequency(220., 44100.)
  for i in range(100):
    print(f"{i} {sine.getSample()}")
  # s_ptr = new Sine[float]()
  # del s_ptr


