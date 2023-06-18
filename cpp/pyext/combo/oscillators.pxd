
cdef extern from "./choc/audio/choc_Oscillators.h" namespace "choc::oscillator":
    cdef cppclass Sine[FloatType]:
        Sine() except +
        void resetPhase()
        void setFrequency (FloatType frequency, FloatType sampleRate)
        FloatType getSample()
