
cdef extern from "mlib.h":
    int m_add(int x, int y)
    int m_sub(int x, int y)
    float m_div(float x, float y)
    int m_mul(int x, int y)
    int m_pow(int x, int y)

def add(x, y):
    return m_add(x, y)

ctypedef fused floating:
    float
    double

cdef struct Point:
    double x, y
    int color

cdef class Employee:
    cpdef str name
    cdef int age
    cdef float cash

    def __init__(self, name, age, cash):
        self.name = name
        self.age = age
        self.cash = cash

    def __repr__(self):
        return "<Employee: %s>" % self.name

    cpdef int add(self, int x, int y):
        return x + y

    def sub(self, int x, int y):
        return m_sub(x, y)

    cdef int _mul(self, int x, int y):
        return m_mul(x, y)

    def mul(self, int x, int y):
        return self._mul(x, y)

    def point(self, double x, double y, int color=10):
        cdef Point p
        p = Point(x=x, y=y, color=color)
        print p.x, p.y, p.color
        return p
