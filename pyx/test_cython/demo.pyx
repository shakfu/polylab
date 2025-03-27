cdef class Foo:
    cdef int _x

    def __cinit__(self, x):
        self._x = x

    property x:
        def __get__(self):
            return self._x


cdef int _add(int x, int y):
    return x + y

def add(x: int, y: int) -> int:
    return _add(x,y)

