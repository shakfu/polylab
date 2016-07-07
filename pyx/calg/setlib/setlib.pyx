cdef extern from "libcalg.h":
    unsigned long string_hash(void *string)
    int string_equal(void *string1, void *string2)
    #Set *set_new(SetHashFunc hash_func, SetEqualFunc equal_func)

cdef class Person:
    cdef char* name
    cdef int age

    def __init__(self, name, age):
        self.name = name
        self.age = age

    def display(self):
        print "my name is", self.name, "and my age is", self.age


def test():
    p = Person('joe', 20)
    p.display()

