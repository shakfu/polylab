"""
A demo of cython's capabilities

"""

ctypedef char* string


cdef class Shrubbery:
    cdef public int width, height
    cdef readonly float depth

def mysum(long N):
    cdef long k, s = 0
    for k from 0 <= k < N:
        s += k
    return s

def fib(int n):
    "print the fibonacci series up to n"
    cdef int a, b
    a, b = 0, 1
    while b < n:
        print b,
        a, b = b, a+b

cdef enum otherstuff:
    sausage, eggs, lettuce

cdef struct spamdish:
    int oz_of_spam
    otherstuff filler

cdef void prepare(spamdish *d):
    d.oz_of_spam = 42
    d.filler = sausage


cdef struct Person:
    char* name
    int age

cdef enum PersonState:
    happy = 1
    sad = 2
    angry = 3

cdef void set_person(Person *p):
    p.name = "joe"
    p.age = 10

cdef void print_person():
    cdef Person p
    set_person(&p)
    print p.name


cdef class Party:
    cdef char* name
    cdef int age

    def __init__(self, n, a):
        self.name = n
        self.age = a

    def display(self):
        print "my name is ", self.name, "my age is ", self.age

def color():
    print_person()

def serve():
    cdef spamdish d
    prepare(&d)
    print "%d oz spam, filler no. %d" % (d.oz_of_spam, d.filler) 

def test():
    p = Party('joe', 20)
    p.display()

serve()

