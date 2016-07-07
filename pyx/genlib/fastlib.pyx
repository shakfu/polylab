
ctypedef int size_t
ctypedef char* string

cdef extern from "funclib.h":
    void println(char *string)
    int findMin(int d1, int d2, int d3)
    int findEditDistance(char *s1, char *s2)
    double add(double x, double y)

#cdef extern from "stdio.h":
    #ctypedef FILE* osfile
    #osfile fopen (string filename, string opentype)

cdef extern from "string.h":
    size_t strlen (char *s) 
    string strerror (int errnum)

cdef extern from "errno.h":
    int EPERM
    int ENOENT

cdef struct Person:
    string name
    int age

def test_person():
    cdef Person p
    p.name = "hello"
    p.age = 10
    return p.name

cdef enum State:
    love, hate, sorrow

def csum(list array):
    cdef int i, N=len(array)
    cdef double x, s=0.0
    for i in range(N):
        x = array[i]
        s += x
    return s


def pyprintln(s):
    println(s)

def find_min(x, y, z):
    return findMin(x,y,z)

def distance(s1, s2):
    return findEditDistance(s1, s2) 

def length(text):
    return strlen(text)

def test():
    print 'findmin:', find_min(1,2,3)
    print 'editdist:', distance("Hello", "My friend")
    print 'add:', add(10, 32)
    print 'csum:', csum(range(10))
    print 'length:', length("Hello World")
    print 'test_person:', test_person()
    print 'enum:', sorrow 
    print 'error.1:', strerror(ENOENT)
    print 'error.2:', strerror(EPERM)

#test()

