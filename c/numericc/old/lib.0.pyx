
cdef extern float** c_matrix "matrix" (long nrl, long nrh, long ncl, long nch)


def test(a,b,c,d):
    cdef float** m
    m = c_matrix(a, b, c, d)
    m[1][1] = 1.0
    m[1][2] = 2.0
    m[2][1] = 3.0
    m[2][2] = 4.0
    print m[1][1]
    print 'ok'


def matrix_old(m=[(1,2), (3,4), (5,6)]):
    cdef float** cm
    rows = len(m)    # == 3
    cols = len(m[0]) # == 2
    cm = c_matrix(1, cols, 1, rows)
    cm[1][1] = m[0][0]
    cm[1][2] = m[0][1]
    cm[2][1] = m[1][0]
    cm[2][2] = m[1][1]
    cm[3][1] = m[2][0]
    cm[3][2] = m[2][1]
    print cm[3][2]
    print "ok"

def matrix(m=[(1,2), (3,4), (5,6)]):
    cdef float** cm
    rows = len(m)    # == 3
    cols = len(m[0]) # == 2
    cm = c_matrix(1, cols, 1, rows)
    for row in range(rows):
        for col in range(cols):
            cm[row+1][col+1] = m[row][col]
    print cm[3][2]
    print cm[3][1]
    print "ok"
    
    