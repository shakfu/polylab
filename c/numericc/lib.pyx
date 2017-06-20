cdef extern float** c_matrix "matrix" (long nrl, long nrh, long ncl, long nch)
cdef extern float trapzd(float (*func)(float), float a, float b, int n)

cdef float** matrix(m):
    cdef float** cm
    rows = len(m)    # == 3
    cols = len(m[0]) # == 2
    cm = c_matrix(1, cols, 1, rows)
    #~ cm[1][1] = m[0][0]
    #~ cm[1][2] = m[0][1]
    #~ cm[2][1] = m[1][0]
    #~ cm[2][2] = m[1][1]
    #~ cm[3][1] = m[2][0]
    #~ cm[3][2] = m[2][1]
    for row in range(rows):
        for col in range(cols):
            #print row+1, col+1, "=", row, col
            cm[row+1][col+1] = m[row][col]
    return cm    

def test(m=[(1,2), (3,4), (5,6)]):
    cdef float** cm 
    cm = matrix(m)
    for i in range(1,4):
        print cm[i][1], cm[i][2]
    
    #~ print cm[1][2]
    print "ok"

cdef float func(float x):
    return x+2.0

def trap(a,b,n):
    return trapzd(func, a, b, n)
    