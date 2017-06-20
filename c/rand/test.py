import time
import random
import montelib


def pydice(N, ndice, nsix):
    M = 0                     # no of successful events
    for i in range(N):        # repeat N experiments
        six = 0               # how many dice with six eyes?
        for j in range(ndice):
            r = random.randint(1, 6)  # roll die no. j
            if r == 6:
               six += 1
        if six >= nsix:       # successful event?
            M += 1
    p = float(M)/N
    return p
    

if __name__ == '__main__':
    N, ndice, nsix = 1000000, 10, 5
    ctime, pytime = 0.0, 0.0
    
    print "N:", N, "ndice:", ndice, "nsix:", nsix
    print
    t0 = time.clock()
    p = montelib.dice(N, ndice, nsix)
    t1 = time.clock()
    pytime = t1-t0
    print "CPU time in python:", pytime
    print
    t0 = time.clock()
    p = pydice(N, ndice, nsix)
    t1 = time.clock()
    ctime = t1-t0    
    print "CPU time in cython:", ctime
    
    print
    
    print "python version", round(ctime/pytime), "times slower" 
