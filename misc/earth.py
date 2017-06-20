# round.py
from math import sqrt, pi

EARTH_RADIUS = 6371.
SECONDS_IN_DAY = 60. * 60. * 24.

def distance_to_horizon(height):
    '''
    calculates distance to horizon in kilometers

    if 
        R = radius of the earth
        h = elevation in meters above ocean surface
        D = distance to horizon

    by pythagoras' theorom

        (R + h)**2 = D**2 + R**2

    which reduces to
        
        R**2 + 2Rh + h**2 = D**2 + R**2

        h(2R + h) = D**2

    since diameter 2R of earth is much bigger than h, therefore
    the error introduced if (2R + h) is replaced by 2R is very small
    gives:

        2Rh = D**2

    '''
    return sqrt(2*EARTH_RADIUS*height)

def solve(seconds, h1, h2):
    return ((distance_to_horizon(h1) - distance_to_horizon(h2))
             / 2 * pi * EARTH_RADIUS ) * SECONDS_IN_DAY




if __name__ == '__main__':
    for i in range(1, 5):
        print "{} km up the distance_to_horizon is {} km".format(
            i, 
            distance_to_horizon(i))
