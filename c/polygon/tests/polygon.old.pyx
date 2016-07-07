# shoelace.pyx

# Area of Polygon using Shoelace formula
# http://en.wikipedia.org/wiki/Shoelace_formula
# FB - 20120218
# corners must be ordered in clockwise or counter-clockwise direction
import math
import time

cdef extern from "shoelace.h":
    double** polygon_create(int n)
    void polygon_destroy(int n, double** matrix)
    void polygon_angles(int n, double** matrix)
    void polygon_print(int n, double** matrix)
    double polygon_area(int n, double **matrix)
    double** polygon_from_file(int n_points, char* path)
    void polygon_area_from_file(int n_points, char* path)


cdef class Polygon:
    cdef double** _matrix
    cdef public int n_points
    cdef list corners

    def __init__(self, list corners):
        #self.corners = self.sort(corners)
        self.corners = corners
        self.n_points = len(corners)
        self._matrix = polygon_create(self.n_points)
        #for i, (x, y, angle) in enumerate(self.corners):
        for i, (x, y) in enumerate(self.corners):
            self._matrix[i][0] = x
            self._matrix[i][1] = y
        polygon_angles(self.n_points, self._matrix)

    def __repr__(self):
        return "<Polygon: %s>" % id(self)

    def __dealloc__(self):
        if self._matrix is not NULL:
            polygon_destroy(self.n_points, self._matrix)

    def area(self):
        return polygon_area(self.n_points, self._matrix)

    # sort corners in counter-clockwise direction
    def sort(self, corners):
        # calculate centroid of the polygon
        cdef int n = len(corners) # of corners
        cdef double cx = float(sum(x for x, y in corners)) / n
        cdef double cy = float(sum(y for x, y in corners)) / n
        # create a new list of corners which includes angles
        cornersWithAngles = []
        for x, y in corners:
            dx = x - cx
            dy = y - cy
            an = (math.atan2(dy, dx) + 2.0 * math.pi) % (2.0 * math.pi)
            cornersWithAngles.append((dx, dy, an))
        # sort it using the angles
        cornersWithAngles.sort(key = lambda tup: tup[2])
        return cornersWithAngles

    @staticmethod
    def from_file(path, n_points):
        polygon_area_from_file(path, n_points)

def diag_gen(length):
    points = [(0.25, 0.25)]
    for _ in range(1, length + 1):
        points.append((points[-1][0] + 1, points[-1][1]))
        points.append((points[-1][0], points[-1][1] + 1))
    points.append((points[-1][0], points[-1][1] + 1))
    points.append((points[-1][0] - 1, points[-1][1]))
    points.append((points[-1][0], points[-1][1] - 1))
    for _ in range(1, length):
        points.append((points[-1][0] - 1, points[-1][1]))
        points.append((points[-1][0], points[-1][1] - 1))
    points.append((0.25, 0.25))
    return points

def timeit(f):
    t1 = time.time()
    print f()
    print time.time() - t1, 'seconds'


def bench():
    polygon = diag_gen(2*10**5)
    timeit(Polygon(polygon).area)

