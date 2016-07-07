# shoelace.pyx

# Area of Polygon using Shoelace formula
# http://en.wikipedia.org/wiki/Shoelace_formula
# FB - 20120218
# corners must be ordered in clockwise or counter-clockwise direction
import time

cdef extern from "shoelace.h":
    double** polygon_create(int n)
    void polygon_destroy(int n, double** matrix)
    void polygon_angles(int n, double** matrix)
    void polygon_print(int n, double** matrix)
    double polygon_area(int n, double **matrix)
    double** polygon_from_file(int n_points, char* path)
    void polygon_area_from_file(int n_points, char* path)

cpdef enum State:
    OPEN   = 1
    CLOSED = 2
    WAIT   = 3
    ENDED  = 4

cdef class Polygon:
    cdef double** _matrix
    cdef public int n_points

    def __init__(self, list corners):
        self.n_points = len(corners)
        self._matrix = polygon_create(self.n_points)
        for i, (x, y) in enumerate(corners):
            self._matrix[i][0] = x
            self._matrix[i][1] = y
        polygon_angles(self.n_points, self._matrix)

    def __repr__(self):
        return "<Polygon: n:'%i'>" % self.n_points

    def __dealloc__(self):
        if self._matrix is not NULL:
            polygon_destroy(self.n_points, self._matrix)

    property area:
        def __get__(self):
            return polygon_area(self.n_points, self._matrix)

    property corners:
        def __get__(self):
            points = []
            m = self._matrix
            for i in range(self.n_points):
                points.append((m[i][0], m[i][1], m[i][2]))
            return points

    @classmethod
    def from_file(cls, path):
        points = []
        with file(path) as f:
            for line in f.readlines():
                x, y = line.strip().split()
                points.append((float(x), float(y)))
        return cls(points)

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

def bench():
    polygon = diag_gen(2*10**5)
    p = Polygon(polygon)
    t1 = time.time()
    print p.area
    print time.time() - t1, 'seconds'

