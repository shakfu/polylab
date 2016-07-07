# shoelace.py

# Area of Polygon using Shoelace formula
# http://en.wikipedia.org/wiki/Shoelace_formula
# FB - 20120218
# corners must be ordered in clockwise or counter-clockwise direction
import math
import time

class Polygon(object):
    def __init__(self, corners):
        self.corners = self.sort(corners)

    def area(self):
        n = len(self.corners) # of corners
        _area = 0.0
        for i in range(n):
            j = (i + 1) % n
            _area += self.corners[i][0] * self.corners[j][1]
            _area -= self.corners[j][0] * self.corners[i][1]
        _area = abs(_area) / 2.0
        return _area

    # sort corners in counter-clockwise direction
    def sort(self, corners):
        # calculate centroid of the polygon
        n = len(corners) # of corners
        cx = float(sum(x for x, y in corners)) / n
        cy = float(sum(y for x, y in corners)) / n
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

def test(corners):
    print 'corners:', corners
    print 'area:', Polygon(corners).area()

# examples
corners = [(2.0, 1.0), (4.0, 5.0), (7.0, 8.0)]
test(corners)

corners = [(3.0, 4.0), (5.0, 11.0), (12.0, 8.0), (9.0, 5.0), (5.0, 6.0)]
test(corners)


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

def test_diag_gen(points):
    res = 0
    for point in points:
        assert abs(point[0] + point[1] - res) <= 1
        res = point[0] + point[1]
    assert points[0] == points[-1]

# test_diag_gen(diag_gen(200))

def area(points):
    acc = 0
    for i in range(len(points) - 1):
        acc += points[i][0] * points[i + 1][1] - points[i + 1][0] * points[i][1]
    return abs(acc) / 2

#assert area(diag_gen(32)) == 64.0

def timeit(f):
    t1 = time.time()
    print f()
    print time.time() - t1, 'seconds'


def bench():

    polygon = diag_gen(2*10**5)
    timeit(lambda: area(polygon))

    timeit(Polygon(polygon).area)


def dump():
    polygon = diag_gen(2*10**5)
    with file('points.txt', 'w') as f:
        for x, y in polygon:
            f.write("%s %s\n" % (x,y))

#dump()
bench()





