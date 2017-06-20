import polygon
import shoelace

def test(corners):
    p = polygon.Polygon(corners)
    print 'corners:', corners
    print 'area:', p.area
    return p

corners = [(2.0, 4.0), (3.0, -8.0), (1.0, 2.0), (2.0, 4.0)]
test(corners)

print "square"
corners = [(1.0, 1.0), (1.0, 4.0), (4.0, 1.0), (4.0, 4.0)]


p = test(corners)

print 'benchmark'
polygon.bench()

s = shoelace.Polygon(corners)

