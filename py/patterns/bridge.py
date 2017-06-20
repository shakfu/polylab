class IDrawingAPI(object):
    def draw_circle(x, y, radius):
        "draw circle"

class DrawingAPI(IDrawingAPI):
    def draw_circle(self, x, y, radius):
        print x, y, radius


class DrawingAPI2(IDrawingAPI):
    def draw_circle(self, x, y, radius):
        print x, y, radius


class IShape(object):
    def draw():
        "draw interface"
    def resize(percent):
        "resize method"


class CircleShape(IShape):
    def __init__(self, x, y, radius, api):
        self.x = x
        self.y = y
        self.radius = radius
        self.api = api

    def draw(self):
        self.api.draw_circle(self.x, self.y, self.radius)

    def resize(self, percent):
        self.radius *= percent

if __name__ == '__main__':
    shapes = [
        CircleShape(1, 2, 3, DrawingAPI()),
        CircleShape(5, 7, 11, DrawingAPI2())
    ]
    for shape in shapes:
        shape.resize(2.5)
        shape.draw()


