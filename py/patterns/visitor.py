class IElementVisitor(object):
    def visit(element):
        "visit an element"

class Element(object):
    def accept(self, visitor):
        "elements have to provide accept"
        visitor.visit(self)

class Wheel(Element):
    def __init__(self, name):
        self.name = name
    

class Engine(Element): pass

class Car(Element):
    def __init__(self):
        self.elements = [
            Wheel("front right"), Wheel("front left"),
            Wheel("back right"),  Wheel("back left"),
            Engine()
        ]
    
    def accept(self, visitor):
        for element in self.elements:
            element.accept(visitor)
        visitor.visit(self)

class PrintVisitor(IElementVisitor):
    def visit_wheel(self, wheel):
        print 'visiting {0} wheel'.format(wheel.name)
    def visit_engine(self, engine):
        print 'visiting engine'
    def visit_car(self, car):
        print 'visiting car'

    def visit(self, obj):
        {    Wheel: self.visit_wheel,
            Engine: self.visit_engine,
               Car: self.visit_car,
        }[type(obj)](obj)



if __name__ == '__main__':
    car = Car()
    car.accept(PrintVisitor())

