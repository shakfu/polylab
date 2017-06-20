

class Object(object):
    cost = 0
    def __call__(self, cost=None):
        if not cost:
            return self.cost
        else:
            return cost + self.cost

class Coffee(Object):
    cost = 1

class Milk(Object): 
    cost = 0.5

class Whip(Object):
    cost = 0.7

class Sprinkles(Object):
    cost = 0.2


coffee, milk, whip, sprinkles = Coffee(), Milk(), Whip(), Sprinkles()
print sprinkles(whip(milk(coffee())))

