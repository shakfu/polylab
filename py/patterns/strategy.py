
class IStrategy(object):
    def execute(a, b):
        "the context uses this to call concrete strategy"

class Add(IStrategy):
    def execute(self, a, b):
        return a + b

class Subtract(IStrategy):
    def execute(self, a, b):
        return a - b

class Multiply(IStrategy):
    def execute(self, a, b):
        return a * b


class Context(object):
    def __init__(self, strategy):
        self.strategy = strategy

    def execute(self, a, b):
        return self.strategy.execute(a, b)


if __name__ == '__main__':
    add = Add()
    subtract = Subtract()
    multiply = Multiply()
    
    context = Context(add)
    print context.execute(1,2)

    context = Context(multiply)
    print context.execute(2,2)


