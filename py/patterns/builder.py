from collections import OrderedDict

class Object(object):
    def __init__(self, elements=None, builder=None):
        self.elements = elements if elements else OrderedDict()
        self.builder = builder
    
    def build(self):
        if self.elements and self.builder:
            return self.builder(self.elements)
        if not self.elements and self.builder:
            return self.builder.build()

class OrderedBag(Object):
    def build(self, elements=None):
        if elements and self.builder and not self.elements:
            self.elements = self.builder.build(elements)
        if not elements and self.builder and not self.elements:
            self.elements = self.builder.build()

    def __getattr__(self, name):
        return self.elements[name]
    
    def __setitem__(self, name, value):
        self.elements[name] = value
        
    def __getitem__(self, name):
        return self.elements[name]    

    def __repr__(self):
        return "<{0} {1}>".format(
            self.__class__.__name__,
            self.elements)

elems = [
    ('a', 1),
    ('b', 2),
]

class Builder(object):
    def __init__(self, elements):
        self.elements = elements
        
    def __call__(self, elements=None):
        if elements:
            return self.__class__(elements).build()
        else:
            return self.build()

    def build(self):
        return self.elements

class OrderedDictBuilder(Builder):
    def build(self):
        return OrderedDict(self.elements)

class ListBuilder(Builder):
    def build(self):
        return list(self.elements)

class ObjectBuilder(Builder):
    def build(self):
        return list(self.elements)


if __name__ == '__main__':
    builder = OrderedDictBuilder(elems)
    bag = OrderedBag(builder=builder)
    
