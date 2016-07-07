
class Person(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def say(self):
        print '%s %s' % (self.name, self.age)

    def add(self, x, y):
        return x+y

class Employee(Person):
    def laugh(self):
        print 'ha ha'


if __name__ == '__main__':
    e = Employee('sam', 10)
    e.say()
    e.laugh()
    print e.add(1, 2)

