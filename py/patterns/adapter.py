
class Creature(object):
    def __init__(self, name):
        self.name = name

class Person(Creature):
    def make_noise(self):
        return "hello"

class Dog(Creature):
    def bark(self):
        return "woof"

class Cat(Creature):
    def meow(self):
        return "meow"


class CreatureAdapter(object):
    def __init__(self, creature, make_noise):
        self.creature = creature
        self.make_noise = make_noise

    def __getattr__(self, attr):
        return getattr(self.creature, attr)


if __name__ == '__main__':
    person = Person('bob')

    fido = Dog('fido')
    canine = CreatureAdapter(fido, fido.bark)

    whiskers = Cat('whiskers')
    feline = CreatureAdapter(whiskers, whiskers.meow)

    for i in [person, canine, feline]:
        print i.name, 'says', i.make_noise()
