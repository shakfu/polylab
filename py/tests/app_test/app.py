

class Person(object):
    def __init__(self, id):
        self.id = id
    def __str__(self):
        return str(self.id)
    def loves(self, other):
        return '%s loves %s' % (self.id, other.id)
    def talk(self):
        return self.id


