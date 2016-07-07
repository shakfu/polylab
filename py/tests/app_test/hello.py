

class Person(object):
    def __init__(self, id):
        self.id = id
    def __str__(self):
        return str(self.id)
    def loves(self, other):
        return '%s loves %s' % (self.id, other.id)
    def talk(self):
        return self.id



def get_person(id='sam'):
    p = Person(id)
    return p


def test_person_id():
    p = get_person()
    assert p.id == 'sam'

def test_person_love():
    sam = get_person()
    ava = get_person('ava')
    assert sam.loves(ava) == 'sam loves ava'

def test_person_notlove():
    sam = get_person()
    sue = get_person('sue')
    assert not sam.loves(sue) == 'sam loves ava'

def test_person_talk():
    sam = get_person()
    assert sam.talk() == sam.id

if __name__ == '__main__':
    test_person_id()
    test_person_love()
    test_person_notlove()
    test_person_talk()
    print 'ok'


