
def get_person(id='sam'):
    from app import Person
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



