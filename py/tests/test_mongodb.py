
import pymongo
from mongokit import MongoDocument

def test_pymongo():

    con = pymongo.Connection('localhost', 27017)

    db = con.test

    db.name()
    db.my_collection
    db.my_collection.save({'x':10})
    db.my_collection.save({'person':20032})
    db.my_collection.find()

    for i in db.my_collection.find():
        print i



class Person(MongoDocument):
    db_name = 'test'
    collection_name = 'tutorial'
    structure = {
        'title': unicode,
        'body': unicode,
        'age': int,
    }
    required_fields = ['title', 'body', 'age']
    default_values = {'age':10}
    use_dot_notation = True
        
def test_mongokit():

    person = Person()

    person['title'] = u'Sam Spade'
    person['body'] = u'a body'
    person['age'] = 210
    person.validate()
    person.save()

    for p in Person.all():
        print p

    for p in Person.all():
        print p['title']


