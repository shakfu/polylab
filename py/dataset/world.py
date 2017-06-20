import dataset
from collections import OrderedDict as odict

db = dataset.connect('postgresql://sa:sa@localhost/sa')

table = db['characters']

characters = [
    odict([('name', 'sam'), ('age', 22), ('character', 'misanthropic')]),
    odict([('name', 'jon'), ('age', 21), ('character', 'happy')]),
    odict([('name', 'lou'), ('age', 23), ('character', 'sa')]),
]

for character in characters:
    table.insert(character)
