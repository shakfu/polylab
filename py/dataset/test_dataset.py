import dataset

db = dataset.connect('postgresql://sa:sa@localhost/sa')

table = db['music']

table.insert(dict(name='sa', age=22, song='misanthrope'))
