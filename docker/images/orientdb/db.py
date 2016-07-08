import pyorient

document = {
    'id': 1012,
    'process' : {
        'name': 'tack_weld',
        'operand': 'pipe',
        'is_nc': True
    },
    'operation': {
        'id': 100,
        'start': 121001,
        'end': 1211200,
    }
}



class DB(object):
    def __init__(self, db, user, password):
        self.db = db
        self.client = pyorient.OrientDB("localhost", 2424)
        self.session = self.client.connect(user, password)
        self.user = user
        try:
            self.create()
        except:
            pass
        self.open(password)

    def create(self):
        self.client.db_create(
            self.db,
            pyorient.DB_TYPE_GRAPH,
            pyorient.STORAGE_TYPE_MEMORY
        )
        assert self.exists()

    def open(self, password):
        self.client.db_open(self.db, self.user, password)

    def exists(self):
        return self.client.db_exists(self.db,
            pyorient.STORAGE_TYPE_MEMORY
        )

    def list(self):
        return self.client.db_list()

    def size(self):
        return self.client.db_size()

    def count(self):
        return self.client.db_count_records()

    def cmd(self, s):
        return self.client.command(s)

    def query(self, *args):
        return self.client.query(*args)

    def add(self, klass, cid, **kwds):
        classname = '@'+klass
        record = { classname: kwds }
        return self.client.record_create(cid, record)


db = DB('factory', user='user', password='pass')

# add people
people = db.cmd('create class Person extends V')[0]
db.add('Person', people, name='jon', age=10)
db.add('Person', people, name='sue', age=20)

# add edges
#likes = db.cmd('create class Likes extends E')[0]
#db.cmd("create edge Likes from (select Person where name='jon') to (select Person where name='sue')")



