
import os, sys
from os.path import basename, abspath, join, splitext, exists
from vedis import Vedis
import hashlib
from logbook import Logger

BYTES = 512
TRANSACTIONS = 1000
DB = 'vedis.store'
RAMDISK = '/tmp/ramdisk'

log = Logger('keystore')


def cmd(s, *args, **kwds):
    _cmd = s.format(*args, **kwds)
    log.info(_cmd)
    os.system(_cmd)


def mk_ramdisk():
    cmd('sudo mkdir ' + RAMDISK)
    cmd('sudo chmod 777 ' + RAMDISK)
    cmd('sudo mount -t tmpfs -o size=256M tmpfs ' + RAMDISK) 


class SecureStore(object):
    def __init__(self, db='vedis.store', ramdisk=True):
        if ramdisk:
            self.data_dir = '/tmp/ramdisk'
        else:
            self.data_dir = abspath('.')
        self.db = join(self.data_dir, db)
        self.basename = basename(self.db)
        self.name = splitext(self.basename)[0]
        self.archive_name = self.name + '.bin'
        self.archive = join(self.data_dir, self.archive_name)
        self.mem = None

    def compress(self, password):
        cmd('7za -p{} a {} {}', password, self.archive, self.db)

    def extract(self, password):
        cmd('7za -p{} x {} -o{}', password, self.archive, self.data_dir)

    def mk_key(self):
        k = os.urandom(BYTES)
        d = hashlib.sha512(k)
        return d.hexdigest()

    def create_db(self):
        db = Vedis(self.db)
        db['current'] = 0
        for i in range(TRANSACTIONS):
            db[i] = self.mk_key()
        db.close()

    def create_archive_key(self):
        with file('key.file', 'w') as f:
            key = self.mk_key()
            f.write(key)
        return key

    def get_archive_key(self):
        with file('key.file') as f:
            key = f.read()
        return key

    def memstore(self):
        db = Vedis(self.db)
        mem = Vedis(':mem:')
        mem['current'] = db['current']
        for i in range(TRANSACTIONS):
            mem[i] = db[i]
        db.close()
        self.shred(self.db)
        self.mem = mem

    def shred(self, path):
        cmd('shred --remove --zero {}', path)

    def process(self):
        self.create_db()
        key = self.create_archive_key()
        self.compress(key)
        self.shred(self.db)

        key = self.get_archive_key()
        self.extract(key)
        self.memstore()
        self.mem.incr('current')


if __name__ == '__main__':
    if len(sys.argv) > 1:
        mk_ramdisk()
    else:
        store = SecureStore()
        store.process()
