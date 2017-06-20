
import os
from vedis import Vedis
import hashlib


BYTES = 512
TRANSACTIONS = 1000
RAMDISK = '/tmp/ramdisk'
DB = 'vedis.store'


def mk_ramdisk():
    cmd('sudo mkdir ' + RAMDISK)
    cmd('sudo chmod 777 ' + RAMDISK)
    cmd('sudo mount -t tmpfs -o size=256M tmpfs ' + RAMDISK) 


def archive_name(path):
    base = os.path.basename(path)
    name = os.path.splitext(base)[0]
    archive = name + '.bin'
    return archive

def compress(path, password):
    base = os.path.basename(path)
    name = os.path.splitext(base)[0]
    archive = name + '.bin'

    os.system('7za -p{} a {} {}'.format(
        password, archive, base
    ))
    return archive

def extract(path, password):
    os.system('7za -p{} x {}'.format(
        password, path
    ))    

def mk_key():
    k = os.urandom(BYTES)
    d = hashlib.sha512(k)
    return d.hexdigest()

def create_db():
    db = Vedis(DB)
    db['current'] = 0

    for i in range(TRANSACTIONS):
        db[i] = mk_key()

    db.close()

def create_archive_key():
    with file('key.file', 'w') as f:
        key = mk_key()
        f.write(key)
    return key

def get_archive_key():
    with file('key.file') as f:
        key = f.read()
    return key

def memstore(dbpath):
    db = Vedis(dbpath)
    mem = Vedis(':mem:')
    mem['current'] = db['current']
    for i in range(TRANSACTIONS):
        mem[i] = db[i]
    db.close()
    os.remove(dbpath)
    return mem

def shred(path):
    os.system('shred --remove --zero {}'.format(path))

# create it from scratch
create_db()

# compress it and encrypt it
key = create_archive_key()
archive = compress(DB, key)
shred(DB)

# decompress it and decrypt it
archive = archive_name(DB)
key = get_archive_key()
extract(archive, key)
mem = memstore(DB)

