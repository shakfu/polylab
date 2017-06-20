import secureconfig
from secureconfig import SecureConfig

TEST_INI = 'seconf.ini'
KEYFILE  = 'my.key'

def get_key():
    with file(KEYFILE) as f:
        key = f.read()
    return key

def test_configparser():

    key = secureconfig.cryptkeeper.CryptKeeper.generate_key()

    scp = secureconfig.SecureConfig.from_key(key)

    scp.add_section('credentials')
    scp.set('credentials', 'user', 'sa')
    scp.set('credentials', 'password', 'sa')

    with file(TEXT_INI, 'w') as f:
        scp.write(f)

    with file(KEYFILE, 'w') as f:
        f.write(key)


    #key = file(keyfile).read()
    # #scp = secureconfig.SecureConfigParser.from_file(keyfile)
    scp = secureconfig.SecureConfig.from_key(key, filepath=TEST_INI)

    print 'username', scp.get('credentials', 'user')
    print 'password', scp.get('credentials', 'password')


def test_configparser():
    pass

key = secureconfig.cryptkeeper.CryptKeeper.generate_key()
#key = get_key()
ck = secureconfig.cryptkeeper.CryptKeeper(key=key)
scp = secureconfig.SecureConfigParser(ck=ck)
#scp.read(TEST_INI)
scp.add_section('credentials')
scp.set('credentials', 'user', 'sa')
scp.set('credentials', 'password', 'sa', encrypt=True)
with file(TEST_INI, 'w') as f:
    scp.write(f)
with file(KEYFILE, 'w') as f:
    f.write(key)

ck = secureconfig.cryptkeeper.CryptKeeper(key=key)
scp = secureconfig.SecureConfigParser(ck=ck)
scp.read(TEST_INI)

print 'username', scp.get('credentials', 'user')
print 'password', scp.get('credentials', 'password')
