
# from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/111286
BASE2 = '01'
BASE10 = '0123456789'
BASE16 = '0123456789ABCDEF'
BASE62 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'
BASE70 = "!'()*-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"


def baseconvert(number, fromdigits, todigits):
    """converts a "number" between two bases of arbitrary digits.

    The input number is assumed to be a string of digits from the
    fromdigits string (which is in order of smallest to largest
    digit). The return value is a string of elements from todigits
    (ordered in the same way). The input and output bases are
    determined from the lengths of the digit strings. Negative
    signs are passed through.

    decimal to binary
    >>> baseconvert(555, BASE10, BASE2)
    '1000101011'

    binary to decimal
    >>> baseconvert('1000101011', BASE2, BASE10)
    '555'

    integer interpreted as binary and converted to decimal (!)
    >>> baseconvert(1000101011, BASE2, BASE10)
    '555'

    base10 to base4
    >>> baseconvert(99, BASE10, "0123")
    '1203'

    base4 to base5 (with alphabetic digits)
    >>> baseconvert(1203, "0123", "abcde")
    'dee'

    base5, alpha digits back to base 10
    >>> baseconvert('dee', "abcde", BASE10)
    '99'

    decimal to a base that uses A-Z0-9a-z for its digits
    >>> baseconvert(257938572394L,BASE10,BASE62)
    'E78Lxik'

    ..convert back
    >>> baseconvert('E78Lxik', BASE62, BASE10)
    '257938572394'

    binary to a base with words for digits (the function cannot convert this back)
    >>> baseconvert('1101', BASE2,('Zero','One'))
    'OneOneZeroOne'

    """

    if str(number)[0] == '-':
        number = str(number)[1:]
        neg = 1
    else:
        neg = 0

    # make an integer out of the number
    x = long(0)
    for digit in str(number):
        x = x * len(fromdigits) + fromdigits.index(digit)

    # create the result in base 'len(todigits)'
    res = ''
    while x > 0:
        digit = x % len(todigits)
        res = todigits[digit] + res
        x /= len(todigits)
    if neg:
        res = '-' + res

    return res


## {{{ http://code.activestate.com/recipes/576980/ (r3)
# PyCrypto-based authenticated symetric encryption
import cPickle as pickle
import hashlib
import hmac
import os
from Crypto.Cipher import AES

class AuthenticationError(Exception): pass

class Crypticle(object):
    """Authenticated encryption class
    
    Encryption algorithm: AES-CBC
    Signing algorithm: HMAC-SHA256
    """

    PICKLE_PAD = "pickle::"
    AES_BLOCK_SIZE = 16
    SIG_SIZE = hashlib.sha256().digest_size

    def __init__(self, key_string, key_size=192):
        self.keys = self.extract_keys(key_string, key_size)
        self.key_size = key_size

    @classmethod
    def generate_key_string(cls, key_size=192):
        key = os.urandom(key_size / 8 + cls.SIG_SIZE)
        return key.encode("base64").replace("\n", "")

    @classmethod
    def extract_keys(cls, key_string, key_size):
        key = key_string.decode("base64")
        assert len(key) == key_size / 8 + cls.SIG_SIZE, "invalid key"
        return key[:-cls.SIG_SIZE], key[-cls.SIG_SIZE:]

    def encrypt(self, data):
        """encrypt data with AES-CBC and sign it with HMAC-SHA256"""
        aes_key, hmac_key = self.keys
        pad = self.AES_BLOCK_SIZE - len(data) % self.AES_BLOCK_SIZE
        data = data + pad * chr(pad)
        iv_bytes = os.urandom(self.AES_BLOCK_SIZE)
        cypher = AES.new(aes_key, AES.MODE_CBC, iv_bytes)
        data = iv_bytes + cypher.encrypt(data)
        sig = hmac.new(hmac_key, data, hashlib.sha256).digest()
        return data + sig

    def decrypt(self, data):
        """verify HMAC-SHA256 signature and decrypt data with AES-CBC"""
        aes_key, hmac_key = self.keys
        sig = data[-self.SIG_SIZE:]
        data = data[:-self.SIG_SIZE]
        if hmac.new(hmac_key, data, hashlib.sha256).digest() != sig:
            raise AuthenticationError("message authentication failed")
        iv_bytes = data[:self.AES_BLOCK_SIZE]
        data = data[self.AES_BLOCK_SIZE:]
        cypher = AES.new(aes_key, AES.MODE_CBC, iv_bytes)
        data = cypher.decrypt(data)
        return data[:-ord(data[-1])]

    def dumps(self, obj, pickler=pickle):
        """pickle and encrypt a python object"""
        return self.encrypt(self.PICKLE_PAD + pickler.dumps(obj))

    def loads(self, data, pickler=pickle):
        """decrypt and unpickle a python object"""
        data = self.decrypt(data)
        # simple integrity check to verify that we got meaningful data
        assert data.startswith(self.PICKLE_PAD), "unexpected header"
        return pickler.loads(data[len(self.PICKLE_PAD):])


if __name__ == "__main__":
    # usage example
    key = Crypticle.generate_key_string()
    data = {"dict": "full", "of": "secrets"}
    crypt = Crypticle(key)
    safe = crypt.dumps(data)
    assert data == crypt.loads(safe)
    print "encrypted data:"
    print safe.encode("base64")
## end of http://code.activestate.com/recipes/576980/ }}}
