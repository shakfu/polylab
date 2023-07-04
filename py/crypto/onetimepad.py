"""onetimepad.py

Basic onetimepad: uses all printable characters

Implemented given method from https://en.wikipedia.org/wiki/One-time_pad

"""

import random
import string
import sys



PRINTABLE = string.printable

MODULO = len(PRINTABLE)

def encrypt(text):
    maxint = 1_0000_000
    key = []
    cipher = []
    for i in text:
        try:
            m = PRINTABLE.index(i)
        except ValueError:
            print("error:", repr(i))
            sys.exit()
        k = random.randint(0, maxint)
        key.append(k)
        mk = m + k
        s = mk % MODULO
        cipher.append(s)
        ciphertext = ''.join(PRINTABLE[i] for i in cipher)
    return ciphertext, key


def decrypt(ciphertext, key):
    res = []
    cipher = [PRINTABLE.index(i) for i in ciphertext]
    for i, elem in enumerate(cipher):
        c = elem - key[i]
        r = c % MODULO
        res.append(r)
    return ''.join(PRINTABLE[i] for i in res)


def test_1():
    secret = "HEllo there friend"
    c, k = encrypt(secret)
    assert decrypt(c, k) == secret
    print(f'ciphertext:', repr(c))
    print('key', k)



if __name__ == '__main__':
    test_1()

