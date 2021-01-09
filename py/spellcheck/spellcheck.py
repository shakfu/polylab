import sys
from os import system
import random

def question(word):
    system(f'say {word}')
    answer = input('> ')
    if answer == word:
        print('well done')
        return True
    else:
        print(f'you wrote {answer} instead of {word}')
        return False

def test(words):
    random.shuffle(words)
    results = []
    for word in words:
        results.append(question(word))
    print(f'you got {sum(results)} out of {len(words)}')


def main(fpath):
    with open(fpath) as f:
        words = [w.strip() for w in f.readlines()]
    return test(words)


if __name__ == '__main__':
    if len(sys.argv) >= 2:
        main(sys.argv[1])
