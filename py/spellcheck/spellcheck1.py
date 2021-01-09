#!/usr/bin/env python

import sys
from os import system
import random

words = ['refer', 'referred', 'referral', 'referral', 'reference', 'reference', 
         'transfer', 'tranferred', 'tranferring', 'transferral', 'transference', 
          'prefer', 'preferred', 'confer', 'conference']



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


test(words)



