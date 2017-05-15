#!/usr/bin/env python

from plainsight import plainsight
from bitstring import ConstBitArray
import markovify


class Corpus(object):
    def __init__(self, model_text, context=3):
        self.model_text = model_text
        self.context = max(0, context - 1)
        self.markov_model = markovify.Text(model_text)
        self.steg_model = plainsight.Model(self.context)
        self.steg_model.add_text(model_text, self.context)
        self.cleartext  = None
        self.ciphertext = None

    def sentence(self):
        return self.markov_model.make_sentence()

    def phrase(self, length=140):
        return self.markov_model.make_short_sentence(length)

    def encipher(self, cleartext):
        self.cleartext = ConstBitArray(bytes=cleartext)
        self.ciphertext = plainsight.cipher(self.steg_model, self.context,
                                            self.cleartext, 'encipher')
        return self.ciphertext

    def decipher(self, ciphertext):
        self.ciphertext = ciphertext.split()
        self.cleartext = plainsight.cipher(self.steg_model, self.context,
                                           self.ciphertext, 'decipher').tobytes()
        return self.cleartext

    @classmethod
    def encipher_from_text(cls, model_text, cleartext, context=3):
        c = cls(model_text, context)
        c.encipher(cleartext)
        return c

    @classmethod
    def sentence_from_text(cls, model_text):
        c = cls(model_text)
        return c.sentence()

def demo1():
    from os.path import join
    context = 9
    cleartext = 'I cannot imagine how this feels'
    sources = ['redblack.txt', 'fred.txt', 'meta.txt']
    for source in sources:
        path = join('sources', source)
        with file(path) as f:
            model_text = f.read()
            corpus = Corpus(model_text)
            print 'corpus', source
            print '\tencipher', corpus.encipher(cleartext)
            print '\tsentence', corpus.sentence()
            print

def get_samples():
    from os.path import join
    import random
    samples = []
    sources = ['redblack.txt', 'fred.txt', 'meta.txt']
    for source in sources:
        print source
        path = join('sources', source)
        with file(path) as f:
            model_text = f.read()
            corpus = Corpus(model_text)
            for i in range(10):
                samples.append(corpus.sentence())
    random.shuffle(samples)
    return samples

if __name__ == '__main__':
    #import pprint
    samples = get_samples()
    lines = [s+'\n' for s in samples]
    #target = '/mnt/hgfs/shared/docs/content.txt'
    target = './output/content.txt'
    with file(target, 'w') as f:
        f.writelines(lines)
    #pprint.pprint(samples)

