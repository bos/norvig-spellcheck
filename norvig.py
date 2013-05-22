#!/usr/bin/env python

import time

def timed(desc, act):
    start = time.time()
    ret = act()
    end = time.time()
    print '%s: %.5fs' % (desc, end - start)
    return ret

import re, collections

def words(text): return re.findall('[a-z]+', text.lower())

def train(features):
    model = collections.defaultdict(lambda: 1)
    for f in features:
        model[f] += 1
    return model

NWORDS = timed('train', lambda: train(words(file('big.txt').read())))

alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits1(word):
   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in splits if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
   inserts    = [a + c + b     for a, b in splits for c in alphabet]
   return set(deletes + transposes + replaces + inserts)

def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

def known(words): return set(w for w in words if w in NWORDS)

def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)

def main():
    import sys
    for arg in sys.argv[1:]:
        if arg.startswith('@'):
            allwords = words(open(arg[1:]).read())
            for w in allwords:
                corr = correct(w)
                if w != corr:
                    print w, corr
        else:
            corr = correct(arg)
            if arg != corr:
                print arg, corr

timed('correct', main)
