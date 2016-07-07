from pyparsing import *
# from leofunc import PATTERNS
import re

# from pyparsing import Literal, Group, OneOrMore, restOfLine

parse_data = lambda toks: toks.asDict()

PARSE_FUNCS = {}

class Bunch(dict):
    __getattr__ = dict.__getitem__
    

def define(name, obj, func=None):
    if not func:
        func = PARSE_FUNCS.get(name) 
        # if not func:
        #     func = lambda toks: toks
    else:
        PARSE_FUNCS[name] = func
    return obj.setResultsName(name)#.setParseAction(func)

def defsequence(labels, funcs):
    parsers = Bunch()
    for label, func in zip(labels, funcs):
        parsers[label] = define(label, func)
    return parsers

word = Word(printables)
words = OneOrMore(word)
directive = Literal('@') + word
line   = directive + restOfLine
sentinel  = LineEnd() + Literal('@') + LineEnd()
data      = words + LineEnd()
code      = words + LineEnd()
block = (
        line 
        + Optional(code) 
        + Optional(sentinel) 
        + Optional(data)
        )

labels = ['word', 'words', 'directive', 'line', 'sentinel', 'data', 'code', 'block']
funcs = [word, words, directive, line, sentinel, data, code, block]
parsers = defsequence(labels, funcs)

ts = dict(
    t0 = '@py',
    t1 = '@py(a=1,b=f())   hello there',
    t2 = '@py(f(10),a=10,b=60) hello\n@\n goodbye then\n',
    t3 = '@pyc hello there \n@py 1+1\nwhat is going on',
    t4 = '@py(a=1,b=2,f()) ok',
    t5 = '@py.tool(a=1,s=3) sd',
    t6 = '@py.tool(a=1,s="3") sd',
    t7 = '@py(a=1,s=2)',
)


from pprint import pprint
results = []
for i in sorted(ts):
    results.append((i, parsers.block.parseString(ts[i])))

for i, j in results:
    # print i, j.asList()
    print i, j.asDict()

# line = PATTERNS['line']
# block = PATTERNS['block']
# # zeuux.org
# 
# for i in sorted(ts):
#     m = line.match(ts[i])
#     if m:
#         print 'line:'.upper()
#         print i, ts[i]
#     m = block.match(ts[i])
#     
#     if m:
#         print 'block:'.upper()
#         print i, ts[i]
# 
# 
#         


