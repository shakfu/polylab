# simple.py

'''

simple module docs

usage::
    
    >>> f = func
    >>> f(10)
    20

'''

def func(x):
    return x+10

def _test(complete=False):
    import doctest
    if complete:
        doctest.testmod()
    else:
        exec doctest.script_from_examples(__doc__) in globals()
        

if __name__ == '__main__':
    _test()
