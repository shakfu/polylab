


def trapzd(func, a, b, n=1):
    return 0.5 * (b-a) * (func(a)+func(b))


def func(x): return x+2
    
print trapzd(func, 1.0, 2.0)