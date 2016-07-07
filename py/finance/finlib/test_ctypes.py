from ctypes import *


CLIBS = {
    'finlib' : {
        #name   restype     argtypes
        'irr' : (c_float,   (POINTER(c_float), c_int)),
        #'npv' : (c_float,   (POINTER(c_float), c_int)),
    },
}


class cLib(object):
    def __init__(self, name):
        self.name = name
        self.headers = CLIBS[name]
        self.funcs = {}
        
        self.clib = cdll.LoadLibrary('./%s.so' % name)
        self.cfuncs = self.setup()

    def setup(self):
        for name in self.headers:
            restype, argtypes = self.headers[name]
            func = getattr(self.clib, name)
            self.funcs[name] = self.specify(func, restype, argtypes)

    def __getattr__(self, name):
        return self.funcs[name]
    

    def specify(self, func, restype, argtypes):
        func.restype = restype
        func.argtypes = argtypes
        return func

def to_array(lst, ctype=c_float):
    n = len(lst)
    atype = ctype * n
    return (atype(*lst), n)


finlib = cLib('finlib')

def irr(cashflows):
    cflows, n = to_array(cashflows)
    return finlib.irr(cflows, n)




print irr([-100.0, 60.0, 60.0, 60.0])



