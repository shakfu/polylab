from ctypes import cdll

lib = cdll.LoadLibrary("./libapp.so")
for f in ['m_add', 'm_sub', 'm_div', 'm_mul', 'm_pow']:
    print f, ':', getattr(lib, f)(20, 10)