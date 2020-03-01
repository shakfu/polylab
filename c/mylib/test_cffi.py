from cffi import FFI
ffi = FFI()

with open('mlib.h', 'r') as f:
    header = f.read()

ffi.cdef(header)
C = ffi.dlopen('./libapp.so')
for f in ['m_add', 'm_sub', 'm_div', 'm_mul', 'm_pow']:
    print(f, ':', getattr(C, f)(20, 10))

