from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext

setup(
    cmdclass = {'build_ext': build_ext},
    ext_modules = [
        Extension(
            "setlib",         # name of the extension 
            ["setlib.pyx"],   # filename of cython file
            include_dirs=['include/libcalg-1.0'],
            #library_dirs=['lib'],
            libraries=['/Users/sa/Code/sandbox/cython/setlib/lib/libcalg']
        )
    ]
)


