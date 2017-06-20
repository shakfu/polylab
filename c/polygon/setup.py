from distutils.core import setup
from Cython.Build import cythonize
from distutils.extension import Extension

sourcefiles = ['polygon.pyx', 'shoelace.c']

extensions = [
    Extension(
        "polygon", 
        sourcefiles,
        libraries=['m'],
    )
]

setup(
    ext_modules = cythonize(extensions)
)
