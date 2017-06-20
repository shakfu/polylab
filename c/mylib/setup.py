from distutils.core import setup
from Cython.Build import cythonize
from distutils.extension import Extension

sourcefiles = ['pylib.pyx', 'mlib.c']

extensions = [Extension("pylib", sourcefiles)]

setup(
    ext_modules = cythonize(extensions)
)
