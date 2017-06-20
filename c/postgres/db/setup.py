from distutils.core import setup
from Cython.Build import cythonize
from distutils.extension import Extension

sourcefiles = ['db.pyx', 'query.c']

extensions = [
    Extension(
        "db", 
        sourcefiles,
        include_dirs = ['/usr/include/postgresql/'],
        library_dirs = ['/usr/lib/postgresql/9.3/lib'],
        libraries=['m', 'pq'],
    )
]

setup(
    ext_modules = cythonize(extensions)
)
