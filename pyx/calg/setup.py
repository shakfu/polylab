
from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
from os.path import join, dirname

root = dirname(__file__)

INCLUDES = [join(root, "include")]
LIBRARIES = [join(root, "lib")]


setup(
        cmdclass = {'build_ext': build_ext},
        ext_modules = [
              Extension("queue", ["queue.pyx", "src/queue.c"],
                     include_dirs=INCLUDES
              ),
       ]

)
