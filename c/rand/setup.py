from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext

setup(
    name='Monte Carlo simulation',
    ext_modules=[Extension('montelib',
        ['montelib.pyx', 'monte.c'])
    ],
    cmdclass={'build_ext': build_ext},
)
