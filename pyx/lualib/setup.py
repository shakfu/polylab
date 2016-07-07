import os
from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext

path = lambda *p: os.path.join(os.path.dirname(__file__), *p)

LOCAL = True

LUALIBS = [path('lib', 'lua5.1')] if LOCAL else ['/usr/lib/lua5.1']
INCLUDES = [path('include','lua5.1')] if LOCAL else ['/usr/include/lua5.1']

setup(
    cmdclass = {'build_ext': build_ext},
    ext_modules = [
        Extension(
            "lualib",           # name of the extension
            ["src/lualib.pyx"], # filename of cython file
            library_dirs=LUALIBS,
            include_dirs=INCLUDES,
            libraries=['lua5.1'],
            #extra_compile_args=[],
            #extra_link_args=['-static']
        )
    ]
)


