from distutils.core import setup
from Cython.Build import cythonize
from distutils.extension import Extension

sourcefiles = [
    # cython file
    'src/pyx/channel.pyx',
    # c files
    'src/c/chan.c',
    'src/c/blocking_pipe.c',
    'src/c/queue.c',
]

extensions = [
    Extension(
        "channel",
        sourcefiles,
        libraries=['pthread'],
        include_dirs=['include'],
    )
]

setup(
    ext_modules = cythonize(extensions)
)
