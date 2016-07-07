from distutils.core import setup
from Cython.Build import cythonize
from Cython.Distutils import Extension
from Cython.Distutils import build_ext
import cython_gsl as gsl

extensions = [
    Extension(
        "pyx_gsl", 
        ["pyx_gsl.pyx"],
        libraries=gsl.get_libraries(),
        library_dirs=[gsl.get_library_dir()],
        include_dirs=[gsl.get_cython_include_dir()]
    )
]

setup(
    include_dirs = [gsl.get_include()],
    cmdclass = {'build_ext': build_ext},
    ext_modules = cythonize(extensions)
)

