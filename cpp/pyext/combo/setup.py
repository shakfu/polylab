from setuptools import Extension, setup
from Cython.Build import cythonize

setup(ext_modules = cythonize(
    Extension(
        "oscillators",
        sources=[
            "oscillators.pyx",
        ],
        language="c++",
        include_dirs=['./choc/audio'],
        extra_compile_args = ["--std=c++17"],
    ),
    language_level=3,
))
