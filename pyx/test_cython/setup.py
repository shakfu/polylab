#!/usr/bin/python3

from setuptools import Extension, setup

from Cython.Build import cythonize


setup(
    name = "demo",
    version = '0.0.1',
    description = "A demo of cython.",
    python_requires = ">=3.8",
    ext_modules=cythonize([
            Extension(
                name="demo",
                sources=["demo.pyx"],
                # define_macros=define_macros if define_macros else [],
                # include_dirs=INCLUDE_DIRS,
                # libraries=LIBRARIES,
                # library_dirs=LIBRARY_DIRS,
                # extra_objects=EXTRA_OBJECTS,
                # extra_compile_args=EXTRA_COMPILE_ARGS,
                # extra_link_args=EXTRA_LINK_ARGS,
                # language="c++",
            ),
        ],
        compiler_directives = {
            'language_level' : '3',
            'embedsignature': False,     # default: False
            'emit_code_comments': False, # default: True
            'warn.unused': True,         # default: False
        },
    ),
    # package_dir={"": "src"},
)

