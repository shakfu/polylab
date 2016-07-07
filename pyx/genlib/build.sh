#!
rm fastlib.so
python setup.py build_ext --inplace
rm -rf build fastlib.c funclib.o
