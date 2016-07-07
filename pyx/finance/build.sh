#!
rm finance.so
python setup.py build_ext --inplace
rm -rf build finance.c finance.o

