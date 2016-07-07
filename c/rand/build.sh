
gcc -std=c99 monte.c -o test
python setup.py build_ext --inplace
strip montelib.so

