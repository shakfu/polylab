python setup.py build_ext --inplace
rm -rf build pyx_gsl.c
strip pyx_gsl.so
python test_pyx_gsl.py

