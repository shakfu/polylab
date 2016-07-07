APP=app
LIB=mlib
PWD=$(pwd)

echo "PWD: $PWD"

echo "compiling $LIB.c"
gcc -O -Wall -fPIC -c $LIB.c

echo "creating $LIB as a static library"
ar -cvq lib$APP.a *.o

echo "creating $LIB as a shared library"
gcc -shared -o lib$APP.so $LIB.o

#gcc mlib.c main.c -o main
echo "compiling main with static library"
gcc -O -o main_static main.c ./lib$APP.a

echo "compiling main with shared library"
gcc -O -L. -Wl,-rpath,. -o main_shared main.c -l$APP

echo "strip library"
strip lib$APP.a lib$APP.so main_shared main_static

echo "build cython extension"
python setup.py build_ext --inplace

echo "cleaning up"
rm *.o
rm -rf build
rm pylib.c

echo "testing"
python test_cffi.py
python test_ctypes.py
python test_cython.py

