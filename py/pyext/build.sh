python setup.py build
cp ./build/lib.linux-x86_64-2.7/hello.so .
cp ./build/lib.linux-x86_64-2.7/fib.so .
strip hello.so fib.so
rm -rf build
python test.py
