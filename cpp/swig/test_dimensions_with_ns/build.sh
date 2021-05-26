echo "generating swig interface code..."
swig -c++ -python eg.i

echo "compiling swig extension..."
g++ -O2 -fPIC -c eg.cpp eg_wrap.cxx `python3-config --includes`
g++ `python3-config --ldflags` -lpython3.9 -dynamiclib -flat_namespace eg.o eg_wrap.o -o _eg.so

echo "cleaning up..."
rm *.o
