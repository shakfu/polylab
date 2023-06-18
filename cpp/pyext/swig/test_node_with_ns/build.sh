PYVER=`python3 -c "import sysconfig; print(sysconfig.get_python_version())"`

swig -c++ -python eg.i

g++ -O2 -fPIC -c eg.cpp eg_wrap.cxx `python3-config --includes`
g++ `python3-config --ldflags` -lpython${PYVER} -dynamiclib -flat_namespace eg.o eg_wrap.o -o _eg.so
rm *.o
