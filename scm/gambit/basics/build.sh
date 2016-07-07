echo "scheme to c"
gsc -link m2 m3

echo "objectifyng"
gsc -obj m1.c m2.c m3.c m3_.c

echo "linking"
gcc -o m.app -I/usr/local/Gambit-C/include \
    -L/usr/local/Gambit-C/lib \
    -D___SINGLE_HOST \
    -O \
    m1.o m2.o m3.o m3_.o -lgambc -lm -ldl -lutil

echo "cleanup"
rm *.o
rm m2.c m3.c m3_.c

echo "shared library"
gsc -link -o mylib.c m2
gsc -obj -cc-options "-D___SHARED" m1.c m2.c mylib.c
gcc -shared m1.o m2.o mylib.o -o mylib.so

gsc -link -l mylib m3
gsc -obj m3.c m3_.c
gcc -o m_shared \
    -I/usr/local/Gambit-C/include \
    -L/usr/local/Gambit-C/lib \
    m3.o m3_.o mylib.so -lgambc -lm -ldl -lutil

echo "cleanup"
rm *.o
rm m2.c m3.c m3_.c mylib.c

LD_LIBRARY_PATH=.:/usr/local/lib ./m_shared


strip m_shared m.app