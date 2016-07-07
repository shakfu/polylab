LIB=app

echo "build library"
ldc -O model/*.d core/*.d -lib -of ./$LIB

echo "build c lib"
gcc -c funcs.c

echo "build exec"
ldc -O -inline -release funcs.o app.d lib$LIB.a

echo "strip lib / exec"
strip app
strip lib$LIB.a

echo "cleanup"
find . -name "*.o" -type f -delete
