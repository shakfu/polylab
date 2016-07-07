
echo "compile hello"
csc hello.scm

echo "compile static: csc -static hello.scm"
csc -o hello_static -static hello.scm
strip hello_static

echo "embed compilation"
gcc -c func.c
csc -o embed func.o embed.scm
rm *.o

