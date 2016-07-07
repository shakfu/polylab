echo "build binary"
gcc main.c finlib.c -lm -o main

echo "build shared library"
gcc -c -Wall -Werror -fpic finlib.c
gcc -shared -o finlib.so finlib.o