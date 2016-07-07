
#clang -o test -lsfl test_sfl.c

gcc -I/usr/local/include -O0 -g3 -Wall -c test_sfl.c

gcc -L/usr/local/lib -o test  ./test_sfl.o   -lsfl


