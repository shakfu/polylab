# requires: sudo apt-get install libjudy-dev 

gcc -I. -L. -o test0 test_judy.c  -lJudy
gcc -I. -L. -o test1 test_judy1.c -lJudy
gcc -I. -L. -o test2 test_judy2.c -lJudy

gcc -o simple simple.c
strip simple
# strip test0 test1 test2
#./test0
#./test1
#./test2

./simple
#valgrind --leak-check=full --show-leak-kinds=all ./simple

