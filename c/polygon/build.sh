clear
#gcc -std=c99 -o shoe.tests shoelace.tests.c -lgsl -lgslcblas -lm
gcc -Wall -std=c99 -o shoe shoelace.c -lm
strip shoe
#./shoe
#valgrind --leak-check=full --show-leak-kinds=all ./shoe

python setup.py build_ext --inplace
rm -rf build polygon.c
strip polygon.so
python test_polygon.py

