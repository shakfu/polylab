echo "colors.c"
gcc -o colors colors.c
strip colors

echo "demo.c (pure c)"
gcc -o demo demo.c -lpq
strip demo

echo "demo1.cpp (pure cpp/c)"
g++ -std=c++11 -o demo1 demo1.cpp -lpq
strip demo1

echo "pqxx (c++)"
g++ -o demo2 demo2.cpp -lpqxx -lpq
strip demo2

echo "pqxx (c++) v3"
g++ -std=c++11 -o demo3 demo3.cpp -lpqxx -lpq
strip demo3

echo "pqxx (c++) v4"
g++ -std=c++11 -o demo4 demo4.cpp -lpqxx -lpq
strip demo4
