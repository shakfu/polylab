
# echo "building c test"
# gcc -I /usr/include/postgresql/ \
#     -L /usr/lib/postgresql/9.3/lib \
#     test_pq.c -lpq -o ./test_pq


echo "building c query"
gcc -I /usr/include/postgresql/ \
    -L /usr/lib/postgresql/9.3/lib \
    query.c -lpq -o ./query

#valgrind --leak-check=full --show-leak-kinds=all ./query
strip query

# echo "building cpp test"
# g++ test_pqxx.cpp \
#     -I/usr/include/postgresql/ \
#     -L /usr/lib/postgresql/9.3/lib -lpqxx -lpq -o ./test_pqxx


echo "building python extension"
python setup.py build_ext --inplace
rm -rf build db.c
strip db.so
python test_db.py
