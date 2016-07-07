# echo "gv test"
#gcc -std=c99 -o test_gv test_gv.c -I/usr/include/graphviz -L/usr/lib/graphviz -lgvc -lcgraph -lcdt
#valgrind --leak-check=full --show-leak-kinds=all ./test_gv
# ./test_gv -Kdot -Tpng > ok.png
# mv ok.png ~/shared/ok.png

# echo "c version"
gcc -Wall -std=c99 -o viterbi viterbi.c
valgrind --leak-check=full --show-leak-kinds=all ./viterbi
strip viterbi
./viterbi

#echo "test glib "
# requires sudo apt-get install libglib2.0-dev
#gcc -Wall -std=c99 -o test_glib test_glib.c `pkg-config --cflags --libs glib-2.0`
#valgrind --leak-check=full --show-leak-kinds=all ./test_glib

# echo "plain cpp"
# g++ -o viterbi_stl viterbi.stl.cpp
# strip viterbi_stl

# echo "shedskin version"
# sag build -sc viterbi.py
# mv viterbi viterbi_ss

#echo "cython version"
#sag build -ec viterbi.pyx
#mv viterbi viterbi_cy
