# https://lemon.cs.elte.hu/trac/lemon

g++ -o lp_demo lp_demo.cc -lemon -lglpk
strip lp_demo

g++ -o test_graph test_graph.cc -lemon
strip test_graph



