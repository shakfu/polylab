
MODULE:="my_ext.cpython-311-darwin.so"
CYMODULE="oscillators.cpython-311-darwin.so"


.PHONY: pybind nanobind cython clean

all: pybind nanobind cython

main:
	@g++ --std=c++17 -I./choc/audio -o main main.cpp

pybind:
	@rm -rf build && mkdir -p build && cd build && cmake .. && make
	@mkdir -p ext/pybind && mv build/$(MODULE) ext/pybind/

nanobind:
	@rm -rf build && mkdir -p build && cd build && cmake .. -DENABLE_NANOBIND=ON && make
	@mkdir -p ext/nano && mv build/$(MODULE) ext/nano/

cython:
	@rm -rf build && python3 setup.py build_ext --inplace
	@mkdir -p ext/cython && mv $(CYMODULE) ext/cython/


clean:
	@rm -rf build ext *.so
