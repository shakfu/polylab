
.PHONY: build test clean


build:
	@cythonize -3 -i demo.pyx


test:
	python3 test_demo.py


clean:
	@rm -rf *.so demo.c build

