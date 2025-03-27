
export MACOSX_DEPLOYMENT_TARGET := 14.7


.PHONY: all build wheel test clean 

all: build

build:
	@python3 setup.py build_ext --inplace
	@rm -rf build
	
wheel:
	@python3 setup.py bdist_wheel

test: build
	@pytest


clean:
	@rm -rf *.so build dist src/*.egg-info .pytest_cache .coverage
