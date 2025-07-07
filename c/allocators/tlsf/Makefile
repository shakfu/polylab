.phony: all build test clean

all: test

build:
	@echo

test:
	@gcc -o test_tlsf test_tlsf.c tlsf.c
	@./test_tlsf
	@rm -f ./test_tlsf

clean:
	@echo
