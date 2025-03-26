BREW_LLVM=$(shell brew --prefix llvm)

.PHONY: all build test clean

all: build

build:
	@$(BREW_LLVM)/bin/clang -g -fsanitize=address -o test1 test1.c

test:
	@ASAN_OPTIONS=detect_leaks=1 ./test1

clean:
	@rm -rf *.dSYM
