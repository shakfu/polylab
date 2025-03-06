
.PHONY: all build test clean

all: build

build:
	@/opt/homebrew/opt/llvm/bin/clang -g -fsanitize=address -o test1 test1.c

test:
	@ASAN_OPTIONS=detect_leaks=1 ./test1

clean:
	@rm -rf *.dSYM
