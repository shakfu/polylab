CC ?= cc
CFLAGS ?= -std=c99 -O2 -Wall -Wextra -Werror
SANITIZE = -g -fsanitize=address,undefined -fno-sanitize-recover=all
LDLIBS = -lm

.PHONY: all test clean

all: main finlib.so

main: main.c finlib.c finlib.h
	$(CC) $(CFLAGS) main.c finlib.c $(LDLIBS) -o $@

finlib.so: finlib.c finlib.h
	$(CC) $(CFLAGS) -fpic -shared finlib.c $(LDLIBS) -o $@

test_finlib: test_finlib.c finlib.c finlib.h
	$(CC) $(CFLAGS) $(SANITIZE) test_finlib.c finlib.c $(LDLIBS) -o $@

test: test_finlib
	./test_finlib

clean:
	rm -rf main finlib.so finlib.o test_finlib *.dSYM
