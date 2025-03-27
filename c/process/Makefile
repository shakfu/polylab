CFLAGS = -Wall -Wextra -g

.PHONY: all clean

all: test_exec test_fork


test_exec: test_exec.c
	@$(CC) $(CFLAGS) $^ -o $@

test_fork: test_fork.c
	@$(CC) $(CFLAGS) $^ -o $@

clean:
	@rm -rf test_exec test_fork *.dSYM
