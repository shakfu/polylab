NAME = guile-3.0
CFLAGS = $(shell pkg-config --cflags $(NAME))
LDFLAGS = $(shell pkg-config --libs $(NAME))

test_guile: clean
	@gcc $(CFLAGS) -o test_guile test_guile.c $(LDFLAGS)

.PHONY: clean bundle

clean:
	@rm -f test_guile

