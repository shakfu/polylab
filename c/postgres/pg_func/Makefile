MODULES = example

PG_CONFIG = pg_config
PGXS = $(shell $(PG_CONFIG) --pgxs)
INCLUDEDIR = $(shell $(PG_CONFIG) --includedir-server)
include $(PGXS)

example.so: example.o
	gcc -shared -o example.so example.o

example.o: example.c
	gcc -o example.o -c example.c $(CFLAGS) -I$(INCLUDEDIR)
