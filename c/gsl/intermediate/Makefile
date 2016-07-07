# 'make'        build executable file
# 'make clean'  removes all .o and executable files
# 'make check'	checks for memory leaks

# define the executable file 
MAIN = main

INCLUDES = 
LFLAGS = 
LIBS = -lgsl -lgslcblas -lm

# define the C compiler and flags to use
CC = gcc
CFLAGS = -Wall -g -std=c99


# The following part of the makefile is generic:

SOURCES := $(wildcard *.c)
OBJECTS := $(SOURCES:%.c=%.o)

.PHONY: clean

all: $(MAIN)
	@echo $(MAIN) has been compiled

$(MAIN): $(OBJECTS) 
	$(CC) $(CFLAGS) $(INCLUDES) -o $(MAIN) $(OBJECTS) $(LFLAGS) $(LIBS)
	strip $(MAIN)
	rm *.o

$(OBJECTS): %.o : %.c
	@$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@
	@echo "Compiled "$<" successfully!"

static: $(OBJECTS) 
	$(CC) -static $(CFLAGS) $(INCLUDES) -o $(MAIN)_static $(OBJECTS) $(LFLAGS) $(LIBS)
	strip $(MAIN)_static
	rm *.o


clean:
	$(RM) *.o *~ $(MAIN)
	$(RM) test.dat

check:
	valgrind --leak-check=yes ./$(MAIN)
