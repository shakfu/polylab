CC=gcc
CXX=g++
RM=rm -f
CPPFLAGS=-g
LDFLAGS=-g -lm
LDLIBS=

SRCS=$(wildcard *.cpp)
OBJS=$(subst .cpp,.o,$(SRCS))

all: main

main: $(OBJS)
	$(CXX) $(LDFLAGS) -o main $(OBJS) $(LDLIBS)

main.o: main.cpp vehicle.h

vehicle.o: vehicle.h vehicle.cpp 

clean:
	$(RM) $(OBJS)

distclean: clean
	$(RM) main
