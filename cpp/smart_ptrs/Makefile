
CPPFLAGS=-std=c++20 -Wall
CXX=g++

SOURCES := ${wildcard *.cpp}
EXES 	:= $(SOURCES:%.cpp=%)

all: $(EXES)

%: %.cpp
	$(CXX) $(CPPFLAGS) -o $@ $<


.PHONY: clean

clean:
	@rm -f $(EXES)

