# 'make'        build executable file
# 'make clean'  removes all .o and executable files
# 'make check'	checks for memory leaks

# define the executable file
MAIN = main

INCLUDES =
LFLAGS =
LIBS =

# directories
SRCDIR = src
OBJDIR = obj

# define the C compiler and flags to use
CXX = g++
CXXFLAGS = -Wall -g -std=c++11

CC = gcc
CFLAGS = -Wall -g -std=c99



# The following part of the makefile is generic:

SOURCES := $(wildcard $(SRCDIR)/*.cpp)
OBJECTS := $(SOURCES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)

CSOURCES := $(wildcard $(SRCDIR)/*.c)
COBJECTS := $(CSOURCES:$(SRCDIR)/%.c=$(OBJDIR)/%.o)

.PHONY: clean

all: $(MAIN)
	@echo $(MAIN) has been compiled

$(MAIN): $(COBJECTS) | $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $(MAIN) $(COBJECTS) $(OBJECTS) $(LFLAGS) $(LIBS)
	strip $(MAIN)

$(COBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.c
	@$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@
	@echo "C Compiled "$<" successfully!"

$(OBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	@$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@
	@echo "CPP Compiled "$<" successfully!"

clean:
	$(RM) $(OBJDIR)/*.o *~ $(MAIN)

check:
	valgrind --leak-check=yes ./$(MAIN)
