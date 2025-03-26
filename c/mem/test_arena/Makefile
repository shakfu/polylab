# Compiler
CC = gcc

# Compiler flags
CFLAGS = -Wall -Wextra -g

# Source files directory
SRCDIR = src

# Output directory
BUILDDIR = build

# Executable name
TARGET = main

# Source files
SOURCES = $(wildcard $(SRCDIR)/*.c)

# Object files
OBJECTS = $(patsubst $(SRCDIR)/%.c,$(BUILDDIR)/%.o,$(SOURCES))

# Default target
all: $(BUILDDIR)/$(TARGET)

# Build the executable
$(BUILDDIR)/$(TARGET): $(OBJECTS)
	@mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) $^ -o $@

# Compile source files into object files
$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Clean build files
clean:
	rm -rf $(BUILDDIR)

# Run the executable
run: all
	$(BUILDDIR)/$(TARGET)

.PHONY: all clean run
