
HOMEBREW := $(shell brew --prefix)
LIB := $(HOMEBREW)/lib
INCLUDE := $(HOMEBREW)/include


.PHONY: all build release format clean

all: build

build:
	-@swift build -Xlinker -L$(LIB) -Xcc -I$(INCLUDE)


release:
	-@swift build -c release -Xlinker -L$(LIB) -Xcc -I$(INCLUDE)


format:
	-@swift-format --configuration .swiftformatrc --recursive --in-place ./src


clean:
	-@rm -rf .build
