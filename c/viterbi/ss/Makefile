# project variables
SHEDSKIN_LIBDIR=$(HOME)/projects/shedskin/shedskin/lib
HOMEBREW_PREFIX=/opt/homebrew
HOMEBREW_INCLUDE=$(HOMEBREW_PREFIX)/include
HOMEBREW_LIB=$(HOMEBREW_PREFIX)/lib
STATIC_GC=$(HOMEBREW_LIB)/libgc.a
STATIC_GCCPP=$(HOMEBREW_LIB)/libgccpp.a
STATIC_PCRE=$(HOMEBREW_LIB)/libpcre.a
STATIC_LIBS=$(STATIC_GC) $(STATIC_GCCPP) $(STATIC_PCRE)
CPPFILES=$(GENERATED_CPPFILES) $(BUILTIN_CPPFILES)
HPPFILES=$(GENERATED_HPPFILES) $(BUILTIN_HPPFILES)

INCLUDEDIRS=-I$(CURDIR) -I$(SHEDSKIN_LIBDIR) -I$(HOMEBREW_INCLUDE)
LINKDIRS=-L/usr/lib -L/usr/local/lib -L$(HOMEBREW_LIB)

CXX=g++
CXXFLAGS+=-O2 -std=c++17 -Wno-deprecated $(CPPFLAGS) $(INCLUDEDIRS)
LDFLAGS+=-fno-strict-overflow -Wsign-compare -Wunreachable-code -fno-common -dynamic -undefined dynamic_lookup $(LINKDIRS)
LDLIBS=-lgc -lgctba -lpcre

BUILTIN_CPPFILES=$(SHEDSKIN_LIBDIR)/builtin.cpp
BUILTIN_HPPFILES=$(SHEDSKIN_LIBDIR)/builtin.hpp
GENERATED_CPPFILES=$(CURDIR)/viterbi.cpp
GENERATED_HPPFILES=$(CURDIR)/viterbi.hpp

.PHONY: all clean static

all: viterbi

static: $(CPPFILES) $(HPPFILES)
	$(CXX) $(CXXFLAGS) $(CPPFILES) $(STATIC_LIBS) $(LDFLAGS) -o viterbi

viterbi: $(CPPFILES) $(HPPFILES)
	$(CXX)  $(CXXFLAGS) $(CPPFILES) $(LDLIBS) $(LDFLAGS) -o viterbi

viterbi_debug: $(CPPFILES) $(HPPFILES)
	$(CXX) -g -ggdb $(CXXFLAGS) $(CPPFILES) $(LDLIBS) $(LDFLAGS) -o viterbi_debug

viterbi_prof: $(CPPFILES) $(HPPFILES)
	$(CXX) -pg -ggdb $(CXXFLAGS) $(CPPFILES) $(LDLIBS) $(LDFLAGS) -o viterbi_prof

clean:
	@rm -rf viterbi viterbi_prof viterbi_debug *.dSYM

reset: clean
	@rm -f $(GENERATED_CPPFILES) $(GENERATED_HPPFILES)

