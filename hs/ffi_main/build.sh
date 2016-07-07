#!

# to make a haskell executable with a main.c entry point
ghc -fglasgow-exts -c main.c foo.hs 
ghc -no-hs-main -o foo main.o foo.o foo_stub.o
rm *.o *.hi
rm *_stub.*