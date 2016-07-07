#!

# to make a haskell executable with a main.c entry point
ghc -fglasgow-exts -c foo.hs
ghc -c foolib.c
ghc -no-hs-main -o foolib foolib.o foo.o foo_stub.o
rm *.o *.hi
rm *_stub.*