#!
ghc --make Foo
ghc main.c
ghc -no-hs-main Foo.o Foo_stub.o main.o -o main
rm *.hi *.o *.h
rm Foo_stub.c



