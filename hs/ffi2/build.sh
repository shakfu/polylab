ghc -O2 --make \
      -no-hs-main -optl '-shared' -optc '-DMODULE=Test' \
      -o Test.so Test.hs module_init.c

rm *.hi *.h *.o
rm Test_stub.c
