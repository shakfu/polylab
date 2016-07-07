#!
ghc -O2 --make \
      -no-hs-main -optl '-shared' -optc '-DMODULE=Simple' \
      -o simple.so simple.hs module_init.c


rm *.hi *.h *.o
rm simple_stub.c

python test.py

