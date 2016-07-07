#!
ghc -O2 --make \
      -no-hs-main -optl '-shared' -optc '-DMODULE=Financial' \
      -o financial.so financial.hs module_init.c


rm *.hi *.h *.o
rm financial_stub.c

python test.py

