clang -dynamiclib calc.c -o libcalc.dylib
c2nim --nep1 calc.h
nim c test_calc.nim

