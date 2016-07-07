# regular
echo "compile with funcs.c"
gcc -c cfuncs.c

DEBUG=1

echo "llvm"
ghc -fllvm -rtsopts -O3 --make -o Main.llvm Main.hs cfuncs.o

echo "asm"
ghc -fasm -rtsopts -O3 --make -o Main.asm Main.hs cfuncs.o

echo "cleanup"
rm *.o *.hi
strip Main.llvm Main.asm

echo "testing LLVM"
echo "======================================================================="
./Main.llvm +RTS -sstderr

echo "testing Normal"
echo "======================================================================="
./Main.asm +RTS -sstderr
